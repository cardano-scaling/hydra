{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano where

import Hydra.Prelude hiding (id)

import Cardano.Api
import Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.Slot as Ledger
import qualified Cardano.Slotting.EpochInfo as Slotting
import qualified Cardano.Slotting.Time as Slotting
import qualified Control.State.Transition as Ledger
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (maybeToStrictMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Orphans ()

-- TODO(SN): Pre-validate transactions to get less confusing errors on
-- transactions which are not expected to working on a layer-2
cardanoLedger :: Ledger (Tx AlonzoEra)
cardanoLedger =
  Ledger
    { applyTransactions = applyAll
    , initUtxo = mempty
    }
 where
  -- NOTE(SN): See full note on 'applyTx' why we only have a single transaction
  -- application here.
  applyAll (Utxo utxo) = \case
    [] -> Right (Utxo utxo)
    (tx : txs) -> do
      utxo' <- applyTx ledgerEnv utxo (toLedgerTx tx)
      applyAll (Utxo utxo') txs

  -- NOTE(SN): This is will fail on any transaction requiring the 'DPState' to be
  -- in a certain state as we do throw away the resulting 'DPState' and only take
  -- the ledger's 'UTxO' forward.
  --
  -- We came to this signature of only applying a single transaction because we
  -- got confused why a sequence of transactions worked but sequentially applying
  -- single transactions didn't. This was because of this not-keeping the'DPState'
  -- as described above.
  applyTx ::
    ( Ledger.ApplyTx era
    , Default (Ledger.State (Ledger.EraRule "PPUP" era))
    ) =>
    Ledger.LedgerEnv era ->
    Ledger.UTxO era ->
    Ledger.Tx era ->
    Either ValidationError (Ledger.UTxO era)
  applyTx env utxo tx =
    case Ledger.applyTxsTransition globals env (pure tx) memPoolState of
      Left err -> Left $ toValidationError err
      Right (ls, _ds) -> Right $ Ledger._utxo ls
   where
    toValidationError = ValidationError . show
    memPoolState = (def{Ledger._utxo = utxo}, def)

--
-- Type conversions & plumbing
--

--
-- Utxo
--

-- | Newtype mostly required to provide 'Ledger.UTXO' with a 'Monoid' instance.
newtype Utxo = Utxo
  { unUtxo :: Ledger.UTxO (ShelleyLedgerEra AlonzoEra)
  }
  deriving newtype (Eq, Show, ToCBOR, FromCBOR)

instance Semigroup Utxo where
  Utxo (Ledger.UTxO a) <> Utxo (Ledger.UTxO b) =
    Utxo (Ledger.UTxO (a <> b))

instance Monoid Utxo where
  mempty =
    Utxo (Ledger.UTxO mempty)

instance ToJSON Utxo where
  toJSON =
    toJSON . Ledger.unUTxO . unUtxo

instance FromJSON Utxo where
  parseJSON =
    fmap (Utxo . Ledger.UTxO) . parseJSON

--
-- Tx
--

instance IsTx (Tx AlonzoEra) where
  type TxIdType (Tx AlonzoEra) = TxId
  type UtxoType (Tx AlonzoEra) = Utxo
  type ValueType (Tx AlonzoEra) = Value

  txId = getTxId . getTxBody
  balance (Utxo (Ledger.UTxO u)) =
    let aggregate (Ledger.Alonzo.TxOut _ value _) = (<>) (fromMaryValue value)
     in Map.foldr aggregate mempty u

instance ToJSON (Tx AlonzoEra) where
  toJSON = toJSON . toLedgerTx

instance FromJSON (Tx AlonzoEra) where
  parseJSON = fmap fromLedgerTx . parseJSON

-- | Convert an existing @cardano-api@'s 'Tx' to a @cardano-ledger-specs@ 'Tx'
toLedgerTx :: Tx AlonzoEra -> Ledger.Tx (ShelleyLedgerEra AlonzoEra)
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Alonzo.Redeemers mempty)
     in Ledger.Alonzo.ValidatedTx
          { Ledger.Alonzo.body =
              body
          , Ledger.Alonzo.isValid =
              toLedgerScriptValidity validity
          , Ledger.Alonzo.auxiliaryData =
              maybeToStrictMaybe auxData
          , Ledger.Alonzo.wits =
              Ledger.Alonzo.TxWitness
                { Ledger.Alonzo.txwitsVKey =
                    fromList [w | ShelleyKeyWitness _ w <- vkWits]
                , Ledger.Alonzo.txwitsBoot =
                    fromList [w | ShelleyBootstrapWitness _ w <- vkWits]
                , Ledger.Alonzo.txscripts =
                    fromList [(Ledger.hashScript @(ShelleyLedgerEra AlonzoEra) s, s) | s <- scripts]
                , Ledger.Alonzo.txdats =
                    datums
                , Ledger.Alonzo.txrdmrs =
                    redeemers
                }
          }
 where
  toLedgerScriptValidity :: TxScriptValidity AlonzoEra -> Ledger.Alonzo.IsValid
  toLedgerScriptValidity =
    Ledger.Alonzo.IsValid . \case
      TxScriptValidityNone -> True
      TxScriptValidity _ ScriptValid -> True
      TxScriptValidity _ ScriptInvalid -> False

-- | Convert an existing @cardano-ledger-specs@'s 'Tx' into a @cardano-api@'s 'Tx'
fromLedgerTx :: Ledger.Tx (ShelleyLedgerEra AlonzoEra) -> Tx AlonzoEra
fromLedgerTx = error "fromLedgerTx: TODO"

--
-- Temporary / Quick-n-dirty
--

-- FIXME: Do not hard-code this, make it configurable / inferred from the
-- genesis configuration.
ledgerEnv :: Ledger.LedgerEnv (ShelleyLedgerEra AlonzoEra)
ledgerEnv =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 1
    , Ledger.ledgerIx = 0
    , Ledger.ledgerPp = def{Ledger.Alonzo._maxTxSize = 1024 * 1024}
    , Ledger.ledgerAccount = error "ledgerEnv: ledgersAccount undefined"
    }

-- FIXME: Do not hard-code this, make it configurable / inferred from the
-- genesis configuration.
--
-- From: shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Utils.hs
globals :: Ledger.Globals
globals =
  Ledger.Globals
    { Ledger.epochInfoWithErr = Slotting.fixedEpochInfo (Ledger.EpochSize 100) (Slotting.mkSlotLength 1)
    , Ledger.slotsPerKESPeriod = 20
    , Ledger.stabilityWindow = 33
    , Ledger.randomnessStabilisationWindow = 33
    , Ledger.securityParameter = 10
    , Ledger.maxKESEvo = 10
    , Ledger.quorum = 5
    , Ledger.maxMajorPV = 1000
    , Ledger.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , Ledger.activeSlotCoeff = Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Ledger.networkId = Ledger.Testnet
    , Ledger.systemStart = Slotting.SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
