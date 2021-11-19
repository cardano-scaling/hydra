{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano where

import Hydra.Prelude hiding (id)

import Cardano.Api
import Cardano.Api.Shelley
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Ledger (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger (WitVKey (..))
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.Slot as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Slotting.EpochInfo as Slotting
import qualified Cardano.Slotting.Time as Slotting
import qualified Control.State.Transition as Ledger
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Orphans ()

type LedgerEra = Ledger.Alonzo.AlonzoEra Ledger.StandardCrypto
type Era = AlonzoEra

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
      utxo' <- coerce <$> applyTx ledgerEnv (coerce utxo) (toLedgerTx tx)
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

signWith ::
  forall era.
  (IsShelleyBasedEra era) =>
  TxId ->
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  KeyWitness era
signWith (TxId h) (PaymentVerificationKey vk, PaymentSigningKey sk) =
  ShelleyKeyWitness (shelleyBasedEra @era) $
    Ledger.WitVKey
      (Ledger.asWitness vk)
      (Ledger.signedDSIGN @Ledger.StandardCrypto sk h)

-- FIXME: The network discriminant should really be a parameter here.
mkVkAddress ::
  VerificationKey PaymentKey ->
  Address ShelleyAddr
mkVkAddress vk =
  makeShelleyAddress
    (Testnet $ NetworkMagic 42)
    (PaymentCredentialByKey $ verificationKeyHash vk)
    NoStakeAddress

--
-- Type conversions & plumbing
--

--
-- Utxo
--

type Utxo = Utxo' LedgerEra (Ledger.Alonzo.TxOut LedgerEra)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO'and provide 'Monoid' and 'Foldable' instances to make utxo
-- manipulation bareable.
newtype Utxo' era out = Utxo
  { unUtxo :: Map (Ledger.TxIn (Ledger.Crypto era)) out
  }
  deriving newtype (Eq, Show)

instance ToCBOR Utxo where
  toCBOR = toCBOR . coerce @_ @(Ledger.UTxO LedgerEra)
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR Utxo where
  fromCBOR = coerce @(Ledger.UTxO LedgerEra) <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

instance Foldable (Utxo' era) where
  foldMap fn = foldMap fn . unUtxo
  foldr fn zero = foldr fn zero . unUtxo

instance Semigroup Utxo where
  Utxo uL <> Utxo uR = Utxo (uL <> uR)

instance Monoid Utxo where
  mempty = Utxo mempty

instance ToJSON Utxo where
  toJSON = toJSON . unUtxo

instance FromJSON Utxo where
  parseJSON = fmap Utxo . parseJSON

prettyUtxo :: (TxIn, TxOut ctx era) -> Text
prettyUtxo (k, TxOut _ (txOutValueToValue -> v) _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> prettyValue v

-- FIXME: This function is wrong! It's mapping a transaction's own inputs to its
-- own outputs. Whoops. It's currently used in Hydra.Chain.Direct.Tx where
-- the calling code only look at the outputs of the transactions and also in the
-- generators of the local-cluster (Whoops bis).
utxoFromTx :: Tx AlonzoEra -> Utxo
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ Ledger.Alonzo.outputs' ledgerBody
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [0 .. fromIntegral (length txOuts)]
        ]
   in Utxo $ Map.fromList $ zip txIns txOuts

--
-- Tx
--

instance IsTx (Tx AlonzoEra) where
  type TxIdType (Tx AlonzoEra) = TxId
  type UtxoType (Tx AlonzoEra) = Utxo
  type ValueType (Tx AlonzoEra) = Value

  txId = getTxId . getTxBody
  balance (Utxo u) =
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
fromLedgerTx (Ledger.Alonzo.ValidatedTx body wits isValid auxData) =
  Tx
    (ShelleyTxBody era body scripts scriptsData (strictMaybeToMaybe auxData) validity)
    vkWits
 where
  era =
    ShelleyBasedEraAlonzo
  scripts =
    Map.elems $ Ledger.Alonzo.txscripts' wits
  scriptsData
    | otherwise =
      TxBodyScriptData
        ScriptDataInAlonzoEra
        (Ledger.Alonzo.txdats' wits)
        (Ledger.Alonzo.txrdmrs' wits)
  validity = case isValid of
    Ledger.Alonzo.IsValid True ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptValid
    Ledger.Alonzo.IsValid False ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptInvalid
  vkWits =
    Set.foldr ((:) . ShelleyKeyWitness era) [] (Ledger.Alonzo.txwitsVKey' wits)
      ++ Set.foldr ((:) . ShelleyBootstrapWitness era) [] (Ledger.Alonzo.txwitsBoot' wits)

--
-- TxId
--

toLedgerTxId :: TxId -> Ledger.TxId Ledger.StandardCrypto
toLedgerTxId (TxId h) =
  Ledger.TxId (Ledger.unsafeMakeSafeHash (Crypto.castHash h))

--
-- Formatting
--

-- TODO: Maybe consider using 'renderValue' from cardano-api instead?
prettyValue :: Value -> Text
prettyValue value =
  let Lovelace lovelace = fromMaybe 0 (valueToLovelace value)
      (ada, decimal) = lovelace `quotRem` 1000000
      n = length (valueToList value)
   in unwords $
        [ show ada <> "." <> padLeft '0' 6 (show decimal)
        , "₳"
        ]
          ++ if n == 0
            then mempty
            else ["and", show n, "asset(s)"]

-- | Pad a text-string to left with the given character until it reaches the given
-- length.
--
-- NOTE: Truncate the string if longer than the given length.
-- TODO: Move into a separate module.
padLeft :: Char -> Int -> Text -> Text
padLeft c n str = T.takeEnd n (T.replicate n (T.singleton c) <> str)

--
-- Generators
--

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
