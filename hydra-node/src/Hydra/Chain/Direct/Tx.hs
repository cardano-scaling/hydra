{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Address (Addr (Addr))
import Cardano.Ledger.Alonzo (AlonzoEra, Script)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (PlutusScript), Tag (Spend))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (RdmrPtr), Redeemers (..), TxDats (..), TxWitness (..), unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (hashScript)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (inject)
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (stateTVar)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (..), OnChainTx (OnInitTx))
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import Hydra.Party (anonymousParty, vkey)
import Plutus.V1.Ledger.Api (PubKeyHash (..), fromData, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Shelley.Spec.Ledger.API (
  Coin (..),
  Credential (ScriptHashObj),
  Network (Testnet),
  ScriptHash,
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxIn,
  Wdrl (Wdrl),
 )

-- TODO(SN): parameterize
network :: Network
network = Testnet

type Era = AlonzoEra StandardCrypto

-- * Post Hydra Head transactions

-- | Maintains information needed to construct on-chain transactions
-- depending on the current state of the head.
data OnChainHeadState
  = Closed
  | Initial
      { -- TODO add the output containing the SM token
        -- TODO initials should be a list of inputs/PubKeyHas
        -- TODO add commits
        initials :: (TxIn StandardCrypto, PubKeyHash)
      }
  deriving (Eq, Show, Generic)

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx :: HeadParameters -> TxIn StandardCrypto -> ValidatedTx Era
initTx HeadParameters{contestationPeriod, parties} txIn =
  mkUnsignedTx body dats mempty mempty
 where
  body =
    TxBody
      { inputs = Set.singleton txIn
      , collateral = mempty
      , -- TODO(SN): of course this is missing the PT outputs
        outputs = StrictSeq.singleton headOut
      , txcerts = mempty
      , txwdrls = Wdrl mempty
      , txfee = Coin 0
      , txvldt = ValidityInterval SNothing SNothing
      , txUpdates = SNothing
      , reqSignerHashes = mempty
      , mint = mempty
      , scriptIntegrityHash = SNothing
      , adHash = SNothing
      , txnetworkid = SNothing
      }

  dats = TxDats $ Map.singleton headDatumHash headDatum

  headOut = TxOut headAddress headValue (SJust headDatumHash)

  -- TODO(SN): The main Hydra Head script address. Will be parameterized by the
  -- thread token eventually. For now, this is just the initial script as well,
  -- although this could be really some arbitrary address. After all it is also
  -- later quite arbitrary/different per Head.
  headAddress = scriptAddr $ PlutusScript "foo"

  -- REVIEW(SN): how much to store here / minUtxoValue / depending on assets?
  headValue = inject (Coin 0)

  headDatumHash = hashData @Era headDatum

  headDatum =
    Data . toData $
      Head.Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

-- | Create transaction which aborts by spending one input. This is currently
-- only possible if this is governed by the initial script and only for a single
-- input. Of course, the Head protocol specifies we need to spend ALL the Utxo
-- containing PTs.
abortTx ::
  -- | The input to be consumed by the abort transaction, locked by the validator
  -- script
  TxIn StandardCrypto ->
  -- | The datum to provide to the validator script
  PubKeyHash ->
  ValidatedTx Era
abortTx txIn pkh =
  mkUnsignedTx body dats redeemers scripts
 where
  body =
    TxBody
      { inputs = Set.singleton txIn
      , collateral = mempty
      , outputs = mempty
      , txcerts = mempty
      , txwdrls = Wdrl mempty
      , txfee = Coin 0
      , txvldt = ValidityInterval SNothing SNothing
      , txUpdates = SNothing
      , reqSignerHashes = mempty
      , mint = mempty
      , scriptIntegrityHash = SNothing
      , adHash = SNothing
      , txnetworkid = SNothing
      }

  -- TODO(SN): dummy exUnits, balancing overrides them?
  redeemers = Map.singleton (RdmrPtr Spend 0) (redeemerData, ExUnits 0 0)

  -- TODO(SN): This should be 'Abort' or so
  redeemerData = Data $ toData ()

  scripts = Map.singleton initialScriptHash initialScript

  initialScriptHash = hashScript @Era initialScript

  initialScript = plutusScript Initial.validatorScript

  dats = TxDats $ Map.singleton initialDatumHash initialDatum

  initialDatumHash = hashData @Era initialDatum

  initialDatum = Data $ toData pkh

--

-- * Observe Hydra Head transactions

-- | Update observable on-chain head state from on-chain transactions.
-- NOTE(AB): I tried to separate the 2 functions, the one working on list of txs and the one
-- working on single tx but I keep getting failed unification between `m` and `m0` which is
-- puzzling...
runOnChainTxs :: forall m tx. MonadSTM m => TVar m OnChainHeadState -> [ValidatedTx Era] -> m [OnChainTx tx]
runOnChainTxs headState = atomically . foldM runOnChainTx []
 where
  runOnChainTx :: [OnChainTx tx] -> ValidatedTx Era -> STM m [OnChainTx tx]
  runOnChainTx observed tx = do
    newObserved <- catMaybes <$> mapM (stateTVar headState) [observeInitTx tx, observeAbortTx tx]
    pure $ observed <> newObserved

observeInitTx :: ValidatedTx Era -> OnChainHeadState -> (Maybe (OnChainTx tx), OnChainHeadState)
observeInitTx ValidatedTx{wits} st =
  case extractParameters of
    Just (Head.Initial cp ps) ->
      ( Just $ OnInitTx (contestationPeriodToDiffTime cp) (map convertParty ps)
      , st
      )
    _ -> (Nothing, st)
 where
  extractParameters = do
    Data d <- firstDatum
    fromData d
  firstDatum = snd . head <$> nonEmpty datums

  datums = Map.toList . unTxDats $ txdats wits

  convertParty = anonymousParty . partyToVerKey

  _firstInput = error "undefined"

observeAbortTx :: ValidatedTx Era -> OnChainHeadState -> (Maybe (OnChainTx tx), OnChainHeadState)
observeAbortTx _ st = (Nothing, st)
--

-- * Helpers

mkUnsignedTx ::
  TxBody Era ->
  TxDats Era ->
  Map RdmrPtr (Data Era, ExUnits) ->
  Map (ScriptHash StandardCrypto) (Script Era) ->
  ValidatedTx Era
mkUnsignedTx body datums redeemers scripts =
  ValidatedTx
    { body
    , wits =
        TxWitness
          { txwitsVKey = mempty
          , txwitsBoot = mempty
          , txscripts = scripts
          , txdats = datums
          , txrdmrs = Redeemers redeemers
          }
    , isValid = IsValid True -- REVIEW(SN): no idea of the semantics of this
    , auxiliaryData = SNothing
    }

-- | Get the ledger address for a given plutus script.
scriptAddr :: Script Era -> Addr StandardCrypto
scriptAddr script =
  Addr
    network
    (ScriptHashObj $ hashScript @Era script)
    -- REVIEW(SN): stake head funds?
    StakeRefNull

plutusScript :: Plutus.Script -> Script Era
plutusScript = PlutusScript . toShort . fromLazy . serialize
