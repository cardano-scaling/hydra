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
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (PlutusScript))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ScriptPurpose (Spending), ValidatedTx (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, Redeemers (..), TxDats (..), TxWitness (..), unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (hashScript)
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (inject)
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (stateTVar)
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (..), OnChainTx (OnInitTx))
import qualified Hydra.Contract.Initial as Initial
import qualified Hydra.Contract.MockHead as Head
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import Hydra.Party (anonymousParty, vkey)
import Ledger (AssetClass)
import Plutus.V1.Ledger.Api (PubKeyHash (..), fromData, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)
import Shelley.Spec.Ledger.API (
  Coin (..),
  Credential (ScriptHashObj),
  Network (Testnet),
  ScriptHash,
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxId (TxId),
  TxIn (TxIn),
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
      { -- | The state machine UTxO produced by the Init transaction
        -- This output should always be present and 'threaded' across all
        -- transactions.
        threadOutput :: (TxIn StandardCrypto, AssetClass, HeadParameters)
      , -- TODO initials should be a list of inputs/PubKeyHas
        -- TODO add commits
        initials :: [(TxIn StandardCrypto, PubKeyHash)]
      }
  deriving (Eq, Show, Generic)

-- FIXME: should not be hardcoded, for testing purposes only
threadToken :: AssetClass
threadToken = assetClass (currencySymbol "hydra") (tokenName "thread token")

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

  -- TODO: replace hardcoded asset class with one derived from the txIn
  headAddress = scriptAddr $ plutusScript $ Head.validatorScript threadToken

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
  -- | The data needed to consume the state-machine output
  (TxIn StandardCrypto, AssetClass, HeadParameters) ->
  -- | Data needed to consume the inital output sent to each party to the Head
  -- which should contain the PT and is locked by initial script
  [(TxIn StandardCrypto, PubKeyHash)] ->
  ValidatedTx Era
abortTx (txIn, token, HeadParameters{contestationPeriod, parties}) initInputs =
  mkUnsignedTx body dats redeemers scripts
 where
  body =
    TxBody
      { inputs = Set.fromList (txIn : map fst initInputs)
      , collateral = mempty
      , outputs =
          StrictSeq.fromList
            [ TxOut
                (scriptAddr headScript)
                (inject $ Coin 0) -- TODO: This really needs to be passed as argument
                (SJust headDatumHash)
            ]
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
  redeemers =
    Map.fromList $
      foldl' hasRdmrPtr [] $
        (rdptr body (Spending txIn), (headRedeemer, ExUnits 0 0)) :
        initialRedeemers

  hasRdmrPtr acc = \case
    (SNothing, _) -> acc
    (SJust v, ex) -> (v, ex) : acc

  headRedeemer = Data $ toData Head.Abort

  initialRedeemers =
    map
      (\(txin, _) -> (rdptr body (Spending txin), (Data $ toData (), ExUnits 0 0)))
      initInputs

  scripts = Map.fromList $ map (\s -> (hashScript @Era s, s)) [initialScript, headScript]

  initialScript = plutusScript Initial.validatorScript

  headScript = plutusScript $ Head.validatorScript token

  dats =
    TxDats $
      Map.fromList $
        (headDatumHash, headDatum) :
        map (initialDatum . snd) initInputs

  headDatumHash = hashData @Era headDatum

  headDatum =
    Data $
      toData $
        Head.Initial
          (contestationPeriodFromDiffTime contestationPeriod)
          (map (partyFromVerKey . vkey) parties)

  initialDatum pkh =
    let datum = Data $ toData pkh
     in (hashData @Era datum, datum)

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
observeInitTx ValidatedTx{wits, body} st =
  case extractParameters of
    Just (Head.Initial cp ps) ->
      ( Just $ OnInitTx (contestationPeriodToDiffTime cp) (map convertParty ps)
      , Initial
          { threadOutput =
              (firstInput, threadToken, HeadParameters (contestationPeriodToDiffTime cp) (map convertParty ps))
          , initials = []
          }
      )
    _ -> (Nothing, st)
 where
  extractParameters = do
    Data d <- firstDatum
    fromData d
  firstDatum = snd . head <$> nonEmpty datums

  datums = Map.toList . unTxDats $ txdats wits

  convertParty = anonymousParty . partyToVerKey

  firstInput = TxIn (TxId $ SafeHash.hashAnnotated body) 0

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
