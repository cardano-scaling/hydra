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
import Cardano.Ledger.Alonzo (Script)
import Cardano.Ledger.Alonzo.Data (Data (Data), DataHash, getPlutusData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (PlutusScript))
import Cardano.Ledger.Alonzo.Tx (IsValid (IsValid), ScriptPurpose (Spending), ValidatedTx (..), rdptr)
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxInfo (transKeyHash)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, Redeemers (..), TxDats (..), TxWitness (..), unRedeemers, unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (hashScript)
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (
  Coin (..),
  Credential (ScriptHashObj),
  Network (Testnet),
  ScriptHash,
  StakeReference (StakeRefNull),
  StrictMaybe (..),
  TxId (TxId),
  TxIn (TxIn),
  VKey (VKey),
  Wdrl (Wdrl),
  hashKey,
 )
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val (inject)
import Control.Monad (foldM)
import Control.Monad.Class.MonadSTM (MonadSTMTx (writeTVar))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe.Strict (strictMaybeToMaybe)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters (..), OnChainTx (OnAbortTx, OnCommitTx, OnInitTx))
import Hydra.Chain.Direct.Util (Era, VerificationKey)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.MockCommit as MockCommit
import qualified Hydra.Contract.MockInitial as MockInitial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime, contestationPeriodToDiffTime)
import Hydra.Data.Party (partyFromVerKey, partyToVerKey)
import qualified Hydra.Data.Party as OnChain
import Hydra.Data.Utxo (fromByteString)
import qualified Hydra.Data.Utxo as OnChain
import Hydra.Ledger (Tx, Utxo)
import Hydra.Party (Party, anonymousParty, vkey)
import Ledger.Value (AssetClass (..), currencyMPSHash)
import Plutus.V1.Ledger.Api (MintingPolicyHash, PubKeyHash (..), fromData, toData)
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.V1.Ledger.Value (assetClass, currencySymbol, tokenName)

-- TODO(SN): parameterize
network :: Network
network = Testnet

-- * Post Hydra Head transactions

-- | Maintains information needed to construct on-chain transactions
-- depending on the current state of the head.
data OnChainHeadState
  = Closed
  | Initial
      { -- | The state machine UTxO produced by the Init transaction
        -- This output should always be present and 'threaded' across all
        -- transactions.
        threadOutput :: (TxIn StandardCrypto, TxOut Era, AssetClass, HeadParameters)
      , -- TODO add commits
        initials :: [(TxIn StandardCrypto, PubKeyHash)]
      }
  | Final
  deriving (Eq, Show, Generic)

ownInitial :: VerificationKey -> [(TxIn StandardCrypto, PubKeyHash)] -> Maybe (TxIn StandardCrypto, PubKeyHash)
ownInitial vkey =
  find ((== pkh) . snd)
 where
  pkh = transKeyHash $ hashKey @StandardCrypto $ VKey vkey

-- FIXME: should not be hardcoded, for testing purposes only
threadToken :: AssetClass
threadToken = assetClass (currencySymbol "hydra") (tokenName "token")

-- FIXME: should not be hardcoded
policyId :: MintingPolicyHash
(policyId, _) = first currencyMPSHash (unAssetClass threadToken)

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx :: HeadParameters -> TxIn StandardCrypto -> ValidatedTx Era
initTx HeadParameters{contestationPeriod, parties} txIn =
  mkUnsignedTx body dats (Redeemers mempty) mempty
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

  headAddress = scriptAddr $ plutusScript $ Head.validatorScript policyId

  -- REVIEW(SN): how much to store here / minUtxoValue / depending on assets?
  headValue = inject (Coin 2000000)

  headDatumHash = hashData @Era headDatum

  headDatum =
    Data . toData $
      Head.Initial
        (contestationPeriodFromDiffTime contestationPeriod)
        (map (partyFromVerKey . vkey) parties)

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
-- TODO(SN): Eventually, this might not be necessary as the 'Utxo tx' would need
-- to be inputs of this transaction.
commitTx ::
  Tx tx =>
  Party ->
  Utxo tx ->
  -- | The inital output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn StandardCrypto, PubKeyHash) ->
  ValidatedTx Era
commitTx party utxo (initialIn, _pkh) =
  mkUnsignedTx body datums redeemers scripts
 where
  body =
    TxBody
      { inputs = Set.singleton initialIn
      , collateral = mempty
      , outputs =
          StrictSeq.fromList
            [ TxOut
                (scriptAddr commitScript)
                (inject $ Coin 2000000) -- TODO: Value of utxo + whatever is in initialIn
                (SJust $ hashData @Era commitDatum)
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

  datums =
    datumsFromList [commitDatum]

  redeemers = redeemersFromList []

  scripts = fromList $ map withScriptHash [initialScript]

  initialScript = plutusScript MockInitial.validatorScript

  commitScript = plutusScript MockCommit.validatorScript

  commitDatum =
    Data . toData $
      MockCommit.datum (partyFromVerKey $ vkey party, commitUtxo)

  commitUtxo = fromByteString $ toStrict $ Aeson.encode utxo

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
abortTx (smInput, _token, HeadParameters{contestationPeriod, parties}) initInputs =
  mkUnsignedTx body datums redeemers scripts
 where
  body =
    TxBody
      { inputs = Set.fromList (smInput : map fst initInputs)
      , collateral = mempty
      , outputs =
          StrictSeq.fromList
            [ TxOut
                (scriptAddr headScript)
                (inject $ Coin 2000000) -- TODO: This really needs to be passed as argument
                (SJust $ hashData @Era abortDatum)
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

  scripts =
    fromList $
      map withScriptHash $
        headScript : [initialScript | not (null initInputs)]

  initialScript = plutusScript MockInitial.validatorScript

  headScript = plutusScript $ Head.validatorScript policyId

  -- TODO(SN): dummy exUnits, balancing overrides them?
  redeemers =
    redeemersFromList $
      (rdptr body (Spending smInput), (headRedeemer, ExUnits 0 0)) : initialRedeemers

  headRedeemer = Data $ toData Head.Abort

  initialRedeemers =
    map
      ( \(txin, _) ->
          ( rdptr body (Spending txin)
          , (Data $ toData $ Plutus.getRedeemer $ MockInitial.redeemer (), ExUnits 0 0)
          )
      )
      initInputs

  -- NOTE: Those datums contain the datum of the spent state-machine input, but
  -- also, the datum of the created output which is necessary for the
  -- state-machine on-chain validator to control the correctness of the
  -- transition.
  datums =
    datumsFromList $ abortDatum : headDatum : map initialDatum initInputs

  headDatum =
    Data $
      toData $
        Head.Initial
          (contestationPeriodFromDiffTime contestationPeriod)
          (map (partyFromVerKey . vkey) parties)

  initialDatum (_, pkh) =
    Data $ toData $ MockInitial.datum pkh

  abortDatum =
    Data $ toData Head.Final

-- * Observe Hydra Head transactions

-- | Update observable on-chain head state from on-chain transactions.
-- NOTE(AB): I tried to separate the 2 functions, the one working on list of txs and the one
-- working on single tx but I keep getting failed unification between `m` and `m0` which is
-- puzzling...
runOnChainTxs :: forall m tx. MonadSTM m => Party -> TVar m OnChainHeadState -> [ValidatedTx Era] -> m [OnChainTx tx]
runOnChainTxs party headState = fmap reverse . atomically . foldM runOnChainTx []
 where
  runOnChainTx :: [OnChainTx tx] -> ValidatedTx Era -> STM m [OnChainTx tx]
  runOnChainTx observed tx = do
    case asum [observeInitTx party tx, observeAbortTx tx] of
      Just (onChainTx, onChainHeadState) -> do
        writeTVar headState onChainHeadState
        pure $ onChainTx : observed
      Nothing -> pure observed

observeInitTx :: Party -> ValidatedTx Era -> Maybe (OnChainTx tx, OnChainHeadState)
observeInitTx party ValidatedTx{wits, body} = do
  (dh, Head.Initial cp ps) <- getFirst $ foldMap (First . decodeInitDatum) datums
  let parties = map convertParty ps
  let cperiod = contestationPeriodToDiffTime cp
  guard $ party `elem` parties
  (i, o) <- getFirst $ foldMap (First . findSmOutput dh) indexedOutputs
  pure
    ( OnInitTx cperiod parties
    , Initial
        { threadOutput =
            (i, o, threadToken, HeadParameters cperiod parties)
        , initials = []
        }
    )
 where
  decodeInitDatum (dh, d) =
    (dh,) <$> fromData (getPlutusData d)

  findSmOutput dh (ix, o@(TxOut _ _ dh')) =
    guard (SJust dh == dh') $> (i, o)
   where
    i = TxIn (TxId $ SafeHash.hashAnnotated body) ix

  datums =
    Map.toList . unTxDats $ txdats wits

  indexedOutputs =
    zip [0 ..] (toList (outputs body))

convertParty :: OnChain.Party -> Party
convertParty = anonymousParty . partyToVerKey

-- | Identify a commit tx by looking for an output which pays to v_commit.
observeCommitTx :: forall tx. Tx tx => ValidatedTx Era -> Maybe (OnChainTx tx)
observeCommitTx ValidatedTx{wits, body} = do
  txOut <- findCommitOutput
  (party, utxo) <- decodeCommitDatum txOut
  OnCommitTx (convertParty party) <$> convertUtxo utxo
 where
  findCommitOutput =
    find payToCommitScript (outputs body)

  decodeCommitDatum (TxOut _ _ datumHash) =
    strictMaybeToMaybe datumHash >>= lookupDatum >>= fromData . getPlutusData

  lookupDatum datumHash =
    Map.lookup datumHash . unTxDats $ txdats wits

  payToCommitScript (TxOut address _ _) =
    address == scriptAddr (plutusScript MockCommit.validatorScript)

  convertUtxo :: OnChain.Utxo -> Maybe (Utxo tx)
  convertUtxo = Aeson.decodeStrict' . OnChain.toByteString

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
-- TODO(SN): make sure this is aborting "the right head / your head"
observeAbortTx ::
  -- | A Utxo set to lookup tx inputs
  Map (TxIn StandardCrypto) (TxOut Era) ->
  ValidatedTx Era ->
  Maybe (OnChainTx tx, OnChainHeadState)
observeAbortTx utxo ValidatedTx{wits} = do
  -- XXX(SN): not hard-code policyId
  txOut <- findScriptOutput utxo . plutusScript $ Head.validatorScript policyId
  case decodeHeadRedeemer txOut of
    Just Head.Abort -> Just (OnAbortTx, Final)
    _ -> Nothing
 where
  decodeHeadRedeemer = undefined

  decodeData d s = s <|> fromData (getPlutusData d)

  redeemerData = fmap fst . Map.elems . unRedeemers $ txrdmrs wits

findScriptOutput ::
  Map (TxIn StandardCrypto) (TxOut Era) ->
  Script Era ->
  Maybe (TxOut StandardCrypto)
findScriptOutput _script = undefined

-- | Provide a UTXO map for some given OnChainHeadState. At least used by the
-- TinyWallet to lookup inputs.
knownUtxo :: OnChainHeadState -> Map (TxIn StandardCrypto) (TxOut Era)
knownUtxo = \case
  Initial{threadOutput = (i, o, _, _)} ->
    -- FIXME: initials should also be part of the resulting UTXO
    Map.singleton i o
  _ ->
    mempty

-- * Helpers

mkUnsignedTx ::
  TxBody Era ->
  TxDats Era ->
  Redeemers Era ->
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
          , txrdmrs = redeemers
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
plutusScript = PlutusScript PlutusV1 . toShort . fromLazy . serialize

withDataHash :: Data Era -> (DataHash StandardCrypto, Data Era)
withDataHash d = (hashData d, d)

withScriptHash :: Script Era -> (ScriptHash StandardCrypto, Script Era)
withScriptHash s = (hashScript @Era s, s)

datumsFromList :: [Data Era] -> TxDats Era
datumsFromList = TxDats . Map.fromList . fmap withDataHash

-- | Slightly unsafe, as it drops `SNothing` values from the list silently.
redeemersFromList ::
  [(StrictMaybe RdmrPtr, (Data Era, ExUnits))] ->
  Redeemers Era
redeemersFromList =
  Redeemers . Map.fromList . foldl' hasRdmrPtr []
 where
  hasRdmrPtr acc = \case
    (SNothing, _) -> acc
    (SJust v, ex) -> (v, ex) : acc
