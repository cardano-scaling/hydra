{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.Cardano.Api.Network (networkIdToNetwork)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.ContestationPeriod (ContestationPeriod, fromChain, toChain)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Contract.MintAction (MintAction (Burn, Mint))
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.HeadId (HeadId (..))
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (addReferenceInputs)
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addVkInputs,
  burnTokens,
  emptyTxBody,
  mintTokens,
  setValidityLowerBound,
  setValidityUpperBound,
  unsafeBuildTransaction,
 )
import Hydra.Party (Party, partyFromChain, partyToChain)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..), SnapshotNumber, fromChainSnapshot)
import PlutusLedgerApi.V2 (CurrencySymbol (CurrencySymbol), fromBuiltin, toBuiltin)
import PlutusLedgerApi.V2 qualified as Plutus

-- | Needed on-chain data to create Head transactions.
type UTxOWithScript = (TxIn, TxOut CtxUTxO, HashableScriptData)

newtype UTxOHash = UTxOHash ByteString
  deriving stock (Eq, Show, Generic)

instance ToJSON UTxOHash where
  toJSON (UTxOHash bytes) =
    Aeson.String . decodeUtf8 $ Base16.encode bytes

instance FromJSON UTxOHash where
  parseJSON = Aeson.withText "UTxOHash" $ \cborText ->
    case Base16.decode $ encodeUtf8 cborText of
      Left e -> fail e
      Right bs -> pure $ UTxOHash bs

-- | Representation of the Head output after an Init transaction.
data InitialThreadOutput = InitialThreadOutput
  { initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initialContestationPeriod :: OnChain.ContestationPeriod
  , initialParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Representation of the Head output after a CollectCom transaction.
data OpenThreadOutput = OpenThreadOutput
  { openThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , openContestationPeriod :: OnChain.ContestationPeriod
  , openParties :: [OnChain.Party]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ClosedThreadOutput = ClosedThreadOutput
  { closedThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , closedParties :: [OnChain.Party]
  , closedContestationDeadline :: Plutus.POSIXTime
  , closedContesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

hydraHeadV1AssetName :: AssetName
hydraHeadV1AssetName = AssetName (fromBuiltin hydraHeadV1)

-- FIXME: sould not be hardcoded
headValue :: Value
headValue = lovelaceToValue (Lovelace 2_200_000)

-- * Create Hydra Head transactions

-- | Create the init transaction from some 'HeadParameters' and a single TxIn
-- which will be used as unique parameter for minting NFTs.
initTx ::
  NetworkId ->
  -- | All participants cardano keys.
  [VerificationKey PaymentKey] ->
  HeadParameters ->
  TxIn ->
  Tx
initTx networkId cardanoKeys parameters seedTxIn =
  unsafeBuildTransaction $
    emptyTxBody
      & addVkInputs [seedTxIn]
      & addOutputs
        ( mkHeadOutputInitial networkId seedTxIn parameters
            : map (mkInitialOutput networkId seedTxIn) cardanoKeys
        )
      & mintTokens (HeadTokens.mkHeadTokenScript seedTxIn) Mint ((hydraHeadV1AssetName, 1) : participationTokens)
 where
  participationTokens =
    [(assetNameFromVerificationKey vk, 1) | vk <- cardanoKeys]

mkHeadOutput :: NetworkId -> PolicyId -> TxOutDatum ctx -> TxOut ctx
mkHeadOutput networkId tokenPolicyId datum =
  TxOut
    (mkScriptAddress @PlutusScriptV2 networkId headScript)
    (headValue <> valueFromList [(AssetId tokenPolicyId hydraHeadV1AssetName, 1)])
    datum
    ReferenceScriptNone
 where
  headScript = fromPlutusScript Head.validatorScript

mkHeadOutputInitial :: NetworkId -> TxIn -> HeadParameters -> TxOut CtxTx
mkHeadOutputInitial networkId seedTxIn HeadParameters{contestationPeriod, parties} =
  mkHeadOutput networkId tokenPolicyId headDatum
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  headDatum =
    mkTxOutDatumInline $
      Head.Initial
        { contestationPeriod = toChain contestationPeriod
        , parties = map partyToChain parties
        , headId = toPlutusCurrencySymbol tokenPolicyId
        , seed = toPlutusTxOutRef seedTxIn
        }

mkInitialOutput :: NetworkId -> TxIn -> VerificationKey PaymentKey -> TxOut CtxTx
mkInitialOutput networkId seedTxIn (verificationKeyHash -> pkh) =
  TxOut initialAddress initialValue initialDatum ReferenceScriptNone
 where
  tokenPolicyId = HeadTokens.headPolicyId seedTxIn
  initialValue =
    headValue <> valueFromList [(AssetId tokenPolicyId (AssetName $ serialiseToRawBytes pkh), 1)]
  initialAddress =
    mkScriptAddress @PlutusScriptV2 networkId initialScript
  initialScript =
    fromPlutusScript Initial.validatorScript
  initialDatum =
    mkTxOutDatumInline $ Initial.datum (toPlutusCurrencySymbol tokenPolicyId)

-- | Craft a commit transaction which includes the "committed" utxo as a datum.
commitTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  HeadId ->
  Party ->
  -- | The UTxO to commit to the Head along with witnesses.
  UTxO' (TxOut CtxUTxO, Witness WitCtxTxIn) ->
  -- | The initial output (sent to each party) which should contain the PT and is
  -- locked by initial script
  (TxIn, TxOut CtxUTxO, Hash PaymentKey) ->
  Tx
commitTx networkId scriptRegistry headId party utxoToCommitWitnessed (initialInput, out, vkh) =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(initialInput, initialWitness)]
      & addReferenceInputs [initialScriptRef]
      & addInputs committedTxIns
      & addExtraRequiredSigners [vkh]
      & addOutputs [commitOutput]
 where
  initialWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference initialScriptRef initialScript InlineScriptDatum initialRedeemer

  initialScript =
    fromPlutusScript @PlutusScriptV2 Initial.validatorScript

  initialScriptRef =
    fst (initialReference scriptRegistry)

  initialRedeemer =
    toScriptData . Initial.redeemer $
      Initial.ViaCommit (toPlutusTxOutRef . fst <$> committedTxIns)

  committedTxIns =
    map (\(i, (_, w)) -> (i, BuildTxWith w)) $ UTxO.pairs utxoToCommitWitnessed

  commitOutput =
    TxOut commitAddress commitValue commitDatum ReferenceScriptNone

  commitScript =
    fromPlutusScript Commit.validatorScript

  commitAddress =
    mkScriptAddress @PlutusScriptV2 networkId commitScript

  commitValue =
    txOutValue out <> foldMap txOutValue utxoToCommit

  commitDatum =
    mkTxOutDatumInline $ mkCommitDatum party utxoToCommit (headIdToCurrencySymbol headId)

  utxoToCommit = fst <$> utxoToCommitWitnessed

mkCommitDatum :: Party -> UTxO -> CurrencySymbol -> Plutus.Datum
mkCommitDatum party utxo headId =
  Commit.datum (partyToChain party, commits, headId)
 where
  commits =
    mapMaybe Commit.serializeCommit $ UTxO.pairs utxo

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Everything needed to spend the Head state-machine output.
  InitialThreadOutput ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | Head id
  HeadId ->
  Tx
collectComTx networkId scriptRegistry vk initialThreadOutput commits headId =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs ((headInput, headWitness) : (mkCommit <$> Map.keys commits))
      & addReferenceInputs [commitScriptRef, headScriptRef]
      & addOutputs [headOutput]
      & addExtraRequiredSigners [verificationKeyHash vk]
 where
  InitialThreadOutput
    { initialThreadUTxO = (headInput, initialHeadOutput)
    , initialParties
    , initialContestationPeriod
    } = initialThreadOutput
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headScriptRef = fst (headReference scriptRegistry)
  headRedeemer = toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV2 networkId headScript)
      (txOutValue initialHeadOutput <> commitValue)
      headDatumAfter
      ReferenceScriptNone
  headDatumAfter =
    mkTxOutDatumInline
      Head.Open
        { Head.parties = initialParties
        , utxoHash
        , contestationPeriod = initialContestationPeriod
        , headId = headIdToCurrencySymbol headId
        }

  extractCommits txOut =
    case txOutScriptData (toTxContext txOut) >>= fromScriptData of
      Nothing -> error "Expected inline commit datum" -- TODO: proper error handling
      Just ((_, cs, _) :: Commit.DatumType) -> cs

  utxoHash =
    Head.hashPreSerializedCommits $ foldMap extractCommits commits

  mkCommit commitTxIn = (commitTxIn, commitWitness)
  commitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitValue =
    mconcat $ txOutValue <$> Map.elems commits
  commitScript =
    fromPlutusScript @PlutusScriptV2 Commit.validatorScript
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.ViaCollectCom

-- | Low-level data type of a snapshot to close the head with. This is different
-- to the 'ConfirmedSnasphot', which is provided to `CloseTx` as it also
-- contains relevant chain state like the 'openUtxoHash'.
data ClosingSnapshot
  = CloseWithInitialSnapshot {openUtxoHash :: UTxOHash}
  | CloseWithConfirmedSnapshot
      { snapshotNumber :: SnapshotNumber
      , closeUtxoHash :: UTxOHash
      , -- XXX: This is a bit of a wart and stems from the fact that our
        -- SignableRepresentation of 'Snapshot' is in fact the snapshotNumber
        -- and the closeUtxoHash as also included above
        signatures :: MultiSignature (Snapshot Tx)
      }

-- | Create a transaction closing a head with either the initial snapshot or
-- with a multi-signed confirmed snapshot.
closeTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | The snapshot to close with, can be either initial or confirmed one.
  ClosingSnapshot ->
  -- | Lower validity slot number, usually a current or quite recent slot number.
  SlotNo ->
  -- | Upper validity slot and UTC time to compute the contestation deadline time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  OpenThreadOutput ->
  -- | Head identifier
  HeadId ->
  Tx
closeTx scriptRegistry vk closing startSlotNo (endSlotNo, utcTime) openThreadOutput headId =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityLowerBound startSlotNo
      & setValidityUpperBound endSlotNo
 where
  OpenThreadOutput
    { openThreadUTxO = (headInput, headOutputBefore)
    , openContestationPeriod
    , openParties
    } = openThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript

  headRedeemer =
    toScriptData
      Head.Close
        { signature
        }

  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  headDatumAfter =
    mkTxOutDatumInline
      Head.Closed
        { snapshotNumber
        , utxoHash = toBuiltin utxoHashBytes
        , parties = openParties
        , contestationDeadline
        , contestationPeriod = openContestationPeriod
        , headId = headIdToCurrencySymbol headId
        , contesters = []
        }

  snapshotNumber = toInteger $ case closing of
    CloseWithInitialSnapshot{} -> 0
    CloseWithConfirmedSnapshot{snapshotNumber = sn} -> sn

  UTxOHash utxoHashBytes = case closing of
    CloseWithInitialSnapshot{openUtxoHash} -> openUtxoHash
    CloseWithConfirmedSnapshot{closeUtxoHash} -> closeUtxoHash

  signature = case closing of
    CloseWithInitialSnapshot{} -> mempty
    CloseWithConfirmedSnapshot{signatures = s} -> toPlutusSignatures s

  contestationDeadline =
    addContestationPeriod (posixFromUTCTime utcTime) openContestationPeriod

-- XXX: This function is VERY similar to the 'closeTx' function (only notable
-- difference being the redeemer, which is in itself also the same structure as
-- the close's one. We could potentially refactor this to avoid repetition or do
-- something more principled at the protocol level itself and "merge" close and
-- contest as one operation.
contestTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Contested snapshot number (i.e. the one we contest to)
  Snapshot Tx ->
  -- | Multi-signature of the whole snapshot
  MultiSignature (Snapshot Tx) ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  -- | Everything needed to spend the Head state-machine output.
  ClosedThreadOutput ->
  HeadId ->
  ContestationPeriod ->
  Tx
contestTx scriptRegistry vk Snapshot{number, utxo} sig (slotNo, _) closedThreadOutput headId contestationPeriod =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs [headOutputAfter]
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setValidityUpperBound slotNo
 where
  ClosedThreadOutput
    { closedThreadUTxO = (headInput, headOutputBefore)
    , closedParties
    , closedContestationDeadline
    , closedContesters
    } = closedThreadOutput

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData
      Head.Contest
        { signature = toPlutusSignatures sig
        }
  headOutputAfter =
    modifyTxOutDatum (const headDatumAfter) headOutputBefore

  contester = toPlutusKeyHash (verificationKeyHash vk)

  onChainConstestationPeriod = toChain contestationPeriod

  newContestationDeadline =
    if length (contester : closedContesters) == length closedParties
      then closedContestationDeadline
      else addContestationPeriod closedContestationDeadline onChainConstestationPeriod

  headDatumAfter =
    mkTxOutDatumInline
      Head.Closed
        { snapshotNumber = toInteger number
        , utxoHash
        , parties = closedParties
        , contestationDeadline = newContestationDeadline
        , contestationPeriod = onChainConstestationPeriod
        , headId = headIdToCurrencySymbol headId
        , contesters = contester : closedContesters
        }
  utxoHash = toBuiltin $ hashUTxO @Tx utxo

-- | Create the fanout transaction, which distributes the closed state
-- accordingly. The head validator allows fanout only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
fanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo (headInput, headOutput) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs orderedTxOutsToFanout
      & burnTokens headTokenScript Burn headTokens
      & setValidityLowerBound (deadlineSlotNo + 1)
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData (Head.Fanout $ fromIntegral $ length utxo)

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxOutsToFanout =
    toTxContext <$> toList utxo

data AbortTxError = OverlappingInputs
  deriving stock (Show)

-- | Create transaction which aborts a head by spending the Head output and all
-- other "initial" outputs.
abortTx ::
  -- | Committed UTxOs to reimburse.
  UTxO ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Script for monetary policy to burn tokens
  PlutusScript ->
  -- | Data needed to spend the initial output sent to each party to the Head.
  -- Should contain the PT and is locked by initial script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | Data needed to spend commit outputs.
  -- Should contain the PT and is locked by commit script.
  Map TxIn (TxOut CtxUTxO) ->
  Either AbortTxError Tx
abortTx committedUTxO scriptRegistry vk (headInput, initialHeadOutput) headTokenScript initialsToAbort commitsToAbort
  | isJust (lookup headInput initialsToAbort) =
      Left OverlappingInputs
  | otherwise =
      Right $
        unsafeBuildTransaction $
          emptyTxBody
            & addInputs ((headInput, headWitness) : initialInputs <> commitInputs)
            & addReferenceInputs [initialScriptRef, commitScriptRef, headScriptRef]
            & addOutputs reimbursedOutputs
            & burnTokens headTokenScript Burn headTokens
            & addExtraRequiredSigners [verificationKeyHash vk]
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headScript =
    fromPlutusScript @PlutusScriptV2 Head.validatorScript
  headRedeemer =
    toScriptData Head.Abort

  initialInputs = mkAbortInitial <$> Map.keys initialsToAbort

  commitInputs = mkAbortCommit <$> Map.keys commitsToAbort

  headTokens =
    headTokensFromValue headTokenScript $
      mconcat
        [ txOutValue initialHeadOutput
        , foldMap txOutValue initialsToAbort
        , foldMap txOutValue commitsToAbort
        ]

  mkAbortInitial initialInput = (initialInput, abortInitialWitness)
  abortInitialWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference initialScriptRef initialScript InlineScriptDatum initialRedeemer
  initialScriptRef =
    fst (initialReference scriptRegistry)
  initialScript =
    fromPlutusScript @PlutusScriptV2 Initial.validatorScript
  initialRedeemer =
    toScriptData $ Initial.redeemer Initial.ViaAbort

  mkAbortCommit commitInput = (commitInput, abortCommitWitness)
  abortCommitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitScript =
    fromPlutusScript @PlutusScriptV2 Commit.validatorScript
  commitRedeemer =
    toScriptData (Commit.redeemer Commit.ViaAbort)

  reimbursedOutputs = toTxContext . snd <$> UTxO.pairs committedUTxO

-- * Observe Hydra Head transactions

-- | Generalised type for arbitrary Head observations on-chain.
data HeadObservation
  = NoHeadTx
  | Init RawInitObservation
  | Abort AbortObservation
  | Commit CommitObservation
  | CollectCom CollectComObservation
  | Close CloseObservation
  | Contest ContestObservation
  | Fanout FanoutObservation

-- | Observe any Hydra head transaction.
observeHeadTx :: NetworkId -> UTxO -> Tx -> HeadObservation
observeHeadTx networkId utxo tx =
  fromMaybe NoHeadTx $
    either (const Nothing) (Just . Init) (observeRawInitTx networkId tx)
      <|> Abort <$> observeAbortTx utxo tx
      <|> Commit <$> observeCommitTx networkId utxo tx
      <|> CollectCom <$> observeCollectComTx utxo tx
      <|> Close <$> observeCloseTx utxo tx
      <|> Contest <$> observeContestTx utxo tx
      <|> Fanout <$> observeFanoutTx utxo tx

-- | Data extracted from a `Tx` that looks like an `InitTx` which could be of
-- interest to us.
data RawInitObservation = RawInitObservation
  { headId :: PolicyId
  , contestationPeriod :: ContestationPeriod
  , onChainParties :: [OnChain.Party]
  , seedTxIn :: TxIn
  , headPTsNames :: [AssetName]
  , initialThreadUTxO :: (TxIn, TxOut CtxUTxO)
  , initials :: [(TxIn, TxOut CtxUTxO)]
  }
  deriving (Show, Eq)

-- | An observation for an `InitTx` which our node is a party for.
data InitObservation = InitObservation
  { threadOutput :: InitialThreadOutput
  -- ^ The state machine UTxO produced by the Init transaction
  -- This output should always be present and 'threaded' across all
  -- transactions.
  -- NOTE(SN): The Head's identifier is somewhat encoded in the TxOut's address
  -- XXX(SN): Data and [OnChain.Party] are overlapping
  , initials :: [(TxIn, TxOut CtxUTxO)]
  , commits :: [(TxIn, TxOut CtxUTxO)]
  , headId :: HeadId
  , seedTxIn :: TxIn
  , contestationPeriod :: ContestationPeriod
  , parties :: [Party]
  }
  deriving stock (Show, Eq)

-- | An explanation for a failed tentative `InitTx` observation.
data NotAnInit
  = -- | The transaction is definitely not a valid InitTx
    NotAnInit NotAnInitReason
  | -- | The transaction /is/ a valid  InitTx but does not match the configuration of our Head.
    NotAnInitForUs MismatchReason
  deriving stock (Show, Eq)

data NotAnInitReason
  = NoHeadOutput
  | NotAHeadDatum
  | NoSTFound
  | NotAHeadPolicy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary NotAnInitReason where
  arbitrary = genericArbitrary

data MismatchReason
  = PartiesMismatch RawInitObservation
  | OwnPartyMissing RawInitObservation
  | CPMismatch RawInitObservation
  | PTsNotMintedCorrectly RawInitObservation
  deriving (Show, Eq)

mismatchReasonObservation :: MismatchReason -> RawInitObservation
mismatchReasonObservation = \case
  PartiesMismatch obs -> obs
  OwnPartyMissing obs -> obs
  CPMismatch obs -> obs
  PTsNotMintedCorrectly obs -> obs

observeRawInitTx ::
  NetworkId ->
  Tx ->
  Either NotAnInitReason RawInitObservation
observeRawInitTx networkId tx = do
  -- XXX: Lots of redundant information here
  (ix, headOut, headState) <-
    maybeLeft NoHeadOutput $
      findFirst headOutput indexedOutputs

  -- check that we have a proper head
  (headId, contestationPeriod, onChainParties, seedTxIn) <- case headState of
    (Head.Initial cp ps cid outRef) -> do
      pure (fromPlutusCurrencySymbol cid, fromChain cp, ps, fromPlutusTxOutRef outRef)
    _ -> Left NotAHeadDatum

  let stQuantity = selectAsset (txOutValue headOut) (AssetId headId hydraHeadV1AssetName)

  -- check that ST is present in the head output
  unless (stQuantity == 1) $
    Left NoSTFound

  -- check that we are using the same seed and headId matches
  unless (headId == HeadTokens.headPolicyId seedTxIn) $
    Left NotAHeadPolicy

  pure $
    RawInitObservation
      { headId
      , contestationPeriod
      , onChainParties
      , seedTxIn
      , headPTsNames = mintedTokenNames headId
      , initialThreadUTxO =
          (mkTxIn tx ix, toCtxUTxOTxOut headOut)
      , initials
      }
 where
  maybeLeft e = maybe (Left e) Right

  headOutput = \case
    (ix, out@(TxOut addr _ (TxOutDatumInline d) _)) -> do
      guard $ addr == headAddress
      (ix,out,) <$> fromScriptData d
    _ -> Nothing

  headAddress =
    mkScriptAddress @PlutusScriptV2 networkId $
      fromPlutusScript Head.validatorScript

  indexedOutputs = zip [0 ..] (txOuts' tx)

  initialOutputs = filter (isInitial . snd) indexedOutputs

  initials =
    map
      (\(i, o) -> (mkTxIn tx i, toCtxUTxOTxOut o))
      initialOutputs

  isInitial (TxOut addr _ _ _) = addr == initialAddress

  initialAddress = mkScriptAddress @PlutusScriptV2 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

  mintedTokenNames headId =
    [ assetName
    | (AssetId policyId assetName, q) <- txMintAssets tx
    , -- NOTE: It is important to check quantity since we want to ensure
    -- the tokens are unique.
    q == 1
    , policyId == headId
    , assetName /= hydraHeadV1AssetName
    ]

observeInitTx ::
  [VerificationKey PaymentKey] ->
  -- | Our node's contestation period
  ContestationPeriod ->
  Party ->
  [Party] ->
  RawInitObservation ->
  Either MismatchReason InitObservation
observeInitTx cardanoKeys expectedCP party otherParties rawTx = do
  let offChainParties = concatMap partyFromChain onChainParties

  -- check that configured CP is present in the datum
  unless (expectedCP == contestationPeriod) $
    Left (CPMismatch rawTx)

  -- check that our party is present in the datum
  unless (party `elem` offChainParties) $
    Left (OwnPartyMissing rawTx)

  -- check that configured parties are matched in the datum
  unless (containsSameElements offChainParties allConfiguredParties) $
    Left (PartiesMismatch rawTx)

  -- pub key hashes of all configured participants == the token names of PTs
  unless (containsSameElements configuredTokenNames headPTsNames) $
    Left (PTsNotMintedCorrectly rawTx)

  pure
    InitObservation
      { threadOutput =
          InitialThreadOutput
            { initialThreadUTxO
            , initialParties = onChainParties
            , initialContestationPeriod = toChain contestationPeriod
            }
      , initials
      , commits = []
      , headId = mkHeadId headId
      , seedTxIn
      , -- NOTE: we should look into why parties and cp are duplicated in the InitObservation.
        -- They are included: Once in the InitialThreadOutput in their on-chain form, once in
        -- InitObservation in their off-chain form and they are also included in the datum of
        -- the initialThreadUTxO`.. so three times.
        contestationPeriod
      , parties = offChainParties
      }
 where
  RawInitObservation
    { headId
    , contestationPeriod
    , headPTsNames
    , onChainParties
    , seedTxIn
    , initialThreadUTxO
    , initials
    } = rawTx

  allConfiguredParties = party : otherParties

  configuredTokenNames = assetNameFromVerificationKey <$> cardanoKeys

  containsSameElements a b = Set.fromList a == Set.fromList b

-- | Full observation of a commit transaction.
data CommitObservation = CommitObservation
  { commitOutput :: (TxIn, TxOut CtxUTxO)
  , party :: Party
  -- ^ Hydra participant who committed the UTxO.
  , committed :: UTxO
  , headId :: HeadId
  }

-- | Identify a commit tx by:
--
-- - Check that its spending from the init validator,
-- - Find the outputs which pays to the commit validator,
-- - Using the datum of that output, deserialize the committed output,
-- - Reconstruct the committed UTxO from both values (tx input and output).
observeCommitTx ::
  NetworkId ->
  -- | A UTxO set to lookup tx inputs. Should at least contain the input
  -- spending from νInitial.
  UTxO ->
  Tx ->
  Maybe CommitObservation
observeCommitTx networkId utxo tx = do
  -- NOTE: Instead checking to spend from initial we could be looking at the
  -- seed:
  --
  --  - We must check that participation token in output satisfies
  --      policyId = hash(mu_head(seed))
  --
  --  - This allows us to assume (by induction) the output datum at the commit
  --    script is legit
  --
  --  - Further, we need to assert / assume that only one script is spent = onle
  --    one redeemer matches the InitialRedeemer, as we do not have information
  --    which of the inputs is spending from the initial script otherwise.
  --
  --  Right now we only have the headId in the datum, so we use that in place of
  --  the seed -> THIS CAN NOT BE TRUSTED.
  guard isSpendingFromInitial

  (commitIn, commitOut) <- findTxOutByAddress commitAddress tx
  dat <- txOutScriptData commitOut
  (onChainParty, onChainCommits, headId) :: Commit.DatumType <- fromScriptData dat
  party <- partyFromChain onChainParty

  -- NOTE: If we have the resolved inputs (utxo) then we could avoid putting
  -- the commit into the datum (+ changing the hashing strategy of
  -- collect/fanout)
  committed <- do
    committedUTxO <- traverse (Commit.deserializeCommit (networkIdToNetwork networkId)) onChainCommits
    pure . UTxO.fromPairs $ committedUTxO

  pure
    CommitObservation
      { commitOutput = (commitIn, toUTxOContext commitOut)
      , party
      , committed
      , headId = mkHeadId $ fromPlutusCurrencySymbol headId
      }
 where
  isSpendingFromInitial :: Bool
  isSpendingFromInitial = do
    let resolvedInputs = mapMaybe (`UTxO.resolve` utxo) $ txIns' tx
    any (\o -> txOutAddress o == initialAddress) resolvedInputs

  initialAddress = mkScriptAddress @PlutusScriptV2 networkId initialScript

  initialScript = fromPlutusScript Initial.validatorScript

  commitAddress = mkScriptAddress @PlutusScriptV2 networkId commitScript

  commitScript = fromPlutusScript Commit.validatorScript

data CollectComObservation = CollectComObservation
  { threadOutput :: OpenThreadOutput
  , headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving stock (Show, Eq)

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CollectComObservation
observeCollectComTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Initial{parties, contestationPeriod}, Head.CollectCom) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
      pure
        CollectComObservation
          { threadOutput =
              OpenThreadOutput
                { openThreadUTxO = (newHeadInput, newHeadOutput)
                , openParties = parties
                , openContestationPeriod = contestationPeriod
                }
          , headId
          , utxoHash
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript
  decodeUtxoHash datum =
    case fromScriptData datum of
      Just Head.Open{utxoHash} -> Just $ fromBuiltin utxoHash
      _ -> Nothing

data CloseObservation = CloseObservation
  { threadOutput :: ClosedThreadOutput
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  }
  deriving stock (Show, Eq)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeCloseTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CloseObservation
observeCloseTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{parties}, Head.Close{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      (closeContestationDeadline, onChainSnapshotNumber) <- case fromScriptData newHeadDatum of
        Just Head.Closed{contestationDeadline, snapshotNumber} -> pure (contestationDeadline, snapshotNumber)
        _ -> Nothing
      pure
        CloseObservation
          { threadOutput =
              ClosedThreadOutput
                { closedThreadUTxO = (newHeadInput, newHeadOutput)
                , closedParties = parties
                , closedContestationDeadline = closeContestationDeadline
                , closedContesters = []
                }
          , headId
          , snapshotNumber = fromChainSnapshot onChainSnapshotNumber
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data ContestObservation = ContestObservation
  { contestedThreadOutput :: (TxIn, TxOut CtxUTxO)
  , headId :: HeadId
  , snapshotNumber :: SnapshotNumber
  , contesters :: [Plutus.PubKeyHash]
  }
  deriving stock (Show, Eq)

-- | Identify a close tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeContestTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ContestObservation
observeContestTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ toTxContext headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Closed{contesters}, Head.Contest{}) -> do
      (newHeadInput, newHeadOutput) <- findTxOutByScript @PlutusScriptV2 (utxoFromTx tx) headScript
      newHeadDatum <- txOutScriptData $ toTxContext newHeadOutput
      let onChainSnapshotNumber = closedSnapshotNumber newHeadDatum
      pure
        ContestObservation
          { contestedThreadOutput = (newHeadInput, newHeadOutput)
          , headId
          , snapshotNumber = fromChainSnapshot onChainSnapshotNumber
          , contesters
          }
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

  closedSnapshotNumber headDatum =
    case fromScriptData headDatum of
      Just Head.Closed{snapshotNumber} -> snapshotNumber
      _ -> error "wrong state in output datum"

data FanoutObservation = FanoutObservation {headId :: HeadId}

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFanoutTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{} -> pure FanoutObservation{headId}
      _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

data AbortObservation = AbortObservation {headId :: HeadId}

-- | Identify an abort tx by looking up the input spending the Head output and
-- decoding its redeemer.
observeAbortTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe AbortObservation
observeAbortTx utxo tx = do
  (headInput, headOutput) <- findTxOutByScript @PlutusScriptV2 utxo headScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput >>= \case
    Head.Abort -> pure $ AbortObservation headId
    _ -> Nothing
 where
  headScript = fromPlutusScript Head.validatorScript

-- * Helpers

mkHeadId :: PolicyId -> HeadId
mkHeadId =
  HeadId . serialiseToRawBytes

headIdToCurrencySymbol :: HeadId -> CurrencySymbol
headIdToCurrencySymbol (HeadId headId) = CurrencySymbol (toBuiltin headId)

headTokensFromValue :: PlutusScript -> Value -> [(AssetName, Quantity)]
headTokensFromValue headTokenScript v =
  [ (assetName, q)
  | (AssetId pid assetName, q) <- valueToList v
  , pid == scriptPolicyId (PlutusScript headTokenScript)
  ]

assetNameFromVerificationKey :: VerificationKey PaymentKey -> AssetName
assetNameFromVerificationKey =
  AssetName . serialiseToRawBytes . verificationKeyHash

-- | Find first occurrence including a transformation.
findFirst :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findFirst fn = getFirst . foldMap (First . fn)

findHeadAssetId :: TxOut ctx -> Maybe (PolicyId, AssetName)
findHeadAssetId txOut =
  flip findFirst (valueToList $ txOutValue txOut) $ \case
    (AssetId pid aname, q)
      | aname == hydraHeadV1AssetName && q == 1 ->
          Just (pid, aname)
    _ ->
      Nothing

-- | Find (if it exists) the head identifier contained in given `TxOut`.
findStateToken :: TxOut ctx -> Maybe HeadId
findStateToken =
  fmap (mkHeadId . fst) . findHeadAssetId
