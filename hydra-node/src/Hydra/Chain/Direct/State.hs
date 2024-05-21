{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Contains the a state-ful interface to transaction construction and observation.
--
-- It defines the 'ChainStateType tx' to be used in the 'Hydra.Chain.Direct'
-- layer and it's constituents.
module Hydra.Chain.Direct.State where

import Hydra.Prelude hiding (init)

import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  AssetId (..),
  AssetName (AssetName),
  ChainPoint (..),
  CtxUTxO,
  Key (SigningKey, VerificationKey, verificationKeyHash),
  NetworkId (Mainnet, Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  PlutusScriptV2,
  PolicyId,
  Quantity (..),
  SerialiseAsRawBytes (serialiseToRawBytes),
  SlotNo (SlotNo),
  Tx,
  TxIn,
  TxOut,
  UTxO,
  UTxO' (UTxO),
  chainPointToSlotNo,
  fromPlutusScript,
  fromScriptData,
  genTxIn,
  isScriptTxOut,
  modifyTxOutValue,
  selectAsset,
  selectLovelace,
  toTxContext,
  txIns',
  txOutScriptData,
  txOutValue,
  txSpendingUTxO,
  valueFromList,
  valueToList,
  pattern ByronAddressInEra,
  pattern ShelleyAddressInEra,
  pattern TxOut,
 )
import Hydra.Chain (
  ChainStateType,
  CommitBlueprintTx (..),
  HeadParameters (..),
  IsChainState (..),
  OnChainTx (..),
  PostTxError (..),
  maxMainnetLovelace,
  maximumNumberOfParties,
 )
import Hydra.Chain.Direct.ScriptRegistry (
  ScriptRegistry (..),
  genScriptRegistry,
  registryUTxO,
 )
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (
  AbortTxError (..),
  CloseObservation (..),
  CloseTxError (..),
  ClosedThreadOutput (..),
  ClosingSnapshot (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestTxError (..),
  FanoutTxError (..),
  InitObservation (..),
  InitialThreadOutput (..),
  NotAnInitReason,
  OpenThreadOutput (..),
  UTxOHash (UTxOHash),
  abortTx,
  closeTx,
  collectComTx,
  commitTx,
  contestTx,
  decrementTx,
  fanoutTx,
  headIdToPolicyId,
  initTx,
  observeCloseTx,
  observeCollectComTx,
  observeCommitTx,
  observeInitTx,
  txInToHeadSeed,
  verificationKeyToOnChainId,
 )
import Hydra.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.ContestationPeriod qualified as ContestationPeriod
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.HeadId (HeadId (..))
import Hydra.Ledger (ChainSlot (ChainSlot), IsTx (hashUTxO))
import Hydra.Ledger.Cardano (genOneUTxOFor, genUTxOAdaOnlyOfSize, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genPointInTimeBefore, genValidityBoundsFromContestationPeriod, slotLength, systemStart)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  genConfirmedSnapshot,
  getSnapshot,
 )
import Test.QuickCheck (choose, frequency, getPositive, oneof, vector)
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Modifiers (Positive (Positive))

-- | A class for accessing the known 'UTxO' set in a type. This is useful to get
-- all the relevant UTxO for resolving transaction inputs.
class HasKnownUTxO a where
  getKnownUTxO :: a -> UTxO

-- * States & transitions

-- | The chain state used by the Hydra.Chain.Direct implementation. It records
-- the actual 'ChainState' paired with a 'ChainSlot' (used to know up to which
-- point to rewind on rollbacks).
data ChainStateAt = ChainStateAt
  { spendableUTxO :: UTxO
  , recordedAt :: Maybe ChainPoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ChainStateAt where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance IsChainState Tx where
  type ChainStateType Tx = ChainStateAt

  chainStateSlot ChainStateAt{recordedAt} =
    maybe (ChainSlot 0) chainSlotFromPoint recordedAt

-- | Get a generic 'ChainSlot' from a Cardano 'ChainPoint'. Slot 0 is used for
-- the genesis point.
chainSlotFromPoint :: ChainPoint -> ChainSlot
chainSlotFromPoint p =
  case chainPointToSlotNo p of
    Nothing -> ChainSlot 0
    Just (SlotNo s) -> ChainSlot $ fromIntegral s

-- | A definition of all transitions between 'ChainState's. Enumerable and
-- bounded to be used as labels for checking coverage.
data ChainTransition
  = Init
  | Abort
  | Commit
  | Collect
  | Decrement
  | Close
  | Contest
  | Fanout
  deriving stock (Eq, Show, Enum, Bounded)

-- | An enumeration of all possible on-chain states of a Hydra Head, where each
-- case stores the relevant information to construct & observe transactions to
-- other states.
data ChainState
  = -- | The idle state does not contain any head-specific information and exists to
    -- be used as a starting and terminal state.
    Idle
  | Initial InitialState
  | Open OpenState
  | Closed ClosedState
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ChainState where
  arbitrary = genChainState
  shrink = genericShrink

instance HasKnownUTxO ChainState where
  getKnownUTxO :: ChainState -> UTxO
  getKnownUTxO = \case
    Idle -> mempty
    Initial st -> getKnownUTxO st
    Open st -> getKnownUTxO st
    Closed st -> getKnownUTxO st

-- | Defines the starting state of the direct chain layer.
initialChainState :: ChainStateType Tx
initialChainState =
  ChainStateAt
    { spendableUTxO = mempty
    , recordedAt = Nothing
    }

-- | Read-only chain-specific data. This is different to 'HydraContext' as it
-- only contains data known to single peer.
data ChainContext = ChainContext
  { networkId :: NetworkId
  , ownVerificationKey :: VerificationKey PaymentKey
  , ownParty :: Party
  , scriptRegistry :: ScriptRegistry
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasKnownUTxO ChainContext where
  getKnownUTxO ChainContext{scriptRegistry} = registryUTxO scriptRegistry

instance Arbitrary ChainContext where
  arbitrary = do
    networkId <- Testnet . NetworkMagic <$> arbitrary
    ownVerificationKey <- genVerificationKey
    otherParties <- choose (1, maximumNumberOfParties) >>= \n -> replicateM n arbitrary
    ownParty <- elements otherParties
    scriptRegistry <- genScriptRegistry
    pure
      ChainContext
        { networkId
        , ownVerificationKey
        , ownParty
        , scriptRegistry
        }

data InitialState = InitialState
  { initialThreadOutput :: InitialThreadOutput
  , initialInitials :: [(TxIn, TxOut CtxUTxO)]
  , initialCommits :: [(TxIn, TxOut CtxUTxO)]
  , headId :: HeadId
  , seedTxIn :: TxIn
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary InitialState where
  arbitrary = do
    ctx <- genHydraContext maxGenParties
    snd <$> genStInitial ctx

  shrink = genericShrink

instance HasKnownUTxO InitialState where
  getKnownUTxO st =
    UTxO $
      Map.fromList $
        initialThreadUTxO : initialCommits <> initialInitials
   where
    InitialState
      { initialThreadOutput = InitialThreadOutput{initialThreadUTxO}
      , initialInitials
      , initialCommits
      } = st

data OpenState = OpenState
  { openThreadOutput :: OpenThreadOutput
  , headId :: HeadId
  , seedTxIn :: TxIn
  , openUtxoHash :: UTxOHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary OpenState where
  arbitrary = do
    ctx <- genHydraContext maxGenParties
    snd <$> genStOpen ctx

  shrink = genericShrink

instance HasKnownUTxO OpenState where
  getKnownUTxO st =
    UTxO.singleton openThreadUTxO
   where
    OpenState
      { openThreadOutput = OpenThreadOutput{openThreadUTxO}
      } = st

data ClosedState = ClosedState
  { closedThreadOutput :: ClosedThreadOutput
  , headId :: HeadId
  , seedTxIn :: TxIn
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ClosedState where
  arbitrary = do
    -- XXX: Untangle the whole generator mess here
    (_, st, _) <- genFanoutTx maxGenParties maxGenAssets
    pure st

  shrink = genericShrink

instance HasKnownUTxO ClosedState where
  getKnownUTxO st =
    UTxO.singleton closedThreadUTxO
   where
    ClosedState
      { closedThreadOutput = ClosedThreadOutput{closedThreadUTxO}
      } = st

-- * Constructing transactions

-- | Construct an init transaction given some general 'ChainContext', the
-- 'HeadParameters' and a seed 'TxIn' which will be spent.
initialize ::
  ChainContext ->
  -- | Seed input.
  TxIn ->
  -- | Verification key hashes of all participants.
  [OnChainId] ->
  HeadParameters ->
  Tx
initialize ctx =
  initTx networkId
 where
  ChainContext{networkId} = ctx

-- | Construct a commit transaction based on known, spendable UTxO and some
-- arbitrary UTxOs to commit. This does look for "our initial output" to spend
-- and check the given 'UTxO' to be compatible. Hence, this function does fail
-- if already committed or if the head is not initializing.
--
-- NOTE: This version of 'commit' does only commit outputs which are held by
-- payment keys. For a variant which supports committing scripts, see `commit'`.
commit ::
  ChainContext ->
  HeadId ->
  -- | Spendable 'UTxO'
  UTxO ->
  -- | 'UTxO' to commit. All outputs are assumed to be owned by public keys
  UTxO ->
  Either (PostTxError Tx) Tx
commit ctx headId spendableUTxO lookupUTxO =
  let blueprintTx = txSpendingUTxO lookupUTxO
   in commit' ctx headId spendableUTxO CommitBlueprintTx{lookupUTxO, blueprintTx}

-- | Construct a commit transaction based on known, spendable UTxO and some
-- user UTxO inputs to commit. This does look for "our initial output" to spend
-- and check the given 'UTxO' to be compatible. Hence, this function does fail
-- if already committed or if the head is not initializing.
--
-- NOTE: A simpler variant only supporting pubkey outputs is 'commit'.
commit' ::
  ChainContext ->
  HeadId ->
  -- | Spendable 'UTxO'
  UTxO ->
  CommitBlueprintTx Tx ->
  Either (PostTxError Tx) Tx
commit' ctx headId spendableUTxO commitBlueprintTx = do
  pid <- headIdToPolicyId headId ?> InvalidHeadId{headId}
  (i, o) <- ownInitial pid ?> CannotFindOwnInitial{knownUTxO = spendableUTxO}
  rejectByronAddress lookupUTxO
  rejectMoreThanMainnetLimit networkId lookupUTxO
  pure $ commitTx networkId scriptRegistry headId ownParty commitBlueprintTx (i, o, vkh)
 where
  CommitBlueprintTx{lookupUTxO} = commitBlueprintTx

  ChainContext{networkId, ownParty, scriptRegistry, ownVerificationKey} = ctx

  vkh = verificationKeyHash ownVerificationKey

  ownInitial pid =
    UTxO.find (hasMatchingPT pid . txOutValue) spendableUTxO

  hasMatchingPT pid val =
    selectAsset val (AssetId pid (AssetName (serialiseToRawBytes vkh))) == 1

rejectByronAddress :: UTxO -> Either (PostTxError Tx) ()
rejectByronAddress u = do
  forM_ u $ \case
    (TxOut (ByronAddressInEra addr) _ _ _) ->
      Left (UnsupportedLegacyOutput addr)
    (TxOut ShelleyAddressInEra{} _ _ _) ->
      Right ()

-- Rejects outputs with more than 'maxMainnetLovelace' lovelace on mainnet
-- NOTE: Remove this limit once we have more experiments on mainnet.
rejectMoreThanMainnetLimit :: NetworkId -> UTxO -> Either (PostTxError Tx) ()
rejectMoreThanMainnetLimit network u = do
  when (network == Mainnet && lovelaceAmt > maxMainnetLovelace) $
    Left $
      CommittedTooMuchADAForMainnet lovelaceAmt maxMainnetLovelace
 where
  lovelaceAmt = foldMap (selectLovelace . txOutValue) u

-- | Construct a abort transaction based on known, spendable UTxO. This function
-- looks for head, initial and commit outputs to spend and it will fail if we
-- can't find the head output.
abort ::
  ChainContext ->
  -- | Seed TxIn
  TxIn ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Committed UTxOs to reimburse.
  UTxO ->
  Either AbortTxError Tx
abort ctx seedTxIn spendableUTxO committedUTxO = do
  headUTxO <-
    maybe (Left CannotFindHeadOutputToAbort) pure $
      UTxO.find (isScriptTxOut headScript) utxoOfThisHead'

  abortTx committedUTxO scriptRegistry ownVerificationKey headUTxO headTokenScript initials commits
 where
  utxoOfThisHead' = utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO

  initials =
    UTxO.toMap $ UTxO.filter (isScriptTxOut initialScript) utxoOfThisHead'

  commits =
    UTxO.toMap $ UTxO.filter (isScriptTxOut commitScript) utxoOfThisHead'

  commitScript = fromPlutusScript @PlutusScriptV2 Commit.validatorScript

  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  initialScript = fromPlutusScript @PlutusScriptV2 Initial.validatorScript

  headTokenScript = mkHeadTokenScript seedTxIn

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

data CollectTxError
  = InvalidHeadIdInCollect {headId :: HeadId}
  | CannotFindHeadOutputToCollect
  deriving stock (Show)

-- | Construct a collect transaction based on known, spendable UTxO. This
-- function looks for head output and commit outputs to spend and it will fail
-- if we can't find the head output.
collect ::
  ChainContext ->
  HeadId ->
  HeadParameters ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  Either CollectTxError Tx
collect ctx headId headParameters utxoToCollect spendableUTxO = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInCollect{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO <- UTxO.find (isScriptTxOut headScript) utxoOfThisHead' ?> CannotFindHeadOutputToCollect
  let commits = UTxO.toMap $ UTxO.filter (isScriptTxOut commitScript) utxoOfThisHead'
  pure $
    collectComTx networkId scriptRegistry ownVerificationKey headId headParameters headUTxO commits utxoToCollect
 where
  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  commitScript = fromPlutusScript @PlutusScriptV2 Commit.validatorScript

  ChainContext{networkId, ownVerificationKey, scriptRegistry} = ctx

-- | Possible errors when trying to construct decrement tx
data DecrementTxError
  = InvalidHeadIdInDecrement {headId :: HeadId}
  | CannotFindHeadOutputInDecrement
  | SnapshotMissingDecrementUTxO
  deriving stock (Show)

decrement ::
  ChainContext ->
  HeadId ->
  HeadParameters ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Confirmed Snapshot
  Snapshot Tx ->
  MultiSignature (Snapshot Tx) ->
  Either DecrementTxError Tx
decrement ctx headId headParameters spendableUTxO snapshot signatures = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInDecrement{headId}
  let utxoOfThisHead' = utxoOfThisHead pid spendableUTxO
  headUTxO <- UTxO.find (isScriptTxOut headScript) utxoOfThisHead' ?> CannotFindHeadOutputInDecrement
  -- case utxoToDecommit of
  --   Nothing ->
  --     Left SnapshotMissingDecrementUTxO
  --   Just _decrementUTxO ->
  Right $ decrementTx scriptRegistry ownVerificationKey headId headParameters headUTxO snapshot signatures
 where
  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  ChainContext{ownVerificationKey, scriptRegistry} = ctx
  Snapshot{utxoToDecommit} = snapshot

-- | Construct a close transaction spending the head output in given 'UTxO',
-- head parameters, and a confirmed snapshot. NOTE: Lower and upper bound slot
-- difference should not exceed contestation period.
close ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  HeadParameters ->
  ConfirmedSnapshot Tx ->
  -- | 'Tx' validity lower bound
  SlotNo ->
  -- | 'Tx' validity upper bound
  PointInTime ->
  Either CloseTxError Tx
close ctx spendableUTxO headId HeadParameters{parties, contestationPeriod} confirmedSnapshot startSlotNo pointInTime = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInClose{headId}
  headUTxO <-
    UTxO.find (isScriptTxOut headScript) (utxoOfThisHead pid spendableUTxO)
      ?> CannotFindHeadOutputToClose
  let openThreadOutput =
        OpenThreadOutput
          { openThreadUTxO = headUTxO
          , openContestationPeriod = ContestationPeriod.toChain contestationPeriod
          , openParties = partyToChain <$> parties
          }
  pure $ closeTx scriptRegistry ownVerificationKey closingSnapshot startSlotNo pointInTime openThreadOutput headId
 where
  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  closingSnapshot = case confirmedSnapshot of
    InitialSnapshot{initialUTxO} -> CloseWithInitialSnapshot{openUtxoHash = UTxOHash $ hashUTxO @Tx initialUTxO}
    ConfirmedSnapshot{snapshot = Snapshot{number, utxo, utxoToDecommit}, signatures} ->
      CloseWithConfirmedSnapshot
        { snapshotNumber = number
        , closeUtxoHash = UTxOHash $ hashUTxO @Tx utxo
        , closeUtxoToDecommitHash =
            UTxOHash $ hashUTxO @Tx $ fromMaybe mempty utxoToDecommit
        , signatures
        }

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

-- | Construct a contest transaction based on the 'ClosedState' and a confirmed
-- snapshot. The given 'PointInTime' will be used as an upper validity bound and
-- needs to be before the deadline.
contest ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  ContestationPeriod ->
  ConfirmedSnapshot Tx ->
  -- | Current slot and posix time to be used as the contestation time.
  PointInTime ->
  Either ContestTxError Tx
contest ctx spendableUTxO headId contestationPeriod confirmedSnapshot pointInTime = do
  pid <- headIdToPolicyId headId ?> InvalidHeadIdInContest{headId}
  headUTxO <-
    UTxO.find (isScriptTxOut headScript) (utxoOfThisHead pid spendableUTxO)
      ?> CannotFindHeadOutputToContest
  closedThreadOutput <- checkHeadDatum headUTxO
  pure $ contestTx scriptRegistry ownVerificationKey sn sigs pointInTime closedThreadOutput headId contestationPeriod
 where
  checkHeadDatum headUTxO@(_, headOutput) = do
    headDatum <- txOutScriptData (toTxContext headOutput) ?> MissingHeadDatumInContest
    datum <- fromScriptData headDatum ?> FailedToConvertFromScriptDataInContest

    case datum of
      Head.Closed{contesters, parties, contestationDeadline} -> do
        let closedThreadUTxO = headUTxO
            closedParties = parties
            closedContestationDeadline = contestationDeadline
            closedContesters = contesters
        pure $
          ClosedThreadOutput
            { closedThreadUTxO
            , closedParties
            , closedContestationDeadline
            , closedContesters
            }
      _ -> Left WrongDatumInContest

  (sn, sigs) =
    case confirmedSnapshot of
      ConfirmedSnapshot{signatures} -> (getSnapshot confirmedSnapshot, signatures)
      _ -> (getSnapshot confirmedSnapshot, mempty)

  ChainContext{ownVerificationKey, scriptRegistry} = ctx

  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

-- | Construct a fanout transaction based on the 'ClosedState' and off-chain
-- agreed 'UTxO' set to fan out.
fanout ::
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | Snapshot UTxO to decommit to fanout
  UTxO ->
  -- | Snapshot UTxO to fanout
  Maybe UTxO ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Either FanoutTxError Tx
fanout ctx spendableUTxO seedTxIn utxo utxoToDecommit deadlineSlotNo = do
  headUTxO <-
    UTxO.find (isScriptTxOut headScript) (utxoOfThisHead (headPolicyId seedTxIn) spendableUTxO)
      ?> CannotFindHeadOutputToFanout

  closedThreadUTxO <- checkHeadDatum headUTxO

  pure $ fanoutTx scriptRegistry utxo utxoToDecommit closedThreadUTxO deadlineSlotNo headTokenScript
 where
  headTokenScript = mkHeadTokenScript seedTxIn

  ChainContext{scriptRegistry} = ctx

  headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript

  checkHeadDatum headUTxO@(_, headOutput) = do
    headDatum <-
      txOutScriptData (toTxContext headOutput) ?> MissingHeadDatumInFanout
    datum <-
      fromScriptData headDatum ?> FailedToConvertFromScriptDataInFanout

    case datum of
      Head.Closed{} -> pure headUTxO
      _ -> Left WrongDatumInFanout

-- * Helpers

utxoOfThisHead :: PolicyId -> UTxO -> UTxO
utxoOfThisHead policy = UTxO.filter hasHeadToken
 where
  hasHeadToken =
    isJust . find isHeadToken . valueToList . txOutValue

  isHeadToken (assetId, quantity) =
    case assetId of
      AdaAssetId -> False
      AssetId pid _ -> pid == policy && quantity == 1

-- * Observing Transitions

-- ** IdleState transitions

-- TODO: This function is not really used anymore (only from
-- 'unsafeObserveInit'). In general, most functions here are actually not used
-- from the "production code", but only to generate test cases and benchmarks.

-- | Observe an init transition using a 'InitialState' and 'observeInitTx'.
observeInit ::
  ChainContext ->
  [VerificationKey PaymentKey] ->
  Tx ->
  Either NotAnInitReason (OnChainTx Tx, InitialState)
observeInit _ctx _allVerificationKeys tx = do
  observation <- observeInitTx tx
  pure (toEvent observation, toState observation)
 where
  toEvent InitObservation{contestationPeriod, parties, headId, seedTxIn, participants} =
    OnInitTx
      { headId
      , headSeed = txInToHeadSeed seedTxIn
      , headParameters = HeadParameters{contestationPeriod, parties}
      , participants
      }

  toState InitObservation{initialThreadUTxO, parties, contestationPeriod, initials, headId, seedTxIn} =
    InitialState
      { initialThreadOutput =
          InitialThreadOutput
            { initialThreadUTxO
            , initialParties = partyToChain <$> parties
            , initialContestationPeriod = toChain contestationPeriod
            }
      , initialInitials = initials
      , initialCommits = mempty
      , headId
      , seedTxIn
      }

-- ** InitialState transitions

-- | Observe an commit transition using a 'InitialState' and 'observeCommitTx'.
observeCommit ::
  ChainContext ->
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, InitialState)
observeCommit ctx st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCommitTx networkId utxo tx
  let CommitObservation{commitOutput, party, committed, headId = commitHeadId} = observation
  guard $ commitHeadId == headId
  let event = OnCommitTx{headId, party, committed}
  let st' =
        st
          { initialInitials =
              -- NOTE: A commit tx has been observed and thus we can
              -- remove all it's inputs from our tracked initials
              filter ((`notElem` txIns' tx) . fst) initialInitials
          , initialCommits =
              commitOutput : initialCommits
          }
  pure (event, st')
 where
  ChainContext{networkId} = ctx

  InitialState
    { initialCommits
    , initialInitials
    , headId
    } = st

-- | Observe an collect transition using a 'InitialState' and 'observeCollectComTx'.
-- This function checks the head id and ignores if not relevant.
observeCollect ::
  InitialState ->
  Tx ->
  Maybe (OnChainTx Tx, OpenState)
observeCollect st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCollectComTx utxo tx
  let CollectComObservation{threadOutput = threadOutput, headId = collectComHeadId, utxoHash} = observation
  guard (headId == collectComHeadId)
  -- REVIEW: is it enough to pass here just the 'openThreadUTxO' or we need also
  -- the known utxo (getKnownUTxO st)?
  let event = OnCollectComTx{headId}
  let st' =
        OpenState
          { openThreadOutput = threadOutput
          , headId
          , seedTxIn
          , openUtxoHash = utxoHash
          }
  pure (event, st')
 where
  InitialState
    { headId
    , seedTxIn
    } = st

-- ** OpenState transitions

-- | Observe a close transition using a 'OpenState' and 'observeCloseTx'.
-- This function checks the head id and ignores if not relevant.
observeClose ::
  OpenState ->
  Tx ->
  Maybe (OnChainTx Tx, ClosedState)
observeClose st tx = do
  let utxo = getKnownUTxO st
  observation <- observeCloseTx utxo tx
  let CloseObservation{threadOutput, headId = closeObservationHeadId, snapshotNumber} = observation
  guard (headId == closeObservationHeadId)
  let ClosedThreadOutput{closedContestationDeadline} = threadOutput
  let event =
        OnCloseTx
          { headId = closeObservationHeadId
          , snapshotNumber
          , contestationDeadline = posixToUTCTime closedContestationDeadline
          }
  let st' =
        ClosedState
          { closedThreadOutput = threadOutput
          , headId
          , seedTxIn
          }
  pure (event, st')
 where
  OpenState
    { headId
    , seedTxIn
    } = st

-- * Generators

-- | Maximum number of parties used in the generators.
maxGenParties :: Int
maxGenParties = 3

-- | Maximum number of assets (ADA or other tokens) used in the generators.
maxGenAssets :: Int
maxGenAssets = 70

-- | Generate a 'ChainState' within known limits above.
genChainState :: Gen ChainState
genChainState =
  oneof
    [ pure Idle
    , Initial <$> arbitrary
    , Open <$> arbitrary
    , Closed <$> arbitrary
    ]

-- | Generate a 'ChainContext' and 'ChainState' within the known limits above, along with a
-- transaction that results in a transition away from it.
genChainStateWithTx :: Gen (ChainContext, ChainState, Tx, ChainTransition)
genChainStateWithTx =
  oneof
    [ genInitWithState
    , genAbortWithState
    , genCommitWithState
    , genDecrementWithState
    , genCollectWithState
    , genCloseWithState
    , genContestWithState
    , genFanoutWithState
    ]
 where
  genInitWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genInitWithState = do
    ctx <- genHydraContext maxGenParties
    cctx <- pickChainContext ctx
    seedInput <- genTxIn
    let tx = initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
    pure (cctx, Idle, tx, Init)

  genAbortWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genAbortWithState = do
    ctx <- genHydraContext maxGenParties
    (cctx, stInitial) <- genStInitial ctx
    -- TODO: also generate sometimes aborts with utxo
    let utxo = getKnownUTxO stInitial
        InitialState{seedTxIn} = stInitial
        tx = unsafeAbort cctx seedTxIn utxo mempty
    pure (cctx, Initial stInitial, tx, Abort)

  genCommitWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genCommitWithState = do
    ctx <- genHydraContext maxGenParties
    (cctx, stInitial) <- genStInitial ctx
    utxo <- genCommit
    let InitialState{headId} = stInitial
    let tx = unsafeCommit cctx headId (getKnownUTxO stInitial) utxo
    pure (cctx, Initial stInitial, tx, Commit)

  genCollectWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genCollectWithState = do
    (ctx, _, st, tx) <- genCollectComTx
    pure (ctx, Initial st, tx, Collect)

  genDecrementWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genDecrementWithState = do
    (ctx, st, tx) <- genDecrementTx maxGenParties
    pure (ctx, Open st, tx, Decrement)

  genCloseWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genCloseWithState = do
    (ctx, st, tx, _) <- genCloseTx maxGenParties
    pure (ctx, Open st, tx, Close)

  genContestWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genContestWithState = do
    (hctx, _, st, tx) <- genContestTx
    ctx <- pickChainContext hctx
    pure (ctx, Closed st, tx, Contest)

  genFanoutWithState :: Gen (ChainContext, ChainState, Tx, ChainTransition)
  genFanoutWithState = do
    Positive numParties <- arbitrary
    Positive numOutputs <- arbitrary
    (hctx, st, tx) <- genFanoutTx numParties numOutputs
    ctx <- pickChainContext hctx
    pure (ctx, Closed st, tx, Fanout)

-- ** Warning zone

-- | Define some 'global' context from which generators can pick
-- values for generation. This allows to write fairly independent generators
-- which however still make sense with one another within the context of a head.
--
-- For example, one can generate a head's _party_ from that global list, whereas
-- other functions may rely on all parties and thus, we need both generation to
-- be coherent.
--
-- Do not use this in production code, but only for generating test data.
data HydraContext = HydraContext
  { ctxVerificationKeys :: [VerificationKey PaymentKey]
  , ctxHydraSigningKeys :: [SigningKey HydraKey]
  , ctxNetworkId :: NetworkId
  , ctxContestationPeriod :: ContestationPeriod
  , ctxScriptRegistry :: ScriptRegistry
  }
  deriving stock (Show)

ctxParties :: HydraContext -> [Party]
ctxParties = fmap deriveParty . ctxHydraSigningKeys

ctxParticipants :: HydraContext -> [OnChainId]
ctxParticipants = map verificationKeyToOnChainId . ctxVerificationKeys

ctxHeadParameters ::
  HydraContext ->
  HeadParameters
ctxHeadParameters ctx@HydraContext{ctxContestationPeriod} =
  HeadParameters ctxContestationPeriod (ctxParties ctx)

-- | Generate a `HydraContext` for a arbitrary number of parties, bounded by
-- given maximum.
genHydraContext :: Int -> Gen HydraContext
genHydraContext maxParties = choose (1, maxParties) >>= genHydraContextFor

-- | Generate a 'HydraContext' for a given number of parties.
genHydraContextFor :: Int -> Gen HydraContext
genHydraContextFor n = do
  ctxVerificationKeys <- replicateM n genVerificationKey
  ctxHydraSigningKeys <- vector n
  ctxNetworkId <- Testnet . NetworkMagic <$> arbitrary
  ctxContestationPeriod <- arbitrary
  ctxScriptRegistry <- genScriptRegistry
  pure $
    HydraContext
      { ctxVerificationKeys
      , ctxHydraSigningKeys
      , ctxNetworkId
      , ctxContestationPeriod
      , ctxScriptRegistry
      }

-- | Get all peer-specific 'ChainContext's from a 'HydraContext'. NOTE: This
-- assumes that 'HydraContext' has same length 'ctxVerificationKeys' and
-- 'ctxHydraSigningKeys'.
-- XXX: This is actually a non-monadic function.
deriveChainContexts :: HydraContext -> Gen [ChainContext]
deriveChainContexts ctx = do
  pure $
    flip map (zip ctxVerificationKeys allParties') $ \(vk, p) ->
      ChainContext
        { networkId = ctxNetworkId
        , ownVerificationKey = vk
        , ownParty = p
        , scriptRegistry = ctxScriptRegistry
        }
 where
  allParties' = ctxParties ctx

  HydraContext
    { ctxVerificationKeys
    , ctxNetworkId
    , ctxScriptRegistry
    } = ctx

-- | Pick one of the participants and derive the peer-specific 'ChainContext'
-- from a 'HydraContext'. NOTE: This assumes that 'HydraContext' has same length
-- 'ctxVerificationKeys' and 'ctxHydraSigningKeys'.
pickChainContext :: HydraContext -> Gen ChainContext
pickChainContext ctx =
  deriveChainContexts ctx >>= elements

genStInitial ::
  HydraContext ->
  Gen (ChainContext, InitialState)
genStInitial ctx = do
  seedInput <- genTxIn
  cctx <- pickChainContext ctx
  let txInit = initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)
  let initState = unsafeObserveInit cctx (ctxVerificationKeys ctx) txInit
  pure (cctx, initState)

genInitTx ::
  HydraContext ->
  Gen Tx
genInitTx ctx = do
  cctx <- pickChainContext ctx
  seedInput <- genTxIn
  pure $ initialize cctx seedInput (ctxParticipants ctx) (ctxHeadParameters ctx)

genCommits ::
  HydraContext ->
  Tx ->
  Gen [Tx]
genCommits =
  genCommits' genCommit

genCommits' ::
  Gen UTxO ->
  HydraContext ->
  Tx ->
  Gen [Tx]
genCommits' genUTxO ctx txInit = do
  -- Prepare UTxO to commit. We need to scale down the quantities by number of
  -- committed UTxOs to ensure we are not as easily hitting overflows of the max
  -- bound (Word64) when collecting all the commits together later.
  commitUTxOs <- forM (ctxParties ctx) $ const genUTxO
  let scaledCommitUTxOs = scaleCommitUTxOs commitUTxOs

  allChainContexts <- deriveChainContexts ctx
  forM (zip allChainContexts scaledCommitUTxOs) $ \(cctx, toCommit) -> do
    let stInitial@InitialState{headId} = unsafeObserveInit cctx (ctxVerificationKeys ctx) txInit
    pure $ unsafeCommit cctx headId (getKnownUTxO stInitial) toCommit
 where
  scaleCommitUTxOs commitUTxOs =
    let numberOfUTxOs = length $ fold commitUTxOs
     in map (fmap (modifyTxOutValue (scaleQuantitiesDownBy numberOfUTxOs))) commitUTxOs

  scaleQuantitiesDownBy x =
    valueFromList . map (\(an, Quantity q) -> (an, Quantity $ q `div` fromIntegral x)) . valueToList

genCommitFor :: VerificationKey PaymentKey -> Gen UTxO
genCommitFor vkey =
  frequency
    [ (1, pure mempty)
    , (10, genOneUTxOFor vkey)
    ]

genCommit :: Gen UTxO
genCommit =
  frequency
    [ (1, pure mempty)
    , (10, genVerificationKey >>= genOneUTxOFor)
    ]

genCollectComTx :: Gen (ChainContext, [UTxO], InitialState, Tx)
genCollectComTx = do
  ctx <- genHydraContextFor maximumNumberOfParties
  txInit <- genInitTx ctx
  commits <- genCommits ctx txInit
  cctx <- pickChainContext ctx
  let (committedUTxO, stInitialized) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) txInit commits
  let InitialState{headId} = stInitialized
  let utxoToCollect = fold committedUTxO
  let spendableUTxO = getKnownUTxO stInitialized
  pure (cctx, committedUTxO, stInitialized, unsafeCollect cctx headId (ctxHeadParameters ctx) utxoToCollect spendableUTxO)

genDecrementTx :: Int -> Gen (ChainContext, OpenState, Tx)
genDecrementTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  cctx <- pickChainContext ctx
  snapshot <- do
    number <- getPositive <$> arbitrary
    (utxo, toDecommit) <- splitUTxO u0
    pure Snapshot{headId, number, confirmed = [], utxo, utxoToDecommit = Just toDecommit}
  let signatures = aggregate $ fmap (`sign` snapshot) (ctxHydraSigningKeys ctx)
  let openUTxO = getKnownUTxO stOpen
  pure (cctx, stOpen, unsafeDecrement cctx headId (ctxHeadParameters ctx) openUTxO snapshot signatures)

splitUTxO :: UTxO -> Gen (UTxO, UTxO)
splitUTxO utxo = do
  -- NOTE: here we skip the head output which is always at the first position.
  ix <- choose (1, length utxo)
  let (p1, p2) = splitAt ix (UTxO.pairs utxo)
  pure (UTxO.fromPairs p1, UTxO.fromPairs p2)

genCloseTx :: Int -> Gen (ChainContext, OpenState, Tx, ConfirmedSnapshot Tx)
genCloseTx numParties = do
  ctx <- genHydraContextFor numParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  (confirmedUtxo, utxoToDecommit) <- splitUTxO u0
  snapshot <- genConfirmedSnapshot headId 1 confirmedUtxo (Just utxoToDecommit) (ctxHydraSigningKeys ctx)
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  let utxo = getKnownUTxO stOpen
  pure (cctx, stOpen, unsafeClose cctx utxo headId (ctxHeadParameters ctx) snapshot startSlot pointInTime, snapshot)

genContestTx :: Gen (HydraContext, PointInTime, ClosedState, Tx)
genContestTx = do
  ctx <- genHydraContextFor maximumNumberOfParties
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  (confirmedUtXO, utxoToDecommit) <- splitUTxO u0
  confirmed <- genConfirmedSnapshot headId 1 confirmedUtXO (Just utxoToDecommit) []
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, closePointInTime) <- genValidityBoundsFromContestationPeriod cp
  let openUTxO = getKnownUTxO stOpen
  let txClose = unsafeClose cctx openUTxO headId (ctxHeadParameters ctx) confirmed startSlot closePointInTime
  let stClosed = snd $ fromJust $ observeClose stOpen txClose
  let utxo = getKnownUTxO stClosed
  someUtxo <- arbitrary
  (confirmedUTxO', utxoToDecommit') <- splitUTxO someUtxo
  contestSnapshot <- genConfirmedSnapshot headId (succ $ number $ getSnapshot confirmed) confirmedUTxO' (Just utxoToDecommit') (ctxHydraSigningKeys ctx)
  contestPointInTime <- genPointInTimeBefore (getContestationDeadline stClosed)
  pure (ctx, closePointInTime, stClosed, unsafeContest cctx utxo headId cp contestSnapshot contestPointInTime)

genFanoutTx :: Int -> Int -> Gen (HydraContext, ClosedState, Tx)
genFanoutTx numParties numOutputs = do
  ctx <- genHydraContext numParties
  utxo <- genUTxOAdaOnlyOfSize numOutputs
  (_, toFanout, stClosed@ClosedState{seedTxIn}) <- genStClosed ctx utxo
  cctx <- pickChainContext ctx
  let deadlineSlotNo = slotNoFromUTCTime systemStart slotLength (getContestationDeadline stClosed)
      spendableUTxO = getKnownUTxO stClosed
  -- TODO: generate UTxO to decommit here too
  pure (ctx, stClosed, unsafeFanout cctx spendableUTxO seedTxIn toFanout Nothing deadlineSlotNo)

getContestationDeadline :: ClosedState -> UTCTime
getContestationDeadline
  ClosedState{closedThreadOutput = ClosedThreadOutput{closedContestationDeadline}} =
    posixToUTCTime closedContestationDeadline

genStOpen ::
  HydraContext ->
  Gen (UTxO, OpenState)
genStOpen ctx = do
  txInit <- genInitTx ctx
  commits <- genCommits ctx txInit
  cctx <- pickChainContext ctx
  let (committed, stInitial) = unsafeObserveInitAndCommits cctx (ctxVerificationKeys ctx) txInit commits
  let InitialState{headId} = stInitial
  let utxoToCollect = fold committed
  let spendableUTxO = getKnownUTxO stInitial
  let txCollect = unsafeCollect cctx headId (ctxHeadParameters ctx) utxoToCollect spendableUTxO
  pure (fold committed, snd . fromJust $ observeCollect stInitial txCollect)

genStClosed ::
  HydraContext ->
  UTxO ->
  Gen (SnapshotNumber, UTxO, ClosedState)
genStClosed ctx utxo = do
  (u0, stOpen@OpenState{headId}) <- genStOpen ctx
  confirmed <- arbitrary
  let (sn, snapshot, toFanout) = case confirmed of
        InitialSnapshot{} ->
          ( 0
          , InitialSnapshot{headId, initialUTxO = u0}
          , u0
          )
        ConfirmedSnapshot{snapshot = snap, signatures} ->
          ( number snap
          , ConfirmedSnapshot
              { snapshot = snap{utxo = utxo}
              , signatures
              }
          , utxo
          )
  cctx <- pickChainContext ctx
  let cp = ctxContestationPeriod ctx
  (startSlot, pointInTime) <- genValidityBoundsFromContestationPeriod cp
  let utxo' = getKnownUTxO stOpen
  let txClose = unsafeClose cctx utxo' headId (ctxHeadParameters ctx) snapshot startSlot pointInTime
  pure (sn, toFanout, snd . fromJust $ observeClose stOpen txClose)

-- ** Danger zone

unsafeCommit ::
  HasCallStack =>
  ChainContext ->
  HeadId ->
  -- | Spendable 'UTxO'
  UTxO ->
  -- | 'UTxO' to commit. All outputs are assumed to be owned by public keys.
  UTxO ->
  Tx
unsafeCommit ctx headId spendableUTxO utxoToCommit =
  either (error . show) id $ commit ctx headId spendableUTxO utxoToCommit

unsafeAbort ::
  HasCallStack =>
  ChainContext ->
  -- | Seed TxIn
  TxIn ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Committed UTxOs to reimburse.
  UTxO ->
  Tx
unsafeAbort ctx seedTxIn spendableUTxO committedUTxO =
  either (error . show) id $ abort ctx seedTxIn spendableUTxO committedUTxO

unsafeDecrement ::
  HasCallStack =>
  ChainContext ->
  HeadId ->
  HeadParameters ->
  -- | Spendable 'UTxO'
  UTxO ->
  Snapshot Tx ->
  MultiSignature (Snapshot Tx) ->
  Tx
unsafeDecrement ctx headId parameters spendableUTxO snapshot signatures =
  either (error . show) id $ decrement ctx headId parameters spendableUTxO snapshot signatures

unsafeClose ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  HeadParameters ->
  ConfirmedSnapshot Tx ->
  -- | 'Tx' validity lower bound
  SlotNo ->
  -- | 'Tx' validity upper bound
  PointInTime ->
  Tx
unsafeClose ctx spendableUTxO headId headParameters confirmedSnapshot startSlotNo pointInTime =
  either (error . show) id $ close ctx spendableUTxO headId headParameters confirmedSnapshot startSlotNo pointInTime

unsafeCollect ::
  ChainContext ->
  HeadId ->
  HeadParameters ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  Tx
unsafeCollect ctx headId headParameters utxoToCollect spendableUTxO =
  either (error . show) id $ collect ctx headId headParameters utxoToCollect spendableUTxO

unsafeContest ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  HeadId ->
  ContestationPeriod ->
  ConfirmedSnapshot Tx ->
  PointInTime ->
  Tx
unsafeContest ctx spendableUTxO headId contestationPeriod confirmedSnapshot pointInTime =
  either (error . show) id $ contest ctx spendableUTxO headId contestationPeriod confirmedSnapshot pointInTime

unsafeFanout ::
  HasCallStack =>
  ChainContext ->
  -- | Spendable UTxO containing head, initial and commit outputs
  UTxO ->
  -- | Seed TxIn
  TxIn ->
  -- | Snapshot UTxO to fanout
  UTxO ->
  -- | Snapshot decommit UTxO to fanout
  Maybe UTxO ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Tx
unsafeFanout ctx spendableUTxO seedTxIn utxo utxoToDecommit deadlineSlotNo =
  either (error . show) id $ fanout ctx spendableUTxO seedTxIn utxo utxoToDecommit deadlineSlotNo

unsafeObserveInit ::
  HasCallStack =>
  ChainContext ->
  [VerificationKey PaymentKey] ->
  Tx ->
  InitialState
unsafeObserveInit cctx txInit allVerificationKeys =
  case observeInit cctx txInit allVerificationKeys of
    Left err -> error $ "Did not observe an init tx: " <> show err
    Right st -> snd st

-- REVIEW: Maybe it would be more convenient if 'unsafeObserveInitAndCommits'
-- returns just 'UTXO' instead of [UTxO]
unsafeObserveInitAndCommits ::
  HasCallStack =>
  ChainContext ->
  [VerificationKey PaymentKey] ->
  Tx ->
  [Tx] ->
  ([UTxO], InitialState)
unsafeObserveInitAndCommits ctx allVerificationKeys txInit commits =
  (utxo, stInitial')
 where
  stInitial = unsafeObserveInit ctx allVerificationKeys txInit

  (utxo, stInitial') = flip runState stInitial $ do
    forM commits $ \txCommit -> do
      st <- get
      let (event, st') = fromJust $ observeCommit ctx st txCommit
      put st'
      pure $ case event of
        OnCommitTx{committed} -> committed
        _ -> mempty
