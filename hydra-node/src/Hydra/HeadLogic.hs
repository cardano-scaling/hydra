{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Implements the Head Protocol's /state machine/ as /pure functions/ in an event sourced manner.
--
-- More specifically, the 'update' will handle 'Input's (or rather "commands" in
-- event sourcing speak) and convert that into a list of side-'Effect's and
-- 'StateChanged' events, which in turn are applied via 'aggregateNodeState' into
-- a single 'NodeState'.
--
-- As the specification is using a more imperative way of specifying the protocol
-- behavior, one would find the decision logic in 'updateProgram' while state updates
-- can be found in the corresponding 'applyEvent' branch.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Outcome,
) where

import Hydra.Prelude

import Data.List (elemIndex, minimumBy)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set ((\\))
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..), validateClientInput)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  ChainEvent (..),
  ChainStateHistory,
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
  initHistory,
  pushNewState,
  rollbackHistory,
  setLastKnown,
 )
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  causes,
  changes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.Program (AcknowledgementProgram, HeadProgram, SnapshotProgram, TransactionProgram)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FinalizedSnapshot (..),
  HeadState (..),
  IdleState (IdleState, chainState),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  getChainState,
  isCollectingAcks,
  mkSeenSnapshot,
  seenSnapshotNumber,
  setChainState,
  snapshotInFlight,
 )
import Hydra.HeadLogic.Version qualified as Version
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, SyncedStatus (..), consumeDeposit, depositsForHead, recordDeposit, rollbackDeposits, syncedStatus, updateDeposit)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..))
import Hydra.Tx (
  HeadId,
  HeadSeed,
  IsTx (..),
  TxIdType,
  UTxOType,
  combinedUTxO,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (
  MultiSignature,
  Signature,
  Verified (..),
  aggregateInOrder,
  getSignableRepresentation,
 )
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

-- * The Coordinated Head protocol

-- | Maximum number of transaction ids per snapshot. This effectively limits our
-- "block size" and ensures it does not grow arbitrarily with the backlog of
-- pending transactions (localTxs). Only applied when requesting snapshots as
-- a leader; followers accept larger requests, so this can change without a
-- coordinated upgrade.
--
-- 4000 was chosen from backlog-drain sweeps at caps 1000 to 8000 (see
-- hydra-cluster/bench/BASELINES.md): per-round costs scale with the backlog,
-- so under sustained overload larger caps drain strictly faster (2.7x at
-- 4000 vs 1000 on a 24k-tx backlog) with lower confirmation latency and
-- lower peak memory, while the cap never binds under light load. Gains taper
-- beyond 4000 and worst-case round size keeps growing (ReqSn bytes, failed
-- round rework, follower reapply stall), so we stop at roughly 10x below
-- etcd's 1.5MiB request limit, which a too-large ReqSn would wedge on.
maxTxsPerSnapshot :: Int
maxTxsPerSnapshot = 4000

-- ** On-Chain Protocol

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  HeadProgram tx ()
onIdleClientInit env =
  Program.emit OnChainEffect{postChainTx = InitTx{participants, headParameters}}
 where
  headParameters = mkHeadParameters env

  Environment{participants} = env

-- | Observe an init transaction and initialize parameters in an 'OpenState'.
--
-- __Transition__: 'IdleState' → 'OpenState'
onIdleChainInitTx ::
  Environment ->
  -- | New chain state.
  ChainStateType tx ->
  HeadId ->
  HeadSeed ->
  HeadParameters ->
  [OnChainId] ->
  HeadProgram tx ()
onIdleChainInitTx env newChainState headId headSeed headParameters participants = decision
 where
  decision
    | configuredParties == initializedParties
        && party `member` initializedParties
        && configuredContestationPeriod == contestationPeriod
        && configuredDepositPeriod == depositPeriod
        && Set.fromList configuredParticipants == Set.fromList participants =
        Program.change
          HeadOpened
            { parameters = headParameters
            , chainState = newChainState
            , headId
            , headSeed
            , parties
            }
    | otherwise =
        Program.change
          IgnoredHeadInitializing
            { headId
            , contestationPeriod
            , parties
            , participants
            }
  initializedParties = Set.fromList parties

  configuredParties = Set.fromList (party : otherParties)

  HeadParameters{parties, contestationPeriod, depositPeriod} = headParameters

  Environment
    { party
    , otherParties
    , contestationPeriod = configuredContestationPeriod
    , depositPeriod = configuredDepositPeriod
    , participants = configuredParticipants
    } = env

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  -- | The transaction to be submitted to the head.
  tx ->
  HeadProgram tx ()
onOpenClientNewTx tx =
  Program.emit . NetworkEffect $
    ReqTx tx

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  forall tx.
  IsTx tx =>
  Environment ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  HeadProgram tx ()
onOpenNetworkReqTx env chainTime st ttl pendingDeposits tx =
  Program.promoteProgram $ onOpenNetworkReqTxProgram (ruleEnvironment env) chainTime st ttl pendingDeposits tx

-- | Public inputs used by the interpreted rules. Signing keys belong only to
-- the interpreter configuration supplied by the production wrappers.
data RuleEnvironment = RuleEnvironment
  { party :: Party
  , depositPeriod :: DepositPeriod
  }

ruleEnvironment :: Environment -> RuleEnvironment
ruleEnvironment Environment{party, depositPeriod} = RuleEnvironment{party, depositPeriod}

-- | The actual transaction rule as an inspectable program. Interpreters supply
-- ledger application; collision checks, retry decisions, and snapshot selection
-- belong to this rule. The event format remains compatible with existing replay.
onOpenNetworkReqTxProgram ::
  forall tx.
  IsTx tx =>
  RuleEnvironment ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  tx ->
  TransactionProgram tx ()
onOpenNetworkReqTxProgram env ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx = do
  alreadyApplied <-
    Program.decide "transaction is already applied locally" $
      Map.member (txId tx) allTxs && any ((== txId tx) . txId) localTxs
  unless alreadyApplied $ do
    Program.change TransactionReceived{tx}
    let outputCheck = checkTransactionOutputs localUTxO tx
    void $ Program.decide "outputs do not collide with the local UTxO" (isRight outputCheck)
    result <-
      case outputCheck of
        Right () -> Program.applyTransaction currentSlot localUTxO tx
        Left err -> pure $ Left err
    case result of
      Left err -> notApplicable err
      Right after -> do
        freshDepositOutputs <-
          Program.decide "outputs do not collide with pending deposits" $
            namesNoneOf (utxoFromTx tx) namedByDepositsInPlay
        if freshDepositOutputs
          then do
            Program.recordAppliedTransaction headId tx localUTxO after
            requestSnapshot
          else notApplicable (ValidationError "transaction creates an output a pending deposit names")
 where
  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty

  notApplicable :: ValidationError -> TransactionProgram tx ()
  notApplicable err = do
    canRetry <- Program.decide "retry budget remains" (ttl > 0)
    if canRetry
      then Program.suspend (WaitOnNotApplicableTx err)
      -- Preserve timeout invalidation, including removal from allTxs. A delayed
      -- prerequisite can still make this transaction applicable to a later ReqSn;
      -- changing that policy is separate from the interpreted-rule migration.
      else Program.change TxInvalid{headId, utxo = localUTxO, transaction = tx, validationError = err}

  requestSnapshot :: TransactionProgram tx ()
  requestSnapshot =
    case chainedSnapshotRequest confirmedUTxO (localTxs Seq.|> tx) pendingDeposits currentDepositTxId decommitTx settlingCommit of
      Nothing -> void $ Program.decide "snapshot request has useful work" False
      Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} -> do
        void $ Program.decide "snapshot request has useful work" True
        noSnapshotInFlight <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
        when noSnapshotInFlight $ do
          leader <- Program.decide "this party leads the next snapshot" (isLeader parameters party nextSn)
          when leader $ do
            Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
            Program.emit (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)

  RuleEnvironment{party} = env
  OpenState{coordinatedHeadState, headId, parameters} = st
  CoordinatedHeadState{localTxs, localUTxO, allTxs, confirmedSnapshot, seenSnapshot, decommitTx, version, currentDepositTxId} = coordinatedHeadState
  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot
  nextSn = confirmedSn + 1
  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)
  settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot)
{-# INLINE onOpenNetworkReqTxProgram #-}

data ReqSnFunding tx v = ReqSnFunding
  { reqSnVersion :: Version.RequestVersion v
  , reqSnActiveUTxO :: UTxOType tx
  , reqSnCommitUTxO :: Maybe (UTxOType tx)
  , reqSnDecommitUTxO :: Maybe (UTxOType tx)
  }

data ReqSnResolved tx v = ReqSnResolved
  { reqSnFunding :: ReqSnFunding tx v
  , reqSnTransactions :: [tx]
  }

data ReqSnApplied tx v = ReqSnApplied
  { reqSnResolved :: ReqSnResolved tx v
  , reqSnNextUTxO :: UTxOType tx
  }

type ReqSnReady tx v = Version.SignedSnapshot tx v

-- | Process a snapshot request ('ReqSn') from party.
--
-- This checks that s is the next snapshot number and that the party is
-- responsible for leading that snapshot. Then, we potentially wait until the
-- previous snapshot is confirmed (no snapshot is in flight), before we apply
-- (or wait until applicable) the requested transactions to the last confirmed
-- snapshot. Only then, we start tracking this new "seen" snapshot, compute a
-- signature of it and send the corresponding 'AckSn' to all parties. Finally,
-- the pending transaction set gets pruned to only contain still applicable
-- transactions.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  forall tx.
  IsTx tx =>
  Environment ->
  PendingDeposits tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot version.
  SnapshotVersion ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  -- | Optional commit of additional funds into the head.
  Maybe (TxIdType tx) ->
  HeadProgram tx ()
onOpenNetworkReqSn env =
  onOpenNetworkReqSnProgram (ruleEnvironment env)

-- | Snapshot selection, validation order, pruning and effect selection remain in
-- the rule. The interpreter only supplies ledger and cryptographic primitives.
onOpenNetworkReqSnProgram ::
  forall tx.
  IsTx tx =>
  RuleEnvironment ->
  PendingDeposits tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot version.
  SnapshotVersion ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  -- | Optional commit of additional funds into the head.
  Maybe (TxIdType tx) ->
  SnapshotProgram tx ()
onOpenNetworkReqSnProgram env pendingDeposits ChainPointTime{currentSlot, currentChainTime} st ttl otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
  decide
 where
  decide :: SnapshotProgram tx ()
  decide = do
    Version.SomeRequestVersion checked <- checkHeader
    funding <- resolveFunding checked
    resolved <- resolveTransactions funding
    applied <- applyRequestedTransactions resolved
    ready <- buildSignedSnapshot applied
    acknowledge ready

  checkHeader :: SnapshotProgram tx Version.SomeRequestVersion
  checkHeader = do
    validNumber <- Program.decide "request is the next snapshot" (sn == seenSn + 1)
    unless validNumber $ Program.reject $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    leader <- Program.decide "sender leads this snapshot" (isLeader parameters otherParty sn)
    unless leader $ Program.reject $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
    confirmed <- Program.decide "previous snapshot is confirmed" (confSn == seenSn)
    unless confirmed $ Program.suspend $ WaitOnSnapshotNumber seenSn
    -- A version bump may reach this follower after the request, so wait.
    let checked = Version.matchRequestVersion version sv
    matches <- Program.decide "snapshot version matches" (isJust checked)
    case checked of
      Just evidence | matches -> pure evidence
      _ -> Program.suspend $ WaitOnSnapshotVersion sv

  resolveFunding :: Version.RequestVersion v -> SnapshotProgram tx (ReqSnFunding tx v)
  resolveFunding reqSnVersion = do
    (reqSnActiveUTxO, reqSnDecommitUTxO) <- requireApplicableDecommitTx reqSnVersion
    reqSnCommitUTxO <- waitForDeposit reqSnVersion
    pure ReqSnFunding{reqSnVersion, reqSnActiveUTxO, reqSnCommitUTxO, reqSnDecommitUTxO}

  resolveTransactions :: ReqSnFunding tx v -> SnapshotProgram tx (ReqSnResolved tx v)
  resolveTransactions reqSnFunding = do
    reqSnTransactions <- waitResolvableTxs
    pure ReqSnResolved{reqSnFunding, reqSnTransactions}

  applyRequestedTransactions :: ReqSnResolved tx v -> SnapshotProgram tx (ReqSnApplied tx v)
  applyRequestedTransactions reqSnResolved@ReqSnResolved{reqSnFunding = ReqSnFunding{reqSnActiveUTxO, reqSnCommitUTxO}, reqSnTransactions} = do
    reqSnNextUTxO <- requireApplyTxs reqSnActiveUTxO reqSnCommitUTxO reqSnTransactions
    pure ReqSnApplied{reqSnResolved, reqSnNextUTxO}

  buildSignedSnapshot :: ReqSnApplied tx v -> SnapshotProgram tx (ReqSnReady tx v)
  buildSignedSnapshot ReqSnApplied{reqSnResolved = ReqSnResolved{reqSnFunding = ReqSnFunding{reqSnVersion, reqSnCommitUTxO, reqSnDecommitUTxO}, reqSnTransactions}, reqSnNextUTxO} = do
    -- The predecessor is confirmed (checkHeader), so its accumulators can be
    -- updated by the UTxO delta instead of rebuilding from every output.
    let prevSnapshot = getSnapshot confirmedSnapshot
        accumulator =
          Accumulator.applyUTxODelta
            prevSnapshot.accumulator
            (combinedUTxO prevSnapshot.utxo Nothing prevSnapshot.utxoToDecommit)
            (combinedUTxO reqSnNextUTxO Nothing reqSnDecommitUTxO)
        appliedAccumulator
          -- Nothing pending: both accumulators share the same commitment.
          | isNothing reqSnCommitUTxO && isNothing reqSnDecommitUTxO = accumulator
          | otherwise =
              Accumulator.applyUTxODelta
                prevSnapshot.appliedAccumulator
                (combinedUTxO prevSnapshot.utxo prevSnapshot.utxoToCommit Nothing)
                (combinedUTxO reqSnNextUTxO reqSnCommitUTxO Nothing)
    requireValidAccumulatorSize accumulator
    requireValidAccumulatorSize appliedAccumulator
    let reqSnSnapshot =
          Snapshot
            { headId
            , version
            , number = sn
            , confirmed = reqSnTransactions
            , utxo = reqSnNextUTxO
            , utxoToCommit = reqSnCommitUTxO
            , utxoToDecommit = reqSnDecommitUTxO
            , depositTxId = mDepositTxId
            , accumulator
            , appliedAccumulator
            }
    Version.signSnapshot $ Version.snapshotForRequest reqSnVersion reqSnSnapshot

  acknowledge :: ReqSnReady tx v -> SnapshotProgram tx ()
  acknowledge ready = do
    let reqSnSnapshot = Version.signedSnapshot ready
        reqSnSignature = Version.snapshotSignature ready
    Program.emit (NetworkEffect $ AckSn reqSnSignature sn)
    -- Prune after staging the acknowledgement to preserve instruction order.
    -- Aggregation rebuilds localUTxO from the snapshot and retained transactions.
    newLocalTxs <- pruneTransactions reqSnSnapshot.utxo
    Program.change
      SnapshotRequested
        { requestedSnapshot = reqSnSnapshot
        , newLocalTxs
        , newCurrentDepositTxId = mDepositTxId
        }

  waitResolvableTxs :: SnapshotProgram tx [tx]
  waitResolvableTxs =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> pure $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
      unseen -> Program.suspend $ WaitOnTxs unseen

  -- Resolve the requested deposit into the snapshot's 'utxoToCommit'. It is
  -- not added to the active UTxO: a deposit is not spendable on L2 until its
  -- increment lands, otherwise a requested tx could spend it and the snapshot
  -- would count its value twice.
  waitForDeposit :: Version.RequestVersion v -> SnapshotProgram tx (Maybe (UTxOType tx))
  waitForDeposit checked =
    case mDepositTxId of
      -- A request without a deposit drops the commit the confirmed snapshot is
      -- settling. Allow that only once its increment can no longer land
      -- ('DropCommit'), otherwise the head ends up over-backed. Wait instead of
      -- failing: this node may be behind the leader's chain view ('Hydra.Node'
      -- keeps this wait beyond the ttl).
      Nothing ->
        case settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot) of
          Just (ContinueCommit (_, Deposit{deposited})) -> Program.suspend WaitOnUnresolvedCommit{commitUTxO = deposited}
          _ -> pure Nothing
      Just depositTxId
        -- The deposit is already claimed by a signed snapshot whose increment
        -- settled on-chain: it only resurfaces in 'pendingDeposits' when a
        -- rollback erased that increment, and honest leaders exclude it from
        -- selection (see 'scopedDeposits'). Reject rather than sign a
        -- second snapshot claiming the same deposit, see #2741.
        | finalizedDepositTxId finalizedCommit == Just depositTxId ->
            Program.reject $ RequireFailed ReqSnDepositBlockedByFinalizedCommit{depositTxId}
      Just depositTxId ->
        case Map.lookup depositTxId pendingDeposits of
          Nothing
            -- NOTE: must be a Wait while ttl remains, not a require: a
            -- follower can receive the ReqSn before its own chain handler has
            -- processed the deposit observation. Erroring would drop the
            -- message permanently and this node would never sign — with the
            -- snapshot then in flight on all other nodes, the head is stuck
            -- for good. Once ttl is exhausted the deposit is genuinely
            -- unknown (e.g. a stale ReqSn referencing an already recovered
            -- deposit) and we error out.
            | ttl > 0 -> Program.suspend WaitOnDepositObserved{depositTxId}
            | otherwise -> Program.reject $ RequireFailed RequestedDepositNotFoundLocally{depositTxId}
          Just Deposit{status, deposited}
            | status == Inactive -> Program.suspend WaitOnDepositActivation{depositTxId}
            -- NOTE: this makes the commits sequential in a sense that you can't
            -- commit unless the previous commit is settled.
            | hasPendingSnapshotAction (Version.requestVersion checked) confVersion confUTxOToCommit ->
                -- NOTE: identity, not just content. Two deposits can record
                -- the same UTxO, and only the one bound into the confirmed
                -- snapshot is the pending commit being settled.
                --
                -- Accepted even if the deposit is 'Expired': its increment
                -- may still land.
                if confUTxOToCommit == Just deposited && confDepositTxId == Just depositTxId
                  then pure confUTxOToCommit
                  else Program.reject $ RequireFailed ReqSnCommitNotSettled
            | status == Expired -> Program.reject $ RequireFailed RequestedDepositExpired{depositTxId}
            | otherwise -> pure (Just deposited)

  requireApplicableDecommitTx :: Version.RequestVersion v -> SnapshotProgram tx (UTxOType tx, Maybe (UTxOType tx))
  requireApplicableDecommitTx checked =
    case mDecommitTx of
      -- A request without a decommit drops the one the confirmed snapshot is
      -- settling. Unlike a settling commit (see 'waitForDeposit'), a decommit
      -- is never dropped: its decrement is re-posted with every snapshot
      -- carrying it, and a leader stops carrying it only once it observed
      -- the decrement, at the bumped version. Signing such a request would
      -- leave the decommitted outputs in no confirmed snapshot once the
      -- decrement lands (a deposit claimed in their place would also race it
      -- for the version bump), so the head could not be closed and fanned
      -- out from any snapshot the honest nodes hold.
      Nothing
        | hasPendingSnapshotAction (Version.requestVersion checked) confVersion confUTxOToDecommit ->
            Program.reject $ RequireFailed ReqSnDecommitNotSettled
        | otherwise -> pure (confirmedUTxO, Nothing)
      -- Spec: require tx𝜔 = ⊥ ∨ tx𝛼 = ⊥
      --
      -- A snapshot settling both a commit and a decommit cannot be closed:
      -- close and fanout express a single incremental action
      -- ('setIncrementalActionMaybe'). The leader never proposes both (see
      -- 'nextSnapshotRequest'), so this rejects a request that does
      -- anyway rather than confirming an unclosable snapshot.
      Just decommitTx
        | Just depositTxId <- mDepositTxId ->
            Program.reject $ RequireFailed ReqSnBothCommitAndDecommit{depositTxId, decommitTxId = txId decommitTx}
      -- 'Hydra.Contract.Head.checkDecrement' requires at least one decommit
      -- output, so a decommit materializing none could never settle on-chain and
      -- would be re-proposed by every later snapshot.
      Just decommitTx
        | utxoFromTx decommitTx == mempty ->
            Program.reject $ RequireFailed ReqSnDecommitNoOutputs{decommitTxId = txId decommitTx}
      Just decommitTx ->
        -- Spec:
        -- require 𝑣 = 𝑣 ̂ ∧ 𝑠 = 𝑠 ̂ + 1 ∧ leader(𝑠) = 𝑗
        -- wait 𝑠 ̂ = 𝒮.𝑠
        if hasPendingSnapshotAction (Version.requestVersion checked) confVersion confUTxOToDecommit
          then
            if confUTxOToDecommit == Just (utxoFromTx decommitTx)
              then pure (confirmedUTxO, confUTxOToDecommit)
              else Program.reject $ RequireFailed ReqSnDecommitNotSettled
          else do
            result <- Program.applyTransaction currentSlot confirmedUTxO decommitTx
            case result of
              Left err ->
                Program.reject $ RequireFailed $ SnapshotDoesNotApply sn (txId decommitTx) err
              Right newConfirmedUTxO -> do
                let utxoToDecommit = utxoFromTx decommitTx
                let activeUTxO = newConfirmedUTxO `withoutUTxO` utxoToDecommit
                pure (activeUTxO, Just utxoToDecommit)

  -- The snapshot can contain fewer transactions than the ones we have seen at
  -- this stage, but they all _must_ apply correctly to the latest snapshot's
  -- UTxO set, eg. it's illegal for a snapshot leader to request a snapshot
  -- containing transactions that do not apply cleanly.
  --
  -- We fully apply here, re-running signature and Plutus checks. Transactions
  -- resolved from 'allTxs' are not guaranteed to have been validated locally
  -- (an invalid one can be recorded on the receipt 'Wait' branch, and a
  -- follower may not have applied a valid one that conflicts with its own
  -- optimistic local state), so full application is the only place that
  -- guarantees a confirmed snapshot never contains an unvalidated transaction.
  --
  -- A claimed deposit must also be 'absorbable' next to the requested txs.
  requireApplyTxs :: UTxOType tx -> Maybe (UTxOType tx) -> [tx] -> SnapshotProgram tx (UTxOType tx)
  requireApplyTxs utxo mUtxoToCommit' requestedTxs
    | Just depositTxId <- mDepositTxId
    , Just deposited <- mUtxoToCommit'
    , not (absorbable utxo requestedTxs deposited) =
        Program.reject $ RequireFailed ReqSnDepositOutputsHeld{depositTxId}
    | otherwise = do
        result <- applyTransactionsProgram currentSlot utxo requestedTxs
        case result of
          Left (tx, err) ->
            Program.reject $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
          Right u -> pure u

  requireValidAccumulatorSize :: Accumulator.HydraAccumulator -> SnapshotProgram tx ()
  requireValidAccumulatorSize accumulator =
    case Accumulator.checkAccumulatorSize accumulator of
      Left AccumulatorTooLarge{utxoCount, maxAllowed} ->
        Program.reject $ RequireFailed ReqSnUTxOSetTooLarge{utxoCount, maxAllowed}
      Right () -> pure ()

  -- \| Filter 'localTxs' to those that still apply against the running UTxO
  -- after each previous successful tx. The post-snapshot UTxO is not returned:
  -- aggregate will recompute it.
  pruneTransactions utxo0 = go utxo0 localTxs
   where
    go _ Seq.Empty = pure Seq.empty
    go u (tx Seq.:<| rest) = do
      result <- applyTransactionsProgram currentSlot u [tx]
      case result of
        Left _ -> go u rest
        Right u' -> (tx Seq.<|) <$> go u' rest
  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  Snapshot{version = confVersion} = getSnapshot confirmedSnapshot

  confUTxOToCommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToCommit}} -> utxoToCommit

  confDepositTxId = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{depositTxId}} -> depositTxId

  confUTxOToDecommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToDecommit}} -> utxoToDecommit

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version, finalizedCommit} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, headId} = st
{-# INLINE onOpenNetworkReqSnProgram #-}

data AckSeen tx = AckSeen
  { ackSnapshot :: Snapshot tx
  , ackSignatures :: Map.Map Party (Signature (Snapshot tx))
  , ackBytes :: ByteString
  }

data AckComplete tx = AckComplete
  { ackConfirmedSnapshot :: Snapshot tx
  , ackMultiSignature :: MultiSignature (Snapshot tx)
  , ackSignedBytes :: ByteString
  }

-- | Process a snapshot acknowledgement ('AckSn') from a party.
--
-- We do require that the is from the last seen or next expected snapshot, and
-- potentially wait wait for the corresponding 'ReqSn' before proceeding. If the
-- party hasn't sent us a signature yet, we store it. Once a signature from each
-- party has been collected, we aggregate a multi-signature and verify it is
-- correct. If everything is fine, the snapshot can be considered as the latest
-- confirmed one. Similar to processing a 'ReqTx', we check whether we are
-- leading the next snapshot and craft a corresponding 'ReqSn' if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkAckSn ::
  IsTx tx =>
  Environment ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  HeadProgram tx ()
onOpenNetworkAckSn env chainTime pendingDeposits st otherParty signature sn =
  Program.promoteProgram $
    onOpenNetworkAckSnProgram (ruleEnvironment env) chainTime pendingDeposits st otherParty signature sn

onOpenNetworkAckSnProgram ::
  forall tx.
  IsTx tx =>
  RuleEnvironment ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  AcknowledgementProgram tx ()
onOpenNetworkAckSnProgram env@RuleEnvironment{party} ChainPointTime{currentChainTime} pendingDeposits openState otherParty snapshotSignature sn =
  Program.handler decide
 where
  decide :: AcknowledgementProgram tx ()
  decide = do
    checkAckNumber
    seen <- findSeenSnapshot
    checkNewSigner seen
    complete <- collectSignatures seen
    verifySignatures complete
    confirmSnapshot complete

  checkAckNumber :: AcknowledgementProgram tx ()
  checkAckNumber = do
    valid <- Program.decide "acknowledgement number is admissible" (sn `elem` [seenSn, seenSn + 1])
    unless valid $ Program.reject $ RequireFailed $ AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}

  findSeenSnapshot :: AcknowledgementProgram tx (AckSeen tx)
  findSeenSnapshot =
    case seenSnapshot of
      -- Ignore redundant acknowledgements for already confirmed snapshots.
      LastSeenSnapshot{lastSeen}
        | sn <= lastSeen -> Program.finish
      SeenSnapshot{snapshot, signatories, signableBytes}
        | seenSn == sn -> pure AckSeen{ackSnapshot = snapshot, ackSignatures = signatories, ackBytes = signableBytes}
      _ -> Program.suspend WaitOnSeenSnapshot

  checkNewSigner :: AckSeen tx -> AcknowledgementProgram tx ()
  checkNewSigner AckSeen{ackSignatures} = do
    fresh <- Program.decide "party has not signed this snapshot" (not $ Map.member otherParty ackSignatures)
    unless fresh $ Program.reject $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys ackSignatures, receivedSignature = otherParty}

  collectSignatures :: AckSeen tx -> AcknowledgementProgram tx (AckComplete tx)
  collectSignatures AckSeen{ackSnapshot, ackSignatures, ackBytes} = do
    let signatureChange = PartySignedSnapshot{snapshotNumber = ackSnapshot.number, party = otherParty, signature = snapshotSignature}
        signatures = Map.insert otherParty snapshotSignature ackSignatures
    Program.change signatureChange
    complete <- Program.decide "all parties have signed" (Map.keysSet signatures == Set.fromList parties)
    unless complete $ do
      -- Preserve the two signature events emitted for a partial acknowledgement.
      Program.change signatureChange
      Program.finish
    pure
      AckComplete
        { ackConfirmedSnapshot = ackSnapshot
        , ackMultiSignature = aggregateInOrder signatures parties
        , ackSignedBytes = ackBytes
        }

  verifySignatures :: AckComplete tx -> AcknowledgementProgram tx ()
  verifySignatures AckComplete{ackMultiSignature, ackSignedBytes} = do
    verified <- Program.verifySnapshot vkeys ackMultiSignature ackSignedBytes
    case verified of
      Verified -> pure ()
      FailedKeys failures ->
        Program.reject $
          RequireFailed $
            InvalidMultisignature{multisig = show ackMultiSignature, vkeys = failures}
      KeyNumberMismatch ->
        Program.reject $
          RequireFailed $
            InvalidMultisignature{multisig = show ackMultiSignature, vkeys}

  confirmSnapshot :: AckComplete tx -> AcknowledgementProgram tx ()
  confirmSnapshot AckComplete{ackConfirmedSnapshot, ackMultiSignature} =
    Program.change SnapshotConfirmed{headId, snapshot = Nothing, signatures = ackMultiSignature}
      & maybePostIncrementTx ackConfirmedSnapshot ackMultiSignature
      & maybePostDecrementTx ackConfirmedSnapshot ackMultiSignature
      & maybeRequestNextSnapshot ackConfirmedSnapshot

  seenSn = seenSnapshotNumber seenSnapshot

  maybeRequestNextSnapshot previous outcome =
    -- Checked against the snapshot just confirmed: the followers check the
    -- request against that snapshot too.
    case chainedSnapshotRequest (settledUTxO version previous) localTxs pendingDeposits currentDepositTxId decommitTx (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous) of
      Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}
        | isLeader parameters party nextSn ->
            outcome
              >> Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
              >> Program.emit (NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId)
      _ -> outcome
   where
    nextSn = previous.number + 1

  maybePostIncrementTx snapshot@Snapshot{utxoToCommit, depositTxId = signedDepositTxId} signatures outcome =
    -- NOTE: use the snapshot's own deposit and not 'currentDepositTxId'. The
    -- latter can be set by a 'DepositActivated' during the ack flow of an
    -- unrelated snapshot, and only the deposit bound into the signed snapshot
    -- can be claimed by an increment on-chain.
    case (signedDepositTxId, utxoToCommit) of
      (Just depositTxId, Just _) ->
        case Map.lookup depositTxId pendingDeposits of
          Just Deposit{deposited} ->
            outcome
              >> Program.change CommitApproved{headId, utxoToCommit = deposited}
              >> Program.emit
                OnChainEffect
                  { postChainTx =
                      IncrementTx
                        { headSeed
                        , headId
                        , headParameters = parameters
                        , incrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
                        , depositTxId
                        }
                  }
          Nothing -> outcome
      _ -> outcome

  maybePostDecrementTx snapshot@Snapshot{utxoToDecommit} signatures outcome =
    case (decommitTx, utxoToDecommit) of
      (Just tx, Just utxo) ->
        outcome
          >> Program.change
            DecommitApproved
              { headId
              , decommitTxId = txId tx
              , utxoToDecommit = utxo
              }
          >> Program.emit
            OnChainEffect
              { postChainTx =
                  DecrementTx
                    { headSeed
                    , headId
                    , headParameters = parameters
                    , decrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
                    }
              }
      _ -> outcome

  vkeys = vkey <$> parties

  OpenState
    { parameters = parameters@HeadParameters{parties}
    , coordinatedHeadState
    , headId
    , headSeed
    } = openState

  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx, currentDepositTxId, version} = coordinatedHeadState
{-# INLINE onOpenNetworkAckSnProgram #-}

-- | Client request to recover deposited UTxO.
--
-- __Transition__: 'OpenState' → 'OpenState'
-- Client request to recover a deposit by posting a recover transaction on-chain.
-- Works in any head state (Open, Closed, or Idle after fanout). Deposits from a
-- previous head are never cleared from 'pendingDeposits' on fanout, so recovery
-- remains available after a head closes. A new head only sees its own deposits via
-- 'depositsForHead', so old deposits are never accidentally ingested into L2.
-- On-chain, the deposit validator only enforces that the deadline has passed and
-- that the recovered outputs match the originals — it does not require the head to
-- still be active.
onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  -- | Deposit claimed by the retained finalized increment of an open head, if
  -- any (see 'openFinalizedDeposit').
  Maybe (TxIdType tx) ->
  TxIdType tx ->
  HeadProgram tx ()
onClientRecover currentSlot pendingDeposits finalizedDeposit recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Program.reject $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited}
      -- The deposit resurfaced because its finalized increment was rolled
      -- back: the deposited funds are already merged into the head, so
      -- recovering them on-chain would corrupt the L2 ledger. Only re-posting
      -- the increment settles this deposit, see #2741.
      | finalizedDeposit == Just recoverTxId ->
          Program.reject $ RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}
      | otherwise ->
          traverse_
            Program.emit
            [ OnChainEffect
                { postChainTx =
                    RecoverTx
                      { headId
                      , recoverTxId = recoverTxId
                      , -- XXX: Why is this called deadline?
                        deadline = currentSlot
                      , recoverUTxO = deposited
                      }
                }
            ]

-- | Client request to decommit UTxO from the head.
--
-- Only possible if there is no decommit _in flight_ and if the tx applies
-- cleanly to the local ledger state.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientDecommit ::
  IsTx tx =>
  HeadId ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  -- | Decommit transaction.
  tx ->
  HeadProgram tx ()
onOpenClientDecommit headId currentSlot coordinatedHeadState decommitTx =
  checkNoDecommitInFlight $
    checkValidDecommitTx $
      requireDecommitOutputs headId localUTxO decommitTx $
        Program.emit (NetworkEffect ReqDec{transaction = decommitTx})
 where
  checkNoDecommitInFlight continue =
    case mExistingDecommitTx of
      Just existingDecommitTx ->
        Program.change
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitAlreadyInFlight
                  { otherDecommitTxId = txId existingDecommitTx
                  }
            }
      Nothing -> continue

  checkValidDecommitTx cont = do
    result <- Program.applyTransaction currentSlot localUTxO decommitTx
    case result of
      Left err ->
        Program.change
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitTxInvalid
                  { localUTxO
                  , validationError = err
                  }
            }
      Right _ -> cont

  CoordinatedHeadState{decommitTx = mExistingDecommitTx, localUTxO} = coordinatedHeadState

-- | Process the request 'ReqDec' to decommit something from the Open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- When node receives 'ReqDec' network message it should:
-- - Check there is no decommit in flight:
--   - Alter it's state to record what is to be decommitted
--   - Issue a server output 'DecommitRequested' with the relevant utxo
--   - Issue a 'ReqSn' since all parties need to agree in order for decommit to
--   be taken out of a Head.
-- - Check if we are the leader
onOpenNetworkReqDec ::
  IsTx tx =>
  Environment ->
  TTL ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  tx ->
  HeadProgram tx ()
onOpenNetworkReqDec env ttl ChainPointTime{currentSlot, currentChainTime} pendingDeposits openState decommitTx =
  -- Spec: wait 𝑈𝛼 = ∅ ^ txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $
    requireDecommitOutputs headId localUTxO decommitTx $
      -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
      -- Spec: txω ← tx
      Program.change DecommitRecorded{headId, decommitTx}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        >> maybeRequestSnapshot
 where
  -- Spec: wait 𝑈𝛼 = ∅. A pending commit (deposit) must settle before a
  -- decommit can be recorded, otherwise a later snapshot would carry both (see
  -- the symmetric guard on 'DepositActivated', which blocks a deposit while a
  -- decommit is pending). While ttl remains we wait, so the decommit proceeds
  -- once the increment finalises and clears 'currentDepositTxId'; once ttl is
  -- exhausted we reject with 'DepositInFlight' (mirroring the branches below) so
  -- the client can act (e.g. recover the deposit) instead of the request being
  -- silently dropped.
  --
  -- The decommit waits if the next snapshot request would include a deposit.
  waitOnApplicableDecommit cont
    | Just depositTxId <- requestDepositTxId
    , Just Deposit{deposited = commitUTxO} <- Map.lookup depositTxId pendingDeposits =
        if ttl > 0
          then Program.suspend $ WaitOnUnresolvedCommit{commitUTxO}
          else
            Program.change
              DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason = DepositInFlight{depositTxId, commitUTxO}
                }
    | otherwise =
        case mExistingDecommitTx of
          Nothing -> do
            result <- Program.applyTransaction currentSlot localUTxO decommitTx
            case result of
              Right _ -> cont
              Left validationError
                | ttl > 0 ->
                    Program.suspend $
                      WaitOnNotApplicableDecommitTx
                        ServerOutput.DecommitTxInvalid{localUTxO, validationError}
                | otherwise ->
                    Program.change
                      DecommitInvalid
                        { headId
                        , decommitTx
                        , decommitInvalidReason =
                            ServerOutput.DecommitTxInvalid{localUTxO, validationError}
                        }
          Just existingDecommitTx
            | ttl > 0 ->
                Program.suspend $
                  WaitOnNotApplicableDecommitTx
                    DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
            | otherwise ->
                Program.change
                  DecommitInvalid
                    { headId
                    , decommitTx
                    , decommitInvalidReason =
                        DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
                    }

  -- May carry a settling commit instead of the decommit.
  maybeRequestSnapshot =
    when (canRequestSnapshot parameters party nextSn seenSnapshot) $
      Program.emit (NetworkEffect (ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId))

  SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} =
    nextSnapshotRequest confirmedUTxO localTxs pendingDeposits currentDepositTxId (Just decommitTx) (settlingCommitAt env version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot))

  Environment{party} = env

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)

  CoordinatedHeadState
    { decommitTx = mExistingDecommitTx
    , confirmedSnapshot
    , localTxs
    , localUTxO
    , version
    , seenSnapshot
    , currentDepositTxId
    } = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
    } = openState

-- | Select deposits in the given lifecycle status.
depositsWithStatus :: DepositStatus -> PendingDeposits tx -> PendingDeposits tx
depositsWithStatus expected = Map.filter (\Deposit{status} -> status == expected)

determineNextDepositStatus :: Environment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  (\deposit -> deposit{status = determineStatus deposit}) <$> pendingDeposits
 where
  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositActivation = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  Environment{depositPeriod, depositActivation} = env

-- | Process the chain (and time) advancing in any head state.
--
-- __Transition__: 'AnyState' → 'AnyState'
--
-- This is primarily used to track deposits status changes.
onChainTick :: IsTx tx => Environment -> PendingDeposits tx -> UTCTime -> HeadProgram tx ()
onChainTick env pendingDeposits chainTime =
  mkDepositActivated newActive >> mkDepositExpired newExpired
 where
  -- XXX: This is a bit messy
  newActive = Map.difference nextActive pendingActive

  newExpired = Map.difference nextExpired pendingExpired

  pendingActive = depositsWithStatus Active pendingDeposits

  pendingExpired = depositsWithStatus Expired pendingDeposits

  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  nextActive = depositsWithStatus Active nextDeposits

  nextExpired = depositsWithStatus Expired nextDeposits

  mkDepositActivated m = traverse_ Program.change . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    [DepositActivated{depositTxId, chainTime, deposit}]

  mkDepositExpired m = traverse_ Program.change . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    [DepositExpired{depositTxId, chainTime, deposit}]

-- | Process the chain (and time) advancing in an open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- This is primarily used to track deposits and either drop them or request
-- snapshots for inclusion.
onOpenChainTick :: forall tx. IsTx tx => Environment -> UTCTime -> PendingDeposits tx -> OpenState tx -> HeadProgram tx ()
onOpenChainTick env chainTime pendingDeposits st = decision
 where
  decision
    | not (canRequestSnapshot parameters party nextSn seenSnapshot) = pure ()
    | otherwise =
        let settling = settlingCommitAt env version chainTime deposits confirmed
            built = nextSnapshotRequest (settledUTxO version (getSnapshot confirmedSnapshot)) localTxs deposits currentDepositTxId decommitTx settling
         in case (settling, built) of
              -- The commit is still settling; requests carrying txs continue it.
              (Just (ContinueCommit _), _) -> pure ()
              -- The commit can no longer land: drop it so other deposits can be claimed.
              (Just DropCommit, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}) -> request requestTxs requestDecommitTx requestDepositTxId
              -- Spec: wait txω = ⊥ ∧ Uα = ∅.
              (Nothing, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId = Just depositTxId})
                | isNothing decommitTx -> request requestTxs requestDecommitTx (Just depositTxId)
              _ -> pure ()
  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  newActive = depositsWithStatus Active nextDeposits

  newExpired = depositsWithStatus Expired nextDeposits

  -- All deposits, with their status updated to this tick's time. Empty
  -- deposits are ignored, because an empty increment cannot be posted.
  deposits = Map.filter (\Deposit{deposited} -> deposited /= mempty) (newActive <> newExpired <> pendingDeposits)

  request :: [tx] -> Maybe tx -> Maybe (TxIdType tx) -> HeadProgram tx ()
  request txs mDecommitTx mDepositTxId =
    Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
      >> Program.emit (NetworkEffect $ ReqSn version nextSn (txId <$> txs) mDecommitTx mDepositTxId)

  confirmed@Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  nextSn = confirmedSn + 1

  Environment{party} = env

  CoordinatedHeadState
    { localTxs
    , confirmedSnapshot
    , seenSnapshot
    , version
    , decommitTx
    , currentDepositTxId
    } = coordinatedHeadState

  OpenState{coordinatedHeadState, parameters} = st

-- | If this node is the snapshot leader and there are pending local transactions,
-- request the next snapshot with the bumped version after a commit or decommit
-- finalises on-chain.
--
-- Guards:
--   * Only fires when 'newVersion' is ahead of the local 'version': this
--     avoids duplicate 'SnapshotRequestDecided' events when multiple parties
--     post the same on-chain tx and each posting produces a separate
--     finalisation observation, and it keeps a finalization re-landing after a
--     rollback (its 'newVersion' is at or behind the local version, which
--     never rolls back) from requesting a snapshot with a stale version.
--   * Skips when AckSns are already being collected ('SeenSnapshot'): the
--     in-flight snapshot will complete and 'maybeRequestNextSnapshot' will chain
--     the next one with the bumped version. Firing here would use stale
--     'localTxs' and cause 'BadInputsUTxO' on other parties.
--   * Allows 'RequestedSnapshot': the in-flight ReqSn carries the old version
--     and will be parked by 'waitOnSnapshotVersion' until TTL drops it, so we
--     re-request immediately with the new version to make progress without
--     waiting for the stale request's retries to exhaust.
--
-- The optional 'depositTxId' argument is forwarded into 'ReqSn': commit
-- finalisation passes 'Nothing' (deposit already included), while decommit
-- finalisation passes the next queued deposit if one is pending.
maybeRequestSnapshotAfterVersionBump ::
  IsTx tx =>
  HeadParameters ->
  Party ->
  SnapshotNumber ->
  -- | The request to make, if any, built for the new version. Its decommit is
  -- ignored: it is the one that just settled.
  Maybe (SnapshotRequest tx) ->
  SnapshotVersion ->
  SnapshotVersion ->
  SeenSnapshot tx ->
  HeadProgram tx ()
maybeRequestSnapshotAfterVersionBump parameters party nextSn mRequest version newVersion seenSnapshot =
  case mRequest of
    Just SnapshotRequest{requestTxs, requestDepositTxId}
      | isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot) ->
          Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
            >> Program.emit (NetworkEffect $ ReqSn newVersion nextSn (txId <$> requestTxs) Nothing requestDepositTxId)
    _ -> pure ()

-- | Observe a increment transaction. If the outputs match the ones of the
-- pending commit UTxO, then we consider the deposit/increment finalized, and remove the
-- increment UTxO from 'pendingDeposits' from the local state.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainIncrementTx ::
  IsTx tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Deposit TxId
  TxIdType tx ->
  SnapshotNumber ->
  MultiSignature (Snapshot tx) ->
  HeadProgram tx ()
onOpenChainIncrementTx env pendingDeposits openState newChainState newVersion depositTxId observedSnapshotNumber observedSignatures =
  Program.change CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
    >> adoptObservedSnapshotOrRequestNext
 where
  -- The increment proves on-chain that the snapshot it settled is fully
  -- signed. If that is the one we are still collecting AckSns for, adopt it
  -- with the on-chain multisignature instead of waiting on a peer.
  adoptObservedSnapshotOrRequestNext =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber, depositTxId = seenDepositTxId}, signableBytes}
        | Just settled <- Version.matchSettlement observedSnapshotNumber newVersion snapshot
        , seenDepositTxId == Just depositTxId ->
            do
              verified <- Program.verifySnapshot vkeys observedSignatures signableBytes
              case verified of
                Verified -> do
                  let settledSnapshot = Version.settlementSnapshot settled
                      settledVersion = Version.settlementVersion settled
                  Program.change SnapshotConfirmed{headId, snapshot = Just settledSnapshot, signatures = observedSignatures}
                  maybeRequestSnapshotAfterVersionBump parameters party (seenSnNumber + 1) (requestOn settledSnapshot) version settledVersion (LastSeenSnapshot seenSnNumber)
                _ -> requestNext
      _ -> requestNext

  requestNext =
    maybeRequestSnapshotAfterVersionBump parameters party nextSn (requestOn (getSnapshot confirmedSnapshot)) version newVersion seenSnapshot

  OpenState{headId, parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, version, seenSnapshot, decommitTx} = coordinatedHeadState

  -- Built on the snapshot that is confirmed after this increment (the adopted
  -- one, if any), as followers check it. The deposit just settled, so it is
  -- excluded.
  requestOn snapshot =
    chainedSnapshotRequest (settledUTxO newVersion snapshot) localTxs (Map.delete depositTxId pendingDeposits) Nothing decommitTx Nothing

  HeadParameters{parties} = parameters

  vkeys = vkey <$> parties

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

-- | Observe a decrement transaction. If the outputs match the ones of the
-- pending decommit tx, then we consider the decommit finalized, and remove the
-- decommit tx in flight.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainDecrementTx ::
  IsTx tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Outputs removed by the decrement
  UTxOType tx ->
  -- | Number of the snapshot the decrement settled
  SnapshotNumber ->
  -- | Multisignature carried by the decrement redeemer
  MultiSignature (Snapshot tx) ->
  HeadProgram tx ()
onOpenChainDecrementTx env pendingDeposits openState newChainState newVersion distributedUTxO observedSnapshotNumber observedSignatures =
  Program.change
    DecommitFinalized
      { chainState = newChainState
      , headId
      , newVersion
      , distributedUTxO
      }
    >> adoptObservedSnapshotOrRequestNext
 where
  -- Same reasoning as in 'onOpenChainIncrementTx': the decrement proves the
  -- snapshot it settled is fully signed, so adopt it if it is the one in flight.
  adoptObservedSnapshotOrRequestNext =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber, utxoToDecommit = seenDecommit}, signableBytes}
        | Just settled <- Version.matchSettlement observedSnapshotNumber newVersion snapshot
        , isJust seenDecommit ->
            do
              verified <- Program.verifySnapshot vkeys observedSignatures signableBytes
              case verified of
                Verified -> do
                  let settledSnapshot = Version.settlementSnapshot settled
                      settledVersion = Version.settlementVersion settled
                  Program.change SnapshotConfirmed{headId, snapshot = Just settledSnapshot, signatures = observedSignatures}
                  maybeRequestSnapshotAfterVersionBump parameters party (seenSnNumber + 1) (requestOn settledSnapshot) version settledVersion (LastSeenSnapshot seenSnNumber)
                _ -> requestNext
      _ -> requestNext

  requestNext =
    maybeRequestSnapshotAfterVersionBump parameters party nextSn (requestOn (getSnapshot confirmedSnapshot)) version newVersion seenSnapshot

  OpenState{headId, parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, currentDepositTxId, version, seenSnapshot, decommitTx} = coordinatedHeadState

  -- Built on the snapshot that is confirmed after this decrement (the adopted
  -- one, if any), as followers check it. No commit is settling next to a
  -- decommit.
  requestOn snapshot =
    chainedSnapshotRequest (settledUTxO newVersion snapshot) localTxs pendingDeposits currentDepositTxId decommitTx Nothing

  HeadParameters{parties} = parameters

  vkeys = vkey <$> parties

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

-- | On rollback, re-post the IncrementTx if there is a pending deposit whose
-- confirmed snapshot contains a matching utxoToCommit. The rollback may have
-- erased the original on-chain IncrementTx observation.
--
-- If the increment was already finalized ('CommitFinalized' consumed the
-- deposit and 'confirmedSnapshot' may have advanced past the incrementing
-- snapshot), fall back to the retained 'finalizedCommit' — but only when the
-- rollback reaches strictly before its observation slot; otherwise the
-- increment is still on chain and re-posting would only produce noise. See
-- #2741.
maybeRepostIncrementTx ::
  IsTx tx =>
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  PendingDeposits tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  -- | Slot rolled back to
  ChainSlot ->
  HeadProgram tx ()
maybeRepostIncrementTx headSeed headId parameters pendingDeposits confirmedSnapshot finalizedCommit rolledBackSlot =
  -- NOTE: the deposit comes from the confirmed snapshot itself, not from
  -- 'currentDepositTxId'. Only the deposit bound into the signed snapshot can be
  -- claimed on-chain, and 'DepositActivated' can set 'currentDepositTxId' to an
  -- unrelated deposit after that snapshot was confirmed.
  case confirmedSnapshot of
    ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId}, signatures}
      | Just Deposit{} <- Map.lookup depositTxId pendingDeposits ->
          repost ConfirmedSnapshot{snapshot, signatures} depositTxId
    _ ->
      case finalizedCommit of
        Just FinalizedSnapshot{snapshot, observedAtSlot}
          -- Strictly (<): the rollback point is the last common block, so
          -- rolling back TO the increment's slot means it is still on chain.
          | rolledBackSlot < observedAtSlot
          , Just depositTxId <- (getSnapshot snapshot).depositTxId ->
              repost snapshot depositTxId
        _ -> pure ()
 where
  repost incrementingSnapshot depositTxId =
    Program.emit
      OnChainEffect
        { postChainTx =
            IncrementTx
              { headSeed
              , headId
              , headParameters = parameters
              , incrementingSnapshot
              , depositTxId
              }
        }

-- | On rollback, re-post the DecrementTx if there is a pending decommit whose
-- confirmed snapshot contains a matching utxoToDecommit. The rollback may have
-- erased the original on-chain DecrementTx observation.
--
-- If the decrement was already finalized ('DecommitFinalized' cleared
-- 'decommitTx' and 'confirmedSnapshot' may have advanced past the decrementing
-- snapshot), fall back to the retained 'finalizedDecommit' — but only when the
-- rollback reaches strictly before its observation slot. See #2741.
maybeRepostDecrementTx ::
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  Maybe tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  -- | Slot rolled back to
  ChainSlot ->
  HeadProgram tx ()
maybeRepostDecrementTx headSeed headId parameters mDecommitTx confirmedSnapshot finalizedDecommit rolledBackSlot =
  case (mDecommitTx, confirmedSnapshot) of
    (Just _, ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToDecommit = Just _}, signatures}) ->
      repost ConfirmedSnapshot{snapshot, signatures}
    _ ->
      case finalizedDecommit of
        Just FinalizedSnapshot{snapshot, observedAtSlot}
          | rolledBackSlot < observedAtSlot ->
              repost snapshot
        _ -> pure ()
 where
  repost decrementingSnapshot =
    Program.emit
      OnChainEffect
        { postChainTx =
            DecrementTx
              { headSeed
              , headId
              , headParameters = parameters
              , decrementingSnapshot
              }
        }

-- | Start a snapshot while idle only when this party leads its number.
canRequestSnapshot :: HeadParameters -> Party -> SnapshotNumber -> SeenSnapshot tx -> Bool
canRequestSnapshot parameters party number seen =
  not (snapshotInFlight seen) && isLeader parameters party number

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- ** Closing the Head

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  HeadProgram tx ()
onOpenClientClose st =
  -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, ξ)
  Program.emit
    OnChainEffect
      { postChainTx =
          CloseTx
            { headId
            , headParameters = parameters
            , openVersion = version
            , closingSnapshot = confirmedSnapshot
            }
      }
 where
  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId, parameters} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- A peer that withheld its 'AckSn' can close with the snapshot we are still
-- collecting signatures for. The close carries its full multisignature, so
-- adopt it as confirmed; otherwise we could not fan out the closed head.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  IsTx tx =>
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  -- | Multisignature carried by the close redeemer.
  MultiSignature (Snapshot tx) ->
  HeadProgram tx ()
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline observedSignatures =
  adoptClosedSnapshot
    >> Program.change HeadClosed{headId, snapshotNumber = closedSnapshotNumber, chainState = newChainState, contestationDeadline}
    & maybePostContest
 where
  -- The multisignature verifying over our own signable bytes proves the head
  -- closed with exactly the snapshot we saw. Anything else leaves the confirmed
  -- snapshot as it is.
  adoptClosedSnapshot =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber}, signableBytes}
        | seenSnNumber == closedSnapshotNumber -> do
            verified <- Program.verifySnapshot vkeys observedSignatures signableBytes
            case verified of
              Verified -> Program.change SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
              _ -> pure ()
      _ -> pure ()

  HeadParameters{parties} = headParameters

  vkeys = vkey <$> parties

  maybePostContest outcome =
    -- Spec: if ̅S.s > sc
    if number (getSnapshot confirmedSnapshot) > closedSnapshotNumber
      then
        outcome
          -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
          --       ξ ← ̅S.σ
          --       postTx (contest, ̅S.v, ̅S.s, η, ξ)
          >> Program.emit
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
      else outcome

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, version} = coordinatedHeadState

  OpenState{parameters = headParameters, headId, coordinatedHeadState} = openState

-- | Client request to side load confirmed snapshot.
--
-- Note this is not covered by the spec as it is not reachable from an organic use of the protocol.
--
-- It must not have any effects outside of a neutral modification of the state to:
-- * something it was before (in the case of the initial snapshot).
-- * something it would be using side communication (in the case of a confirmed snapshot).
--
-- Besides the above, it is expected to work very much like the confirmed snapshot.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientSideLoadSnapshot :: IsTx tx => OpenState tx -> ConfirmedSnapshot tx -> HeadProgram tx ()
onOpenClientSideLoadSnapshot openState requestedConfirmedSnapshot =
  case requestedConfirmedSnapshot of
    InitialSnapshot{} ->
      requireVerifiedSameSnapshot $
        Program.change LocalStateCleared{headId, snapshotNumber = requestedSn}
    ConfirmedSnapshot{snapshot, signatures} ->
      requireVerifiedSnapshotNumber $
        requireVerifiedL1Snapshot $
          requireVerifiedMultisignature snapshot signatures $
            traverse_
              Program.change
              [ SnapshotConfirmed{headId, snapshot = Just snapshot, signatures}
              , LocalStateCleared{headId, snapshotNumber = requestedSn}
              ]
 where
  OpenState
    { headId
    , parameters = HeadParameters{parties}
    , coordinatedHeadState
    } = openState

  CoordinatedHeadState
    { confirmedSnapshot = currentConfirmedSnapshot
    } = coordinatedHeadState

  vkeys = vkey <$> parties

  currentSnapshot@Snapshot
    { version = lastSeenSv
    , number = lastSeenSn
    , utxoToCommit = lastSeenSc
    , depositTxId = lastSeenDeposit
    , utxoToDecommit = lastSeenSd
    } = getSnapshot currentConfirmedSnapshot

  requestedSnapshot@Snapshot
    { version = requestedSv
    , number = requestedSn
    , utxoToCommit = requestedSc
    , depositTxId = requestedDeposit
    , utxoToDecommit = requestedSd
    } = getSnapshot requestedConfirmedSnapshot

  clientInput = SideLoadSnapshot requestedConfirmedSnapshot

  sideLoadFailed requirementFailure =
    Program.emit . ClientEffect $
      ServerOutput.SideLoadSnapshotRejected{clientInput, requirementFailure}

  requireVerifiedSameSnapshot cont =
    if requestedSnapshot == currentSnapshot
      then cont
      else sideLoadFailed SideLoadInitialSnapshotMismatch

  requireVerifiedSnapshotNumber cont =
    if requestedSn >= lastSeenSn
      then cont
      else sideLoadFailed SideLoadSnNumberInvalid{requestedSn, lastSeenSn}

  requireVerifiedL1Snapshot cont
    | requestedSv /= lastSeenSv = sideLoadFailed SideLoadSvNumberInvalid{requestedSv, lastSeenSv}
    | requestedSc /= lastSeenSc = sideLoadFailed SideLoadUTxOToCommitInvalid{requestedSc, lastSeenSc}
    -- The pending commit is L1-relevant state, and since the binding change it is
    -- the deposit that identifies it, not the committed content.
    | requestedDeposit /= lastSeenDeposit = sideLoadFailed SideLoadDepositTxIdInvalid{requestedDeposit, lastSeenDeposit}
    | requestedSd /= lastSeenSd = sideLoadFailed SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}
    | otherwise = cont

  requireVerifiedMultisignature snapshot signatories cont = do
    verified <- Program.verifySnapshot vkeys signatories (getSignableRepresentation snapshot)
    case verified of
      Verified -> cont
      FailedKeys failures ->
        sideLoadFailed SideLoadInvalidMultisignature{multisig = show signatories, vkeys = failures}
      KeyNumberMismatch ->
        sideLoadFailed SideLoadInvalidMultisignature{multisig = show signatories, vkeys}

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- A peer that withheld its 'AckSn' can close with an older snapshot and then
-- contest with the one we are still collecting signatures for. The contest
-- carries its full multisignature, so adopt it as confirmed, like
-- 'onOpenChainCloseTx' does for a close.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state.
  ChainStateType tx ->
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  -- | Multisignature carried by the contest redeemer.
  MultiSignature (Snapshot tx) ->
  HeadProgram tx ()
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline observedSignatures = do
  adopted <- case unconfirmedSnapshot of
    Just snapshot@Snapshot{number = unconfirmedSn}
      | unconfirmedSn == snapshotNumber -> do
          verified <- Program.verifySnapshot vkeys observedSignatures (getSignableRepresentation snapshot)
          case verified of
            Verified -> do
              Program.change SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
              pure True
            _ -> pure False
    _ -> pure False
  Program.change HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
  when (not adopted && number (getSnapshot confirmedSnapshot) > snapshotNumber) $
    Program.emit
      OnChainEffect
        { postChainTx =
            ContestTx
              { headId
              , headParameters
              , openVersion = version
              , contestingSnapshot = confirmedSnapshot
              }
        }
 where
  ClosedState{parameters = headParameters, confirmedSnapshot, headId, version, unconfirmedSnapshot} = closedState

  HeadParameters{parties} = headParameters

  vkeys = vkey <$> parties

-- | Client request to fanout the whole closed head automatically. Emits a
-- 'FanoutTx'; the chain layer either lands a single full fanout (→ 'IdleState')
-- or falls back to dynamically-chunked partial fanouts. The first observed
-- partial fanout transitions the head to 'PartialFanout' in 'AutoDrain' mode,
-- which keeps draining the rest automatically until the final (burning) step.
--
-- This node becomes the fanout /driver/: it transitions into 'PartialFanout' in
-- 'AutoDrain' mode so that, as the chain layer chunks the fanout, /this/ node
-- auto-continues to completion. Other parties that merely observe the resulting
-- partial fanout do not auto-drive (see 'onClosedChainPartialFanoutTx').
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState' (then → 'IdleState' once
-- the final fanout is observed).
onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  HeadProgram tx ()
onClosedClientFanout closedState =
  -- A plain 'Fanout' is a full fanout by definition, and is only reachable from
  -- 'Closed'.
  fanoutStepStateChange headId FullFanoutStep fullUTxO
    >> emitFanoutStep FullFanoutStep confirmedSnapshot version headSeed contestationDeadline
 where
  fullUTxO = computeFullFanoutUTxO closedState

  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | The state change that goes with a fanout step, for the callers that own the
-- head state: taking the driver role, or recording the selection being
-- distributed. Decided here once rather than in each handler.
--
-- The selection comes from the step rather than the caller, so a step can only
-- ever be recorded against the set it actually distributes.
fanoutStepStateChange ::
  HeadId ->
  NextFanoutStep tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  HeadProgram tx ()
fanoutStepStateChange headId step remainingOutputs =
  case step of
    -- The selection is not distributed as one: it becomes a full fanout, and this
    -- node drives the rest of it.
    FullFanoutStep -> Program.change HeadFanoutInitiated{headId, remainingOutputs}
    -- 'FinalStep' posts a transaction and nothing else, so the selection has to
    -- be recorded for it too: a rollback or restart before that transaction
    -- lands otherwise leaves the driver with no selection to resume from.
    FinalStep{stepDistribute} -> recordSelection stepDistribute
    PartialStep{stepDistribute} -> recordSelection stepDistribute
 where
  recordSelection selection = Program.change HeadPartialFanoutSelected{headId, remainingOutputs, selection}

-- | Which transaction the next fanout step has to be, carrying the sets that
-- transaction needs. Decided once by 'nextFanoutStep' so that callers deciding
-- what else to record and 'emitFanoutStep' deciding what to post cannot
-- disagree, and so that no caller can pair a step with sets it does not go with.
data NextFanoutStep tx
  = -- | The target is the whole remainder and the head is already in
    -- @FanoutProgress@ on chain: the final step, distributing the rest and
    -- burning the head tokens.
    FinalStep {stepDistribute :: UTxOType tx}
  | -- | The target is the whole remainder but the head output still carries the
    -- @Closed@ datum, which the final step is not valid against. Posted as a
    -- non-final step it would empty the head, which 'mustNotBeLastBatch' rejects,
    -- so the chunk search settles for one output less and the head needs a second
    -- transaction to finish — and cannot be drained at all when a single output
    -- is left. Covering everything is a full fanout, so post that (#2855).
    FullFanoutStep
  | -- | The target is a strict subset: a non-final chunk distributing it, proved
    -- against the set the head's current datum commits to.
    PartialStep {stepDistribute :: UTxOType tx, stepProof :: UTxOType tx}

-- | Decide the next fanout step. The multiset comparison here is the only one
-- needed: callers pass the result on rather than re-deriving it.
nextFanoutStep ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  -- | Chunk source for the next step (the user selection, or the whole remaining
  --   set when auto-draining)
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  OnChainFanoutDatum ->
  NextFanoutStep tx
nextFanoutStep confirmedSnapshot version target remaining onChainDatum
  | target `sameOutputs` remaining =
      case onChainDatum of
        DatumFanoutProgress -> FinalStep{stepDistribute = remaining}
        DatumClosed -> FullFanoutStep
  | otherwise = PartialStep{stepDistribute = target, stepProof = proofSet}
 where
  -- The set the on-chain datum commits to: the fan-out-able set from a @Closed@
  -- head, and the not-yet-distributed set once the head is in @FanoutProgress@,
  -- since every step removes exactly what it distributed.
  proofSet = case onChainDatum of
    DatumClosed -> fanoutUTxOFromSnapshot confirmedSnapshot version
    DatumFanoutProgress -> remaining

-- | Given the on-chain @version@ and a snapshot's own version, decide which of a
-- pending commit / decommit is still to be distributed on fanout. When the
-- increment has landed on chain (versions match) the commit was already applied
-- (drop it) while a pending decommit still applies; otherwise the commit still
-- applies and the decommit was already paid out. Centralises the version check
-- shared by 'mkFullFanoutTx' and 'fanoutUTxOFromSnapshot'.
effectiveCommitDecommit ::
  -- | On-chain version
  SnapshotVersion ->
  -- | Snapshot version
  SnapshotVersion ->
  -- | Pending commit
  Maybe (UTxOType tx) ->
  -- | Pending decommit
  Maybe (UTxOType tx) ->
  (Maybe (UTxOType tx), Maybe (UTxOType tx))
effectiveCommitDecommit onChainVersion snapshotVersion utxoToCommit utxoToDecommit
  | snapshotVersion == onChainVersion = (Nothing, utxoToDecommit)
  | otherwise = (utxoToCommit, Nothing)

-- | Build the full automatic 'FanoutTx' from a confirmed snapshot at the given
-- on-chain version. Shared by 'onClosedClientFanout' and the rollback re-post in
-- 'FanoutProgress' ('repostFanoutStep').
mkFullFanoutTx ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  PostChainTx tx
mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline =
  FanoutTx
    { utxo
    , utxoToCommit = effectiveCommit
    , utxoToDecommit = effectiveDecommit
    , headSeed
    , contestationDeadline
    }
 where
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot

-- | Client request to fan out a user-selected subset of a freshly closed head.
-- Validates the selection is a non-empty sub-multiset (by content) of the
-- fan-out-able UTxO, then transitions the head into 'PartialFanout' and posts
-- whichever step 'nextFanoutStep' decides on: a strict subset records the
-- selection and posts a 'PartialFanoutTx', while a selection covering the whole
-- head is a full fanout and drains it automatically instead.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedClientPartialFanout ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  HeadProgram tx ()
onClosedClientPartialFanout closedState selection = decision
 where
  decision
    | nullOutputs selection || not (selection `isSubMultisetOf` fullUTxO) =
        Program.emit . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (Closed closedState)
    | otherwise =
        -- A selection covering the whole head becomes a full fanout - what the user
        -- means by "fan out everything", and the only option for a head holding a
        -- single UTxO, since a non-final batch has to leave one behind.
        fanoutStepStateChange headId step fullUTxO
          >> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
  -- Fresh head: the on-chain datum is still @Closed@.
  step = nextFanoutStep confirmedSnapshot version selection fullUTxO DatumClosed

  fullUTxO = computeFullFanoutUTxO closedState
  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | Client request to continue a selective partial fanout. Validates the
-- selection against the current 'remainingOutputs' and emits whichever step
-- 'nextFanoutStep' decides on, which is not always a recorded selection: one
-- covering the whole remainder finalizes the head, or, if nothing has been
-- distributed yet, becomes a full fanout that drains it automatically.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutClientPartialFanout ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  HeadProgram tx ()
onPartialFanoutClientPartialFanout pfs selection = decision
 where
  decision
    | nullOutputs selection || not (selection `isSubMultisetOf` remainingOutputs) =
        Program.emit . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (FanoutProgress pfs)
    | otherwise =
        fanoutStepStateChange headId step remainingOutputs
          >> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
  -- The on-chain datum is only @FanoutProgress@ once a partial fanout has
  -- actually landed (some outputs distributed); until then it is still @Closed@
  -- and a 'FinalPartialFanoutTx' is not yet valid. Compute this the same way as
  -- 'repostFanoutStep' rather than assuming 'True'.
  step = nextFanoutStep confirmedSnapshot version selection remainingOutputs (onChainFanoutDatum distributedOutputs)

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, distributedOutputs} = pfs

-- | Observe a (full or final) fanout transaction, finalizing the head.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  HeadProgram tx ()
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  Program.change HeadFannedOut{headId, finalizedOutputs = fanoutUTxO, chainState = newChainState}
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while this node is still 'Closed' — i.e. a partial
-- fanout this node did NOT initiate (another party did). The fanout driver moved
-- to 'PartialFanout' when it issued its 'Fanout'/'PartialFanout' command, so this
-- handler is only reached by passive observers.
--
-- The observer transitions into 'PartialFanout' in 'AwaitingSelection' mode and
-- does __not__ auto-drive the rest: only the driver advances the fanout. This is
-- what makes selective partial fanout work in a multi-party head — observers must
-- not steamroll the remaining UTxO the driver deliberately left.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  HeadProgram tx ()
onClosedChainPartialFanoutTx closedState newChainState observedDistributed =
  let fullUTxO = computeFullFanoutUTxO closedState
      remaining = removeDistributedOutputs (outputsOfUTxO observedDistributed) fullUTxO
      distributedUTxO = withoutUTxO fullUTxO remaining
   in Program.change
        HeadPartialFannedOut
          { headId
          , distributedOutputs = distributedUTxO
          , remainingOutputs = remaining
          , chainState = newChainState
          , mode = AwaitingSelection
          }
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while in 'PartialFanout'. Updates the remaining and
-- distributed sets and, depending on the current 'FanoutMode', either continues
-- draining automatically (or within the active selection) or waits for the next
-- 'PartialFanout' command.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  HeadProgram tx ()
onPartialFanoutChainPartialFanoutTx pfs newChainState observedDistributed =
  let observedOutputs = outputsOfUTxO observedDistributed
      remaining = removeDistributedOutputs observedOutputs remainingOutputs
      distributedUTxO = withoutUTxO remainingOutputs remaining
      newMode = case mode of
        AutoDrain -> AutoDrain
        AwaitingSelection -> AwaitingSelection
        DistributingSelection sel ->
          let sel' = removeDistributedOutputs observedOutputs sel
           in if nullOutputs sel' then AwaitingSelection else DistributingSelection sel'
      record =
        Program.change
          HeadPartialFannedOut
            { headId
            , distributedOutputs = distributedUTxO
            , remainingOutputs = remaining
            , chainState = newChainState
            , mode = newMode
            }
      -- Already in 'FanoutProgress' on chain, so any continuation may finalize.
      finalize = emitStep (nextFanoutStep confirmedSnapshot version remaining remaining DatumFanoutProgress)
      continue
        -- The head's remaining set is now empty: emit the final (burning) step.
        -- This must happen regardless of mode — otherwise a selection that
        -- drains everything would stop at 'AwaitingSelection' and wedge the
        -- head, never burning the tokens.
        | nullOutputs remaining = finalize
        | otherwise = case newMode of
            AutoDrain -> finalize
            DistributingSelection sel' ->
              emitStep (nextFanoutStep confirmedSnapshot version sel' remaining DatumFanoutProgress)
            AwaitingSelection -> pure ()
   in record >> continue
 where
  emitStep step = emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, mode} = pfs

-- | Observe the final fanout while in 'PartialFanout', finalizing the head with
-- the accumulated distributed outputs plus this final batch.
--
-- __Transition__: 'PartialFanoutState' → 'IdleState'
onPartialFanoutChainFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  HeadProgram tx ()
onPartialFanoutChainFanoutTx pfs newChainState fanoutUTxO =
  Program.change HeadFannedOut{headId, finalizedOutputs = distributedOutputs <> fanoutUTxO, chainState = newChainState}
 where
  PartialFanoutState{headId, distributedOutputs} = pfs

-- | Compute the full UTxO set to be fanned out, combining snapshot utxo
-- with utxoToCommit/utxoToDecommit based on version.
computeFullFanoutUTxO ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx
computeFullFanoutUTxO ClosedState{confirmedSnapshot, version} =
  fanoutUTxOFromSnapshot confirmedSnapshot version

-- | The fan-out-able UTxO of a confirmed snapshot at the given on-chain version:
-- the snapshot UTxO plus a pending commit (if the increment landed on chain) or a
-- pending decommit (if the decrement has not landed yet).
fanoutUTxOFromSnapshot ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  UTxOType tx
fanoutUTxOFromSnapshot confirmedSnapshot version =
  combinedUTxO utxo effectiveCommit effectiveDecommit
 where
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit

-- | Build the 'PartialFanout' ('FanoutProgress') head state from a closed head,
-- carrying over the snapshot/parameters and using the given chain state, remaining
-- and distributed UTxO and fanout 'mode'. Shared by the three @Closed →
-- FanoutProgress@ transitions in 'aggregateNodeState'.
closedToFanoutProgress ::
  ClosedState tx ->
  ChainStateType tx ->
  UTxOType tx ->
  UTxOType tx ->
  FanoutMode tx ->
  HeadState tx
closedToFanoutProgress closedState chainState remaining distributed mode =
  FanoutProgress
    PartialFanoutState
      { parameters
      , confirmedSnapshot
      , contestationDeadline
      , chainState
      , headId
      , headSeed
      , version
      , remainingOutputs = remaining
      , distributedOutputs = distributed
      , mode
      }
 where
  ClosedState{parameters, confirmedSnapshot, contestationDeadline, headId, headSeed, version} = closedState

-- | Rebuild the 'ClosedState' from a 'PartialFanoutState' when reverting an
-- optimistic 'Closed' → 'PartialFanout' transition (see 'HeadFanoutReverted').
-- 'readyToFanoutSent' is restored to 'True' because a fanout is only reachable
-- after the head was announced 'ReadyToFanout'. The contestation period is over
-- by then, so no contest can confirm an 'unconfirmedSnapshot' anymore.
fanoutProgressToClosed :: PartialFanoutState tx -> ClosedState tx
fanoutProgressToClosed pfs =
  ClosedState
    { parameters
    , confirmedSnapshot
    , contestationDeadline
    , readyToFanoutSent = True
    , chainState
    , headId
    , headSeed
    , version
    , unconfirmedSnapshot = Nothing
    }
 where
  PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version} = pfs

-- | The step this node is currently driving. 'Nothing' while a manual fanout
-- waits on the next selection, where nothing is pending.
currentFanoutStep :: IsTx tx => PartialFanoutState tx -> Maybe (NextFanoutStep tx)
currentFanoutStep PartialFanoutState{confirmedSnapshot, version, remainingOutputs, distributedOutputs, mode} =
  case mode of
    AwaitingSelection -> Nothing
    AutoDrain -> Just (step remainingOutputs)
    DistributingSelection selection -> Just (step selection)
 where
  step target = nextFanoutStep confirmedSnapshot version target remainingOutputs onChainDatum

  onChainDatum = onChainFanoutDatum distributedOutputs

-- | Whether a posted transaction is the one a given step posts, telling a failure
-- of the step this node is driving from one it has already moved past.
--
-- Compares the distributed set, not just the transaction shape, since two
-- selections can both be non-final steps. By content, because naming the same
-- outputs under other inputs is a valid way to ask for the same set.
matchesFanoutStep :: IsTx tx => NextFanoutStep tx -> PostChainTx tx -> Bool
matchesFanoutStep step postChainTx =
  case (step, postChainTx) of
    -- Only one full fanout is ever in flight: it covers the whole head.
    (FullFanoutStep, FanoutTx{}) -> True
    (FinalStep{stepDistribute}, FinalPartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    (PartialStep{stepDistribute}, PartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    _ -> False

removeDistributedOutputs :: IsTx tx => [TxOutType tx] -> UTxOType tx -> UTxOType tx
removeDistributedOutputs = flip (foldl' (flip removeOneOutputFromUTxO))

-- | Whether a UTxO has no outputs.
nullOutputs :: IsTx tx => UTxOType tx -> Bool
nullOutputs = null . outputsOfUTxO

-- | Whether the outputs of @sub@ are a sub-multiset (by content) of @sup@. This
-- mirrors how partial fanout tracks distributed UTxO by content rather than by
-- 'TxIn', so a user-provided selection is validated against what is actually
-- still in the head.
isSubMultisetOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
isSubMultisetOf sub sup =
  length (outputsOfUTxO (removeDistributedOutputs (outputsOfUTxO sub) sup))
    == length (outputsOfUTxO sup)
    - length (outputsOfUTxO sub)

-- | Whether two UTxO sets have the same outputs (by content, as a multiset).
-- The @a == b@ short-circuit avoids the O(n²) multiset comparison in the common
-- case where both arguments are the same tracked set (e.g. the auto-drain
-- @sameOutputs remaining remaining@ check on every observed chunk), which is
-- exactly the large-UTxO heads partial fanout targets.
sameOutputs :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
sameOutputs a b =
  a == b || (length (outputsOfUTxO a) == length (outputsOfUTxO b) && a `isSubMultisetOf` b)

-- | Which on-chain head datum the next fanout step will be posted against:
-- still @Closed@ (no partial fanout has landed yet) or already @FanoutProgress@.
-- A 'FinalPartialFanoutTx' (which burns the head tokens) is only valid once the
-- datum is 'DatumFanoutProgress'.
data OnChainFanoutDatum = DatumClosed | DatumFanoutProgress

-- | The on-chain datum implied by how much has been distributed so far: still
-- @Closed@ while nothing has landed, @FanoutProgress@ once some has.
onChainFanoutDatum :: IsTx tx => UTxOType tx -> OnChainFanoutDatum
onChainFanoutDatum distributed
  | nullOutputs distributed = DatumClosed
  | otherwise = DatumFanoutProgress

-- | The mode a recorded selection leaves the driver in. A selection covering the
-- whole remainder with nothing distributed yet is not distributed as a selection
-- at all: 'nextFanoutStep' routes it to a full fanout and the driver auto-drains
-- the rest, so that is the mode it has to be recorded under.
--
-- 'fanoutStepStateChange' never pairs the two - such a step is a
-- 'FullFanoutStep', recorded as 'HeadFanoutInitiated' - so this only normalises
-- a selection recorded before that routing existed and replayed here. Without it
-- the re-posts ('repostFanoutStep') would post the full fanout that
-- 'nextFanoutStep' decides on while the mode kept claiming a selection is being
-- distributed, which is what every 'HeadPartiallyFannedOut' then reports.
recordedSelectionMode ::
  IsTx tx =>
  -- | Distributed so far
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  -- | The recorded selection
  UTxOType tx ->
  FanoutMode tx
recordedSelectionMode distributed remaining selection
  | nullOutputs distributed && selection `sameOutputs` remaining = AutoDrain
  | otherwise = DistributingSelection selection

-- | Post the transaction a decided 'NextFanoutStep' calls for. The chain layer
-- chunks a full fanout, and sizes a partial one, dynamically.
--
-- Transactions only: any accompanying state change belongs to the caller that
-- owns the head state, via 'fanoutStepStateChange'. That keeps this safe to call
-- where the caller owns the event ordering. Startup recovery uses the pure
-- transaction constructor 'fanoutStepTx'.
emitFanoutStep ::
  IsTx tx =>
  -- | The step to emit, from 'nextFanoutStep'.
  NextFanoutStep tx ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  HeadProgram tx ()
emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline =
  Program.emit $ OnChainEffect $ fanoutStepTx step confirmedSnapshot version headSeed contestationDeadline

-- | Construct the same fanout transaction for input handling and startup recovery.
fanoutStepTx :: IsTx tx => NextFanoutStep tx -> ConfirmedSnapshot tx -> SnapshotVersion -> HeadSeed -> UTCTime -> PostChainTx tx
fanoutStepTx step confirmedSnapshot version headSeed contestationDeadline =
  case step of
    FinalStep{stepDistribute} ->
      FinalPartialFanoutTx{utxoToDistribute = stepDistribute, headSeed, contestationDeadline}
    FullFanoutStep ->
      mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline
    PartialStep{stepDistribute, stepProof} ->
      PartialFanoutTx{utxoToDistribute = stepDistribute, utxoForProof = stepProof, headSeed, contestationDeadline}

-- | Re-post the next fanout step after a chain rollback while in
-- 'FanoutProgress', so the fanout resumes instead of stalling with the
-- rolled-back transaction gone and nothing re-posted. This mirrors the
-- Increment/Decrement re-post on rollback ('maybeRepostIncrementTx' /
-- 'maybeRepostDecrementTx'): it uses the current best-effort bookkeeping and,
-- like those re-posts, assumes the rolled-back transactions re-appear — it does
-- not attempt to reconstruct fanout progress across a divergent rollback (the
-- same limitation the general rollback handling has).
--
-- Which step that is comes from 'currentFanoutStep', the same derivation the
-- revert guard uses to tell this step's failure from a superseded one's.
repostFanoutStep :: IsTx tx => PartialFanoutState tx -> HeadProgram tx ()
repostFanoutStep pfs =
  traverse_ (Program.emit . OnChainEffect) (repostFanoutTx pfs)

-- | Startup recovery needs only the transaction, without interpreting a handler
-- or producing state changes outside the input loop.
repostFanoutTx :: IsTx tx => PartialFanoutState tx -> Maybe (PostChainTx tx)
repostFanoutTx pfs = do
  step <- currentFanoutStep pfs
  pure $ fanoutStepTx step confirmedSnapshot version headSeed contestationDeadline
 where
  PartialFanoutState{confirmedSnapshot, version, headSeed, contestationDeadline} = pfs

-- | Detect our view of the chain going out of sync and issue a 'NodeUnsynced'
-- event when this is the case.
handleOutOfSync ::
  IsChainState tx =>
  Environment ->
  -- | Current system time
  UTCTime ->
  -- | Latest Chain point observed
  ChainPointType tx ->
  -- | Latest Chain point time representation observed
  UTCTime ->
  SyncedStatus ->
  HeadProgram tx ()
handleOutOfSync Environment{unsyncedPeriod} now chainPoint chainTime syncStatus =
  -- Emit only on an actual sync-status transition, rather than on every tick, so
  -- clients are not flooded (see issue #2749). The continuous drift value is
  -- exposed as a metric ('hydra_chain_drift_seconds') instead.
  case (syncStatus, newSyncStatus) of
    (InSync, CatchingUp) -> Program.change NodeUnsynced{chainSlot, chainTime, drift}
    (CatchingUp, InSync) -> Program.change NodeSynced{chainSlot, chainTime, drift}
    _ -> pure ()
 where
  plus = flip addUTCTime
  chainSlot = chainPointSlot chainPoint

  threshold = unsyncedPeriodToNominalDiffTime unsyncedPeriod
  drift = now `diffUTCTime` chainTime

  -- We consider the node out of sync when:
  -- the last observed chainTime plus the delta allowed by the unsyncedPeriod (threshold)
  -- falls behind the current system time (now).
  -- NOTE: this is the same as drift > threshold
  nodeOutOfSync = chainTime `plus` threshold < now
  newSyncStatus = if nodeOutOfSync then CatchingUp else InSync

-- | The pending deposit that a local 'currentDepositTxId' still refers to, if any: the deposit must
--   be registered in 'pendingDeposits' and not 'Expired'.
--
--   Being registered and unexpired is what makes a recorded deposit id something the head may still
--   act on, and nothing else should be treated as a commit in flight. Neither of the two ways a
--   deposit stops being pending clears 'currentDepositTxId': 'DepositExpired' deliberately keeps the
--   deposit in the map so it can still be recovered, and 'DepositRecovered' only deletes the map
--   entry. So a caller that reads 'currentDepositTxId' on its own can end up waiting on a deposit
--   that is unclaimable, or already gone, and that wait never resolves.
existingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx, Deposit tx)
existingDeposit pendingDeposits currentDeposit =
  case currentDeposit of
    Nothing -> Nothing
    Just depositTxId ->
      case Map.lookup depositTxId pendingDeposits of
        Nothing -> Nothing
        Just deposit
          | deposit.status == Expired -> Nothing
          | otherwise -> Just (depositTxId, deposit)

-- | Find the oldest non-empty active deposit the request being built can
-- absorb ('absorbable'), if any. Deposits are selected in FIFO order by their
-- 'created' timestamp.
nextActiveDepositId :: IsTx tx => (UTxOType tx -> Bool) -> PendingDeposits tx -> Maybe (TxIdType tx)
nextActiveDepositId absorbableHere deposits =
  case filter (\(_, Deposit{deposited, status}) -> deposited /= mempty && status == Active && absorbableHere deposited) (Map.toList deposits) of
    [] -> Nothing
    xs -> Just (fst (minimumBy (comparing ((.created) . snd)) xs))

-- | Select the deposit to include in the next snapshot.
--
-- Prefers a deposit already tracked in 'currentDepositTxId' (if still pending).
-- Falls back to the oldest active deposit from 'pendingDeposits' the request
-- can absorb ('absorbable'), but only when neither a decommit is pending nor
-- the last confirmed snapshot's commit is still settling (to avoid
-- double-posting 'IncrementTx' before 'CommitFinalized' removes the deposit).
selectNextDeposit ::
  IsTx tx =>
  -- | Whether the request being built can absorb a deposit of these outputs,
  -- see 'absorbable'.
  (UTxOType tx -> Bool) ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  Maybe (TxIdType tx)
selectNextDeposit absorbableHere pendingDeposits currentDepositTxId mDecommitTx =
  (existingDeposit pendingDeposits currentDepositTxId >>= tracked)
    <|> (guard (isNothing mDecommitTx) *> nextActiveDepositId absorbableHere pendingDeposits)
 where
  tracked (depositTxId, Deposit{deposited}) = depositTxId <$ guard (deposited /= mempty && absorbableHere deposited)

-- | Reject a decommit that materializes no output.
-- 'Hydra.Contract.Head.checkDecrement' requires at least one, so such a decommit
-- can never settle on-chain, and recording it would block every later snapshot
-- (which cannot carry a different one).
--
-- Belongs after the applicability check at every call site: a transaction that
-- does not apply is reported with the ledger's own, more precise reason.
requireDecommitOutputs ::
  IsTx tx =>
  HeadId ->
  UTxOType tx ->
  tx ->
  HeadProgram tx () ->
  HeadProgram tx ()
requireDecommitOutputs headId localUTxO decommitTx continue
  | utxoFromTx decommitTx == mempty =
      Program.change
        DecommitInvalid
          { headId
          , decommitTx
          , decommitInvalidReason =
              ServerOutput.DecommitTxInvalid
                { localUTxO
                , validationError = ValidationError "decommit transaction has no outputs"
                }
          }
  | otherwise = continue

-- | What a leader puts in its next 'ReqSn', see 'nextSnapshotRequest'.
data SnapshotRequest tx = SnapshotRequest
  { requestTxs :: [tx]
  , requestDecommitTx :: Maybe tx
  , requestDepositTxId :: Maybe (TxIdType tx)
  }

-- | The content of a leader's next 'ReqSn': the transactions to carry and the
-- incremental action, a commit or a decommit, never both. A snapshot carrying
-- both cannot be closed, since close and fanout express a single incremental
-- action ('setIncrementalActionMaybe').
--
-- A commit wins: its deposit expires on-chain, while a decommit only waits. This
-- cannot starve the decommit, because 'selectNextDeposit' refuses to start a
-- *new* commit while a decommit is pending — only one already in flight can win,
-- and that one stops being selected once it settles and leaves 'pendingDeposits'.
--
-- Leaders never build a request followers reject: a leader's own rejection
-- would stall the head. A settling commit is always carried; txs colliding
-- with it are held back.
nextSnapshotRequest ::
  IsTx tx =>
  -- | The UTxO 'onOpenNetworkReqSn' applies the request's transactions to.
  UTxOType tx ->
  -- | Pending local transactions.
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | The commit the confirmed snapshot is settling, see 'settlingCommitAt'
  Maybe (SettlingCommit tx) ->
  SnapshotRequest tx
nextSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit =
  fst (buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit)

-- | 'nextSnapshotRequest', if worth sending right away: it carries a tx or a
-- new deposit, or it prunes a pending tx no node accepts. A request carrying
-- only a settling commit or a decommit is not: it would be re-requested
-- forever at the same version.
chainedSnapshotRequest ::
  IsTx tx =>
  UTxOType tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  Maybe tx ->
  Maybe (SettlingCommit tx) ->
  Maybe (SnapshotRequest tx)
chainedSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit =
  request <$ guard worthChaining
 where
  (request, worthChaining) = buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit

-- | The request and whether it is worth chaining, see 'chainedSnapshotRequest'.
buildSnapshotRequest ::
  forall tx.
  IsTx tx =>
  UTxOType tx ->
  Seq tx ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  Maybe tx ->
  Maybe (SettlingCommit tx) ->
  (SnapshotRequest tx, Bool)
buildSnapshotRequest utxo localTxs pendingDeposits currentDepositTxId mDecommitTx mSettlingCommit =
  case mSettlingCommit of
    Just (ContinueCommit (depositTxId, Deposit{deposited})) ->
      let requestTxs = requestable (utxo <> deposited)
       in (SnapshotRequest{requestTxs, requestDecommitTx = Nothing, requestDepositTxId = Just depositTxId}, not (null requestTxs) || prunesPending)
    Just DropCommit -> withoutDeposit
    Nothing ->
      case selectNextDeposit (absorbable utxo candidates) pendingDeposits currentDepositTxId mDecommitTx of
        Just depositTxId -> (SnapshotRequest{requestTxs = requestable utxo, requestDecommitTx = Nothing, requestDepositTxId = Just depositTxId}, True)
        Nothing -> withoutDeposit
 where
  withoutDeposit :: (SnapshotRequest tx, Bool)
  withoutDeposit =
    let requestTxs = requestable utxo
     in (SnapshotRequest{requestTxs, requestDecommitTx = mDecommitTx, requestDepositTxId = Nothing}, not (null requestTxs) || prunesPending)

  -- The first pending tx collides with the ledger; only a snapshot prunes it.
  prunesPending :: Bool
  prunesPending = case candidates of
    tx : _ -> not (namesNoneOf (utxoFromTx tx) utxo)
    [] -> False

  -- The pending txs up to the first one creating an output the ledger or the
  -- carried commit holds: no node accepts that one.
  requestable :: UTxOType tx -> [tx]
  requestable held = takeWhile (\tx -> namesNoneOf (utxoFromTx tx) held) candidates

  -- The first 'maxTxsPerSnapshot' pending txs: a snapshot request lists at
  -- most that many.
  candidates :: [tx]
  candidates = toList (Seq.take maxTxsPerSnapshot localTxs)

-- | The deposit claimed by a retained finalized increment ('finalizedCommit'),
-- if any. Such a deposit only resurfaces in 'pendingDeposits' when a rollback
-- erased its increment, and it is settled solely by re-posting that increment
-- ('maybeRepostIncrementTx'): it must never be proposed for a snapshot again
-- (the deposited funds are already counted in the head) nor recovered (that
-- would corrupt the L2 ledger). See #2741.
finalizedDepositTxId :: IsTx tx => Maybe (FinalizedSnapshot tx) -> Maybe (TxIdType tx)
finalizedDepositTxId finalizedCommit = do
  FinalizedSnapshot{snapshot} <- finalizedCommit
  (getSnapshot snapshot).depositTxId

-- | The deposits the handlers of an open head may act on: scoped to the head,
-- excluding the deposit claimed by the retained finalized increment (see
-- 'finalizedDepositTxId').
scopedDeposits :: IsTx tx => OpenState tx -> PendingDeposits tx -> PendingDeposits tx
scopedDeposits OpenState{headId, coordinatedHeadState} =
  maybe id Map.delete (finalizedDepositTxId coordinatedHeadState.finalizedCommit) . depositsForHead headId

-- | What the next request does with a commit the confirmed snapshot claims but
-- whose increment has not landed. Leaders and followers decide it the same way.
data SettlingCommit tx
  = -- | The increment may still land: every request must carry the commit.
    ContinueCommit (TxIdType tx, Deposit tx)
  | -- | The increment can no longer land: the deposit was recovered, or its
    -- deadline passed by more than 'depositPeriod' (a margin for rollbacks).
    -- 'Expired' is not enough: it is set before the deadline.
    DropCommit

-- | A carried incremental action remains pending at the snapshot's version.
hasPendingSnapshotAction :: SnapshotVersion -> SnapshotVersion -> Maybe a -> Bool
hasPendingSnapshotAction headVersion snapshotVersion action =
  headVersion == snapshotVersion && isJust action

settlingCommitAt :: IsTx tx => Environment -> SnapshotVersion -> UTCTime -> PendingDeposits tx -> Snapshot tx -> Maybe (SettlingCommit tx)
settlingCommitAt Environment{depositPeriod} = settlingCommitAtPeriod depositPeriod

settlingCommitAtPeriod :: IsTx tx => DepositPeriod -> SnapshotVersion -> UTCTime -> PendingDeposits tx -> Snapshot tx -> Maybe (SettlingCommit tx)
settlingCommitAtPeriod depositPeriod version chainTime pendingDeposits Snapshot{utxoToCommit, depositTxId, version = snapshotVersion}
  | not (hasPendingSnapshotAction version snapshotVersion utxoToCommit) = Nothing
  | otherwise = Just $ case (\claimed -> (claimed,) <$> Map.lookup claimed pendingDeposits) =<< depositTxId of
      Just claimed@(_, Deposit{deadline})
        | chainTime <= addUTCTime (toNominalDiffTime depositPeriod) deadline -> ContinueCommit claimed
      -- Also for snapshots without a 'depositTxId' (before 2.4.0).
      _ -> DropCommit

-- | The UTxO the next request is validated against: the snapshot in flight, if
-- any, else the confirmed one.
nextRequestUTxO :: IsTx tx => CoordinatedHeadState tx -> UTxOType tx
nextRequestUTxO CoordinatedHeadState{seenSnapshot, confirmedSnapshot, version} =
  case seenSnapshot of
    SeenSnapshot{snapshot} -> settledUTxO version snapshot
    _ -> settledUTxO version (getSnapshot confirmedSnapshot)

-- | Whether a request applying these txs to this UTxO can absorb the deposit:
-- it must name no output the UTxO holds or the txs create. Deposit datums are
-- not authenticated, so anyone can name existing outputs, and absorbing such a
-- deposit would count value the ledger does not have.
absorbable :: IsTx tx => UTxOType tx -> [tx] -> UTxOType tx -> Bool
absorbable utxo txs =
  let created = foldMap utxoFromTx txs
   in \deposited -> namesNoneOf deposited utxo && namesNoneOf deposited created

-- | Whether a deposit of the given outputs names none of the output references
-- the given UTxO holds.
namesNoneOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
namesNoneOf deposited utxo =
  -- NOTE: remove the UTxO from the deposit, not the deposit from the UTxO: the
  -- deposit is small, so this is cheaper.
  deposited `withoutUTxO` utxo == deposited

-- | Apply txs, refusing one that creates an output the UTxO already holds.
-- Impossible on L1, possible on L2: a deposit can name a tx's outputs before
-- that tx is submitted.
applyTransactionsWithoutCollision :: IsTx tx => Ledger tx -> ChainSlot -> UTxOType tx -> [tx] -> Either (tx, ValidationError) (UTxOType tx)
applyTransactionsWithoutCollision Ledger{applyTransactions = apply} slot =
  foldlM $ \utxo tx -> do
    first (tx,) $ checkTransactionOutputs utxo tx
    apply slot utxo [tx]

-- | The collision guard is a rule; the interpreter only applies one transaction.
applyTransactionsProgram :: IsTx tx => ChainSlot -> UTxOType tx -> [tx] -> SnapshotProgram tx (Either (tx, ValidationError) (UTxOType tx))
applyTransactionsProgram slot = go
 where
  go u [] = pure $ Right u
  go u (tx : rest) = do
    let collision = checkTransactionOutputs u tx
    void $ Program.decide "snapshot transaction outputs are fresh" (isRight collision)
    result <- case collision of
      Left err -> pure $ Left err
      Right () -> Program.applyTransaction slot u tx
    case result of
      Left err -> pure $ Left (tx, err)
      Right u' -> go u' rest

-- | Shared admission check for transaction requests and snapshot application.
checkTransactionOutputs :: IsTx tx => UTxOType tx -> tx -> Either ValidationError ()
checkTransactionOutputs utxo tx
  | namesNoneOf (utxoFromTx tx) utxo = Right ()
  | otherwise = Left $ ValidationError "transaction creates an output the head already holds"

-- | The deposit blocked by a retained finalized increment of an open head, if
-- any.
--
-- Deliberately 'Nothing' for any other head state: an increment can only
-- settle into an open head, so once the head closes the retained snapshot can
-- never claim the deposit on-chain anymore. The resurfaced deposit's escape
-- hatch is then to recover it (after its deadline) and exclude the deposited
-- outputs from the fanout via 'PartialFanout' — so 'Recover' must not stay
-- blocked after close.
openFinalizedDeposit :: IsTx tx => HeadState tx -> Maybe (TxIdType tx)
openFinalizedDeposit = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{finalizedCommit}} -> finalizedDepositTxId finalizedCommit
  _ -> Nothing

-- | Update the retained 'finalizedCommit' on (re-)observation of an increment
-- claiming @depositTxId@:
--
--   * the locally confirmed snapshot claims it: retain that snapshot — only it
--     can re-claim the deposit if a rollback erases the increment, and
--     'confirmedSnapshot' may advance past it;
--   * the already retained snapshot claims it: the increment re-landed (e.g.
--     re-posted after a rollback) — re-stamp the observation slot, so a
--     rollback of the re-landed increment still triggers a re-post;
--   * neither: the observation raced local snapshot confirmation (another
--     party collected the last AckSn and posted first) — retention happens
--     once the snapshot confirms, see the 'SnapshotConfirmed' branch of
--     'applyEvent'.
--
-- See #2741.
retainFinalizedCommit ::
  IsTx tx =>
  ChainSlot ->
  TxIdType tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  Maybe (FinalizedSnapshot tx)
retainFinalizedCommit slot depositTxId confirmedSnapshot retained
  | claims confirmedSnapshot = Just FinalizedSnapshot{snapshot = confirmedSnapshot, observedAtSlot = slot}
  | Just f@FinalizedSnapshot{snapshot} <- retained, claims snapshot = Just f{observedAtSlot = slot}
  | otherwise = retained
 where
  claims s = (getSnapshot s).depositTxId == Just depositTxId

-- | Update the retained 'finalizedDecommit' on (re-)observation of a decrement
-- bumping to @newVersion@; the counterpart of 'retainFinalizedCommit'. A
-- snapshot is tied to the decrement by identity, not by merely carrying a
-- decommit: only the decrement of that very snapshot bumps the version to
-- exactly one past the snapshot's (with a second decommit racing, the locally
-- confirmed snapshot may still be the previous, already settled one).
retainFinalizedDecommit ::
  IsTx tx =>
  ChainSlot ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  Maybe (FinalizedSnapshot tx)
retainFinalizedDecommit slot newVersion confirmedSnapshot retained
  | settles confirmedSnapshot = Just FinalizedSnapshot{snapshot = confirmedSnapshot, observedAtSlot = slot}
  | Just f@FinalizedSnapshot{snapshot} <- retained, settles snapshot = Just f{observedAtSlot = slot}
  | otherwise = retained
 where
  settles s = isJust (getSnapshot s).utxoToDecommit && (getSnapshot s).version + 1 == newVersion

-- | Handles inputs and converts them into 'StateChanged' events along with
-- 'Effect's, in case it is processed successfully. Later, the Node will
-- apply the events via 'aggregateNodeState', resulting in a new 'NodeState'.
update ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current system time.
  UTCTime ->
  -- | Current NodeState to validate the command against.
  NodeState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
update env ledger now nodeState ev =
  Interpreter.runProgramWith (Interpreter.snapshotConfig env ledger) $
    updateProgram env now nodeState ev

updateProgram ::
  IsChainState tx =>
  Environment ->
  -- | Current system time.
  UTCTime ->
  -- | Current NodeState to validate the command against.
  NodeState tx ->
  -- | Input to be processed.
  Input tx ->
  HeadProgram tx ()
updateProgram env now nodeState ev
  -- SECURITY: before anything else, in every head state and whether or not the
  -- node is in sync. An accumulator over more elements than the trusted setup
  -- supports has no commitment at all -- forcing it calls 'error', see
  -- 'Hydra.Tx.Accumulator.checkAccumulatorSize'. Almost every way this input
  -- can be turned away echoes it back to clients, and encoding that echo forces
  -- the accumulator: 'RejectedInputBecauseUnsynced' while catching up,
  -- 'CommandFailed' in a state that does not handle the command,
  -- 'SideLoadSnapshotRejected' from the open-state checks, 'UnhandledInput'.
  -- So an oversized snapshot has to be rejected here, ahead of all of them.
  --
  -- This is a backstop. The client API rejects such a snapshot before it is ever
  -- queued (see 'Hydra.API.ClientInput.validateClientInput'), which it must,
  -- since the node traces an input before the head logic sees it and tracing
  -- forces the accumulator too. That is also where a client gets a useful
  -- error. Reaching here means some new producer of 'SideLoadSnapshot' skipped
  -- that check, so this only has to be safe, not informative -- hence 'Error'
  -- via 'SideLoadSnapshotFailed', which carries the failure alone. Note this
  -- cannot go through 'sideLoadFailed': that emits a 'SideLoadSnapshotRejected'
  -- client message, which echoes the input.
  | ClientInput clientInput <- ev
  , Left AccumulatorTooLarge{utxoCount, maxAllowed} <- validateClientInput clientInput =
      Program.reject . SideLoadSnapshotFailed $ SideLoadUTxOSetTooLarge{utxoCount, maxAllowed}
  | otherwise =
      case nodeState of
        NodeCatchingUp{headState, chainPointTime} ->
          updateCatchingUpHead env now chainPointTime nodeState.pendingDeposits headState ev (syncedStatus nodeState)
        NodeInSync{headState, chainPointTime} ->
          updateInSyncHead env now chainPointTime nodeState.pendingDeposits headState ev (syncedStatus nodeState)

updateCatchingUpHead ::
  IsChainState tx =>
  Environment ->
  -- | Current system time.
  UTCTime ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  HeadProgram tx ()
updateCatchingUpHead env now chainPointTime pendingDeposits st ev syncStatus =
  case ev of
    ChainInput{} ->
      handleChainInput env now chainPointTime pendingDeposits st ev syncStatus
    ClientInput{clientInput} ->
      Program.emit . ClientEffect $ ServerOutput.RejectedInputBecauseUnsynced clientInput drift
    NetworkInput{} ->
      Program.suspend WaitOnNodeInSync{currentSlot}
 where
  ChainPointTime{currentSlot, drift} = chainPointTime

updateInSyncHead ::
  IsChainState tx =>
  Environment ->
  -- | Current system time.
  UTCTime ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  HeadProgram tx ()
updateInSyncHead env now chainPointTime pendingDeposits st ev syncStatus =
  case ev of
    ChainInput{} ->
      handleChainInput env now chainPointTime pendingDeposits st ev syncStatus
    ClientInput{} ->
      handleClientInput env chainPointTime pendingDeposits st ev
    NetworkInput{} ->
      handleNetworkInput env chainPointTime pendingDeposits st ev

-- * Input Handlers

handleChainInput ::
  IsChainState tx =>
  Environment ->
  -- | Current system time.
  UTCTime ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  SyncedStatus ->
  HeadProgram tx ()
handleChainInput env now _chainPointTime pendingDeposits st ev syncStatus = case (st, ev) of
  (Idle _, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState}) ->
    onIdleChainInitTx env newChainState headId headSeed headParameters participants
  -- Open
  ( Open openState@OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline, signatures}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline signatures
      | otherwise ->
          Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState, ChainInput Tick{chainTime, chainPoint}) ->
    -- XXX: We originally forgot the normal TickObserved state event here and so
    -- time did not advance in an open head anymore. This is a hint that we
    -- should compose event handling better.
    Program.change TickObserved{chainPoint, chainTime}
      >> handleOutOfSync env now chainPoint chainTime syncStatus
      >> onChainTick env pendingDeposits chainTime
      >> onOpenChainTick env chainTime (scopedDeposits openState pendingDeposits) openState
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId, snapshotNumber, signatures}, newChainState})
    | ourHeadId == headId ->
        onOpenChainIncrementTx env (scopedDeposits openState pendingDeposits) openState newChainState newVersion depositTxId snapshotNumber signatures
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO, snapshotNumber, signatures}, newChainState})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx env (scopedDeposits openState pendingDeposits) openState newChainState newVersion distributedUTxO snapshotNumber signatures
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline, signatures}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline signatures
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed ClosedState{contestationDeadline, readyToFanoutSent, headId}, ChainInput Tick{chainTime, chainPoint})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        Program.change TickObserved{chainPoint, chainTime}
          >> handleOutOfSync env now chainPoint chainTime syncStatus
          >> onChainTick env pendingDeposits chainTime
          >> Program.change HeadIsReadyToFanout{headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onClosedChainFanoutTx closedState newChainState fanoutUTxO
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onClosedChainPartialFanoutTx closedState newChainState distributedOutputs
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainPartialFanoutTx partialFanoutState newChainState distributedOutputs
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainFanoutTx partialFanoutState newChainState fanoutUTxO
    | otherwise ->
        Program.reject NotOurHead{ourHeadId, otherHeadId = headId}
  -- Node-level: deposit/recover observations scoped to our head
  ( Open OpenState{headId = ourHeadId, headSeed, parameters, coordinatedHeadState = CoordinatedHeadState{finalizedCommit}}
    , ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState}
    )
      | ourHeadId == headId ->
          Program.change DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
            -- The finalized deposit observed *again* means a rollback erased
            -- both the deposit and its finalized increment, and the deposit tx
            -- just re-landed on the new chain. The increment re-post issued at
            -- rollback time necessarily failed (the deposit UTxO did not exist
            -- on the new chain yet) and is not retried anywhere else, while
            -- every alternative settlement of this deposit is deliberately
            -- blocked (see 'finalizedDepositTxId'): re-post the increment
            -- now that the deposit is on chain again, see #2741.
            >> case finalizedCommit of
              Just FinalizedSnapshot{snapshot}
                | finalizedDepositTxId finalizedCommit == Just depositTxId ->
                    Program.emit
                      OnChainEffect
                        { postChainTx =
                            IncrementTx
                              { headSeed
                              , headId = ourHeadId
                              , headParameters = parameters
                              , incrementingSnapshot = snapshot
                              , depositTxId
                              }
                        }
              _ -> pure ()
      | otherwise ->
          pure ()
  (Closed ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        Program.change DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        pure ()
  -- Mirror the 'Closed' case while mid-fanout: a deposit observed during a
  -- (partial) fanout must still be recorded so it remains recoverable via
  -- 'Recover'. Without this the input falls through to 'Error' and is dropped.
  (FanoutProgress PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        Program.change DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        pure ()
  (Idle _, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    pure ()
  -- Deposit recovery is node-level: emit DepositRecovered for any tracked deposit
  -- regardless of which head is currently active. Previous-head deposits survive
  -- fanout in 'pendingDeposits', so recovery works even while a new head is Open.
  -- Unrelated deposits (never in pendingDeposits) are silently ignored.
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState})
    | Map.member recoveredTxId pendingDeposits ->
        Program.change DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
    | otherwise ->
        pure ()
  -- Open + Rollback: re-post IncrementTx/DecrementTx if they were in-flight
  -- or already finalized past the rolled-back slot (#2741)
  ( Open
      OpenState
        { headSeed
        , headId
        , parameters
        , coordinatedHeadState =
          CoordinatedHeadState
            { confirmedSnapshot
            , decommitTx
            , finalizedCommit
            , finalizedDecommit
            }
        }
    , ChainInput Rollback{rolledBackChainState, chainTime}
    ) ->
      Program.change ChainRolledBack{chainState = rolledBackChainState}
        >> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
        >> maybeRepostIncrementTx headSeed headId parameters (depositsForHead headId pendingDeposits) confirmedSnapshot finalizedCommit (chainStateSlot rolledBackChainState)
        >> maybeRepostDecrementTx headSeed headId parameters decommitTx confirmedSnapshot finalizedDecommit (chainStateSlot rolledBackChainState)
  -- FanoutProgress + Rollback: re-post the next fanout step so the fanout
  -- resumes rather than stalling (the in-flight fanout tx may have been rolled
  -- back). Mirrors the Open re-post above.
  (FanoutProgress partialFanoutState, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    Program.change ChainRolledBack{chainState = rolledBackChainState}
      >> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
      >> repostFanoutStep partialFanoutState
  -- General
  (_, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    Program.change ChainRolledBack{chainState = rolledBackChainState}
      >> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
  (_, ChainInput Tick{chainTime, chainPoint}) ->
    Program.change TickObserved{chainPoint, chainTime}
      >> handleOutOfSync env now chainPoint chainTime syncStatus
      >> onChainTick env pendingDeposits chainTime
  (_, ChainInput PostTxError{postTxError = StalePartialFanoutTx}) ->
    -- The chain advanced past this step before we could post it (another node
    -- was faster). The chain observation loop already emitted the correct next
    -- step, so this is safe to ignore.
    pure ()
  (FanoutProgress pfs, ChainInput PostTxError{postChainTx, postTxError})
    -- We optimistically moved 'Closed' → 'PartialFanout' when the fanout was
    -- initiated. If posting the initiating fanout tx fails terminally before
    -- anything has been distributed on chain (so the on-chain datum is still
    -- 'Closed'), revert to 'Closed' rather than wedging the head — otherwise
    -- 'Fanout' stays rejected and there is no clean way to recover. Once any
    -- partial fanout has landed ('distributedOutputs' non-empty) the on-chain
    -- datum is genuinely 'FanoutProgress', so we must not revert.
    --
    -- Only for a failure of the step this node is actually driving, of which
    -- 'AwaitingSelection' has none. A superseded transaction can still be in
    -- flight — a selection covering the whole remainder turns into a full fanout
    -- and leaves the earlier chunk behind — and reverting on its failure would
    -- drop the driver role for a transaction nobody is waiting on.
    | PartialFanoutState{headId, distributedOutputs} <- pfs
    , nullOutputs distributedOutputs
    , maybe False (`matchesFanoutStep` postChainTx) (currentFanoutStep pfs) ->
        Program.change HeadFanoutReverted{headId}
          >> Program.emit (ClientEffect ServerOutput.PostTxOnChainFailed{postChainTx, postTxError})
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    Program.emit . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  _ ->
    Program.reject $ UnhandledInput ev st

handleNetworkInput ::
  IsChainState tx =>
  Environment ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  HeadProgram tx ()
handleNetworkInput env chainPointTime pendingDeposits st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  -- Open
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env chainPointTime openState ttl (scopedDeposits openState pendingDeposits) tx
  -- NOTE: 'ReqSn' gets the unfiltered (per-head) deposits: it distinguishes a
  -- requested deposit that is blocked by a finalized increment (hard error)
  -- from one that is simply not known locally (wait).
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    onOpenNetworkReqSn env (depositsForHead ourHeadId pendingDeposits) chainPointTime openState ttl sender sv sn txIds decommitTx depositTxId
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    onOpenNetworkAckSn env chainPointTime (scopedDeposits openState pendingDeposits) openState sender snapshotSignature sn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ttl chainPointTime (scopedDeposits openState pendingDeposits) openState transaction
  _ ->
    Program.reject $ UnhandledInput ev st

onConnectionEvent :: Text -> Network.Connectivity -> HeadProgram tx ()
onConnectionEvent misconfiguredPeers event = case event of
  Network.NetworkConnected ->
    Program.change NetworkConnected
  Network.NetworkDisconnected ->
    Program.change NetworkDisconnected
  Network.VersionMismatch{ourVersion, theirVersion} ->
    Program.change NetworkVersionMismatch{ourVersion, theirVersion}
  Network.ClusterIDMismatch{clusterPeers} ->
    Program.change NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers}
  Network.PeerConnected{peer} ->
    Program.change PeerConnected{peer}
  Network.PeerDisconnected{peer} ->
    Program.change PeerDisconnected{peer}
  Network.BroadcastStalled{pendingBroadcasts, stallReason} ->
    Program.change NetworkBroadcastStalled{pendingBroadcasts, stallReason}
  Network.BroadcastResumed ->
    Program.change NetworkBroadcastResumed

handleClientInput ::
  IsChainState tx =>
  Environment ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  HeadProgram tx ()
handleClientInput env ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
  (Idle _, ClientInput Init) ->
    onIdleClientInit env
  -- Open
  (Open openState, ClientInput Close) ->
    onOpenClientClose openState
  (Open openState, ClientInput SafeClose) ->
    onOpenClientClose openState
  (Open{}, ClientInput (NewTx tx)) ->
    onOpenClientNewTx tx
  (Open openState@OpenState{headId = ourHeadId}, ClientInput (SideLoadSnapshot confirmedSnapshot)) ->
    let Snapshot{headId = otherHeadId} = getSnapshot confirmedSnapshot
     in if ourHeadId == otherHeadId
          then onOpenClientSideLoadSnapshot openState confirmedSnapshot
          else Program.reject NotOurHead{ourHeadId, otherHeadId}
  (Open OpenState{headId, coordinatedHeadState}, ClientInput Decommit{decommitTx}) -> do
    onOpenClientDecommit headId currentSlot coordinatedHeadState decommitTx
  -- Closed
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, ClientInput PartialFanout{utxoToFanout}) ->
    onClosedClientPartialFanout closedState utxoToFanout
  -- PartialFanout: once a partial fanout has started, only further
  -- 'PartialFanout' commands are accepted (a plain 'Fanout' falls through to the
  -- general 'CommandFailed' below).
  (FanoutProgress partialFanoutState, ClientInput PartialFanout{utxoToFanout}) ->
    onPartialFanoutClientPartialFanout partialFanoutState utxoToFanout
  -- Node-level
  (_, ClientInput Recover{recoverTxId}) -> do
    onClientRecover currentSlot pendingDeposits (openFinalizedDeposit st) recoverTxId
  -- General
  (_, ClientInput{clientInput}) ->
    Program.emit . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    Program.reject $ UnhandledInput ev st

-- * NodeState aggregate

-- | Reflect 'StateChanged' events onto the 'NodeState' aggregateNodeState.
-- Events carrying a 'HeadId' that does not match the current state are silently
-- ignored, preventing cross-head state contamination during event replay.
-- Events without a 'HeadId' are always applied.
aggregateNodeState :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
aggregateNodeState nodeState sc =
  case (headIdOf (headState nodeState), eventHeadId sc) of
    (Just sid, Just eid) | sid /= eid -> nodeState
    _ ->
      let st = applyEvent (headState nodeState) sc
          chainPointTimeState = chainPointTime nodeState
       in case sc of
            HeadOpened{chainState} ->
              nodeState
                { headState = st
                , chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}
                }
            DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
              recordDeposit (chainStateSlot chainState) depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} $
                nodeState{headState = st}
            DepositActivated{depositTxId, deposit} ->
              updateDeposit depositTxId deposit $
                nodeState{headState = st}
            DepositExpired{depositTxId, deposit} ->
              -- NB: We keep expired deposits since we actually need them when Recovering.
              -- There is a corresponding error RequestedDepositExpired which gives users context on stale ReqSn.
              updateDeposit depositTxId deposit $
                nodeState{headState = st}
            DepositRecovered{chainState, depositTxId} ->
              consumeDeposit (chainStateSlot chainState) depositTxId $
                case st of
                  Open os@OpenState{coordinatedHeadState} ->
                    nodeState
                      { headState =
                          Open
                            os
                              { coordinatedHeadState =
                                  coordinatedHeadState
                                    { currentDepositTxId =
                                        if coordinatedHeadState.currentDepositTxId == Just depositTxId
                                          then Nothing
                                          else coordinatedHeadState.currentDepositTxId
                                    }
                              }
                      }
                  _ ->
                    nodeState{headState = st}
            CommitFinalized{chainState, newVersion, depositTxId} ->
              consumeDeposit (chainStateSlot chainState) depositTxId $ case st of
                Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{localUTxO, confirmedSnapshot, seenSnapshot}}
                  -- Only convergence bookkeeping: in particular 'localUTxO'
                  -- must not absorb the deposit again (its outputs may have
                  -- been spent on L2 in the meantime and the union would
                  -- resurrect them) and an unrelated deposit already parked in
                  -- 'currentDepositTxId' for the next snapshot must be left
                  -- alone. See #2741.
                  | isReobservation newVersion chs ->
                      nodeState
                        { headState =
                            Open
                              os
                                { chainState
                                , coordinatedHeadState =
                                    chs
                                      { currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId
                                      , finalizedCommit = retainFinalizedCommit (chainStateSlot chainState) depositTxId confirmedSnapshot chs.finalizedCommit
                                      }
                                }
                        }
                  | otherwise ->
                      nodeState
                        { headState =
                            Open
                              os
                                { chainState
                                , coordinatedHeadState =
                                    chs
                                      { version = newVersion
                                      , -- NOTE: This must correspond to the just finalized
                                        -- depositTxId, but we should not verify this here.
                                        currentDepositTxId = Nothing
                                      , localUTxO = localUTxO <> maybe mempty (.deposited) (Map.lookup depositTxId nodeState.pendingDeposits)
                                      , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                                      , finalizedCommit = retainFinalizedCommit (chainStateSlot chainState) depositTxId confirmedSnapshot chs.finalizedCommit
                                      }
                                }
                        }
                _ ->
                  nodeState{headState = st}
            TickObserved{chainPoint, chainTime} ->
              nodeState{headState = st, chainPointTime = chainPointTimeState{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
            ChainRolledBack{chainState} ->
              -- Deposits are L1-derived: restore the view at the rolled-back
              -- slot. Deposits whose consuming tx (increment/recover) was
              -- erased resurface, deposits whose deposit tx was erased vanish;
              -- forward re-observation converges the view again. See #2741.
              rollbackDeposits (chainStateSlot chainState) $
                nodeState{headState = st, chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}}
            NodeUnsynced{chainSlot, chainTime, drift} ->
              NodeCatchingUp{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
            NodeSynced{chainSlot, chainTime, drift} ->
              NodeInSync{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
            -- Restore the full snapshot: a checkpoint carries the aggregated
            -- 'pendingDeposits' and 'chainPointTime' (and synced constructor),
            -- which the default arm below would otherwise drop on replay.
            Checkpoint checkpointedNodeState ->
              checkpointedNodeState
            _ ->
              nodeState{headState = st}

-- * HeadState aggregate helpers

-- | Whether a finalized increment or decrement at 'newVersion' was applied
-- before and re-landed after a rollback. The local version never rolls back.
isReobservation :: SnapshotVersion -> CoordinatedHeadState tx -> Bool
isReobservation newVersion CoordinatedHeadState{version} = newVersion <= version

-- | The 'SeenSnapshot' after a version bump. A snapshot already in flight is
-- kept: every party has processed its 'ReqSn', so it can still complete and
-- chain the next one with the bumped version.
seenSnapshotAfterVersionBump :: IsTx tx => ConfirmedSnapshot tx -> SeenSnapshot tx -> SeenSnapshot tx
seenSnapshotAfterVersionBump confirmedSnapshot = \case
  seen@SeenSnapshot{} -> seen
  _ -> LastSeenSnapshot{lastSeen = (getSnapshot confirmedSnapshot).number}

-- | The spendable UTxO of a snapshot, given the head's current 'SnapshotVersion'.
-- A pending commit only becomes spendable once its increment has landed on
-- chain, which bumps the version past the snapshot's.
settledUTxO :: IsTx tx => SnapshotVersion -> Snapshot tx -> UTxOType tx
settledUTxO headVersion Snapshot{utxo, utxoToCommit, version}
  | headVersion > version = utxo <> fromMaybe mempty utxoToCommit
  | otherwise = utxo

-- | Extract the 'HeadId' from a 'StateChanged' event, if the event carries one.
-- Events that do not carry a 'HeadId' always pass through 'aggregateNodeState' unchanged.
eventHeadId :: StateChanged tx -> Maybe HeadId
eventHeadId = \case
  HeadOpened{headId} -> Just headId
  TransactionAppliedToLocalUTxO{headId} -> Just headId
  SnapshotConfirmed{headId} -> Just headId
  LocalStateCleared{headId} -> Just headId
  DepositRecorded{headId} -> Just headId
  DepositRecovered{} -> Nothing
  CommitApproved{headId} -> Just headId
  CommitFinalized{headId} -> Just headId
  DecommitRecorded{headId} -> Just headId
  DecommitApproved{headId} -> Just headId
  DecommitInvalid{headId} -> Just headId
  DecommitFinalized{headId} -> Just headId
  HeadIsReadyToFanout{headId} -> Just headId
  HeadClosed{headId} -> Just headId
  HeadContested{headId} -> Just headId
  HeadFannedOut{headId} -> Just headId
  TxInvalid{headId} -> Just headId
  HeadPartialFannedOut{headId} -> Just headId
  HeadFanoutInitiated{headId} -> Just headId
  HeadPartialFanoutSelected{headId} -> Just headId
  HeadFanoutReverted{headId} -> Just headId
  -- The headId in IgnoredHeadInitializing is the OTHER head's id (not ours),
  -- so it must not be used to filter against the current head state.
  IgnoredHeadInitializing{} -> Nothing
  TransactionReceived{} -> Nothing
  SnapshotRequestDecided{} -> Nothing
  SnapshotRequested{} -> Nothing
  PartySignedSnapshot{} -> Nothing
  DepositActivated{} -> Nothing
  DepositExpired{} -> Nothing
  ChainRolledBack{} -> Nothing
  TickObserved{} -> Nothing
  NetworkDisconnected -> Nothing
  NetworkConnected -> Nothing
  PeerConnected{} -> Nothing
  PeerDisconnected{} -> Nothing
  NetworkVersionMismatch{} -> Nothing
  NetworkClusterIDMismatch{} -> Nothing
  NetworkBroadcastStalled{} -> Nothing
  NetworkBroadcastResumed -> Nothing
  Checkpoint{} -> Nothing
  NodeUnsynced{} -> Nothing
  NodeSynced{} -> Nothing

-- | Extract the 'HeadId' from the current 'HeadState', if any.
headIdOf :: HeadState tx -> Maybe HeadId
headIdOf = \case
  Idle _ -> Nothing
  Open OpenState{headId} -> Just headId
  Closed ClosedState{headId} -> Just headId
  FanoutProgress PartialFanoutState{headId} -> Just headId

applyEvent :: IsChainState tx => HeadState tx -> StateChanged tx -> HeadState tx
applyEvent st = \case
  NetworkConnected -> st
  NetworkDisconnected -> st
  NetworkVersionMismatch{} -> st
  NetworkClusterIDMismatch{} -> st
  NetworkBroadcastStalled{} -> st
  NetworkBroadcastResumed -> st
  PeerConnected{} -> st
  PeerDisconnected{} -> st
  HeadOpened{headSeed, headId, parameters, chainState} ->
    Open
      OpenState
        { headId
        , headSeed
        , parameters
        , coordinatedHeadState =
            CoordinatedHeadState
              { localUTxO = mempty
              , allTxs = mempty
              , localTxs = mempty
              , confirmedSnapshot = InitialSnapshot{headId}
              , seenSnapshot = NoSeenSnapshot
              , currentDepositTxId = Nothing
              , decommitTx = Nothing
              , version = 0
              , finalizedCommit = Nothing
              , finalizedDecommit = Nothing
              }
        , chainState
        }
  TransactionReceived{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { allTxs = Map.insert (txId tx) tx allTxs
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> st
  TransactionAppliedToLocalUTxO{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { localUTxO =
                      -- NOTE: Safe to use localUTxO here because the tx was
                      -- ledger-validated before this event was emitted.
                      -- 'aggregate' folds events in order, so 'localUTxO'
                      -- here always reflects all previously applied transactions.
                      applyTxTo tx localUTxO
                  , -- NOTE: Order of transactions is important here. See also
                    -- 'pruneTransactions'.
                    localTxs = localTxs Seq.|> tx
                  }
            }
       where
        CoordinatedHeadState{localUTxO, localTxs} = coordinatedHeadState
      _otherState -> st
  SnapshotRequestDecided{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot =
                      RequestedSnapshot
                        { lastSeen = seenSnapshotNumber seenSnapshot
                        , requested = snapshotNumber
                        }
                  }
            }
       where
        CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
      _otherState -> st
  SnapshotRequested{requestedSnapshot = snapshot, newLocalTxs, newCurrentDepositTxId} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot = mkSeenSnapshot snapshot mempty
                  , localTxs = newLocalTxs
                  , -- NOTE: pure UTxO arithmetic. 'newLocalTxs' was pre-pruned
                    -- by 'pruneTransactions' in 'onOpenNetworkReqSn' (so each tx
                    -- is guaranteed to apply), making 'applyTxTo' safe to use
                    -- without ledger validation.
                    localUTxO = foldl' (flip applyTxTo) (settledUTxO version snapshot) newLocalTxs
                  , allTxs = foldr (Map.delete . txId) allTxs snapshot.confirmed
                  , currentDepositTxId = newCurrentDepositTxId
                  }
            }
       where
        CoordinatedHeadState{allTxs, version} = coordinatedHeadState
      _otherState -> st
  PartySignedSnapshot{party, signature} ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState =
            chs@CoordinatedHeadState
              { seenSnapshot = ss@SeenSnapshot{signatories}
              }
          } ->
          Open
            os
              { coordinatedHeadState =
                  chs
                    { seenSnapshot =
                        ss
                          { signatories = Map.insert party signature signatories
                          }
                    }
              }
      _otherState -> st
  SnapshotConfirmed{snapshot = mSnapshot, signatures} ->
    case st of
      Open os@OpenState{chainState, coordinatedHeadState = chs@CoordinatedHeadState{seenSnapshot, version}} ->
        case mSnapshot <|> snapshotFromSeen seenSnapshot of
          Just snapshot ->
            -- The snapshot's settling transaction (increment/decrement) may
            -- have been observed on chain *before* this local confirmation
            -- (another party collected the last AckSn and posted first) —
            -- recognizable by 'version' already bumped one past the
            -- snapshot's. Nothing could be retained at observation time (see
            -- 'retainFinalizedCommit'), so retain now. The current chain slot
            -- over-approximates the observation slot, which errs towards
            -- re-posting on rollback (harmless noise) rather than staying
            -- silent when the settlement was erased.
            let confirmed = ConfirmedSnapshot{snapshot, signatures}
                retainIfSettled settles retained
                  | version == snapshot.version + 1 && settles =
                      Just FinalizedSnapshot{snapshot = confirmed, observedAtSlot = chainStateSlot chainState}
                  | otherwise = retained
             in Open
                  os
                    { coordinatedHeadState =
                        chs
                          { confirmedSnapshot = confirmed
                          , seenSnapshot = LastSeenSnapshot snapshot.number
                          , finalizedCommit =
                              retainIfSettled (isJust snapshot.utxoToCommit && isJust snapshot.depositTxId) chs.finalizedCommit
                          , finalizedDecommit =
                              retainIfSettled (isJust snapshot.utxoToDecommit) chs.finalizedDecommit
                          }
                    }
          -- Nothing to confirm: leave the state as it is rather than crashing
          -- the node, which would also happen on every replay of the event.
          Nothing -> st
      -- Only emitted, with the snapshot, for the 'unconfirmedSnapshot' adopted
      -- from the multisignature of an observed contest. Anything else leaves
      -- the closed head as it is, as it did before this case existed.
      Closed cst ->
        case mSnapshot of
          Just snapshot ->
            Closed
              cst
                { confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures}
                , unconfirmedSnapshot = Nothing
                }
          Nothing -> st
      _otherState -> st
   where
    snapshotFromSeen :: SeenSnapshot tx -> Maybe (Snapshot tx)
    snapshotFromSeen (SeenSnapshot sn _ _) = Just sn
    snapshotFromSeen _ = Nothing
  LocalStateCleared{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState = coordinatedHeadState@CoordinatedHeadState{confirmedSnapshot, version = currentVersion}} ->
        Open
          os
            { coordinatedHeadState =
                case confirmedSnapshot of
                  InitialSnapshot{} ->
                    coordinatedHeadState
                      { localUTxO = mempty
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = NoSeenSnapshot
                      }
                  ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToDecommit}} ->
                    coordinatedHeadState
                      { localUTxO = settledUTxO currentVersion snapshot
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = LastSeenSnapshot snapshotNumber
                      , -- A decommit the snapshot still settles is kept: every
                        -- request at this version must carry it (see
                        -- 'onOpenNetworkReqSn'), and a side-load cannot change
                        -- the pending decommit, so the recorded tx is the one
                        -- producing it. Any other is pruned like the pending txs.
                        decommitTx = do
                          tx <- coordinatedHeadState.decommitTx
                          guard (currentVersion == snapshot.version && Just (utxoFromTx tx) == utxoToDecommit)
                          pure tx
                      , currentDepositTxId = Nothing
                      }
            }
      _otherState -> st
  DepositRecorded{} -> st
  DepositActivated{depositTxId, deposit} -> case st of
    Open os@OpenState{headId = ourHeadId, coordinatedHeadState = chs}
      | deposit.headId == ourHeadId ->
          -- Spec: txω = ⊥ ∨ txα = ⊥ — deposit and decommit are mutually exclusive.
          -- Only queue the deposit when no decommit is pending; otherwise the tick
          -- will pick it up once the decommit completes.
          case chs.decommitTx of
            Just _ -> st
            Nothing
              -- The finalized deposit can be re-activated when a rollback rewinds
              -- the deposit view to before its activation: never park it in
              -- 'currentDepositTxId' — it is settled by re-posting the increment,
              -- not by a new snapshot, see 'finalizedDepositTxId' and #2741.
              | finalizedDepositTxId chs.finalizedCommit == Just depositTxId -> st
              -- Also do not park a deposit the next snapshot request cannot
              -- include, see 'absorbable'.
              | not (absorbable (nextRequestUTxO chs) (toList (Seq.take maxTxsPerSnapshot chs.localTxs)) deposit.deposited) -> st
              | otherwise -> Open os{coordinatedHeadState = chs{currentDepositTxId = chs.currentDepositTxId <|> Just depositTxId}}
    _ -> st
  DepositExpired{} -> st
  CommitApproved{} -> st
  DepositRecovered{} -> st
  CommitFinalized{} -> st
  DecommitRecorded{decommitTx} -> case st of
    Open
      os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { -- Apply the decommit to localUTxO and remove its outputs:
                    -- decommit's outputs leave the head, so net effect is
                    -- removing the spent inputs from localUTxO.
                    localUTxO = applyTxTo decommitTx localUTxO `withoutUTxO` utxoFromTx decommitTx
                  , decommitTx = Just decommitTx
                  }
            }
       where
        CoordinatedHeadState{localUTxO} = coordinatedHeadState
    _otherState -> st
  DecommitApproved{} -> st
  DecommitInvalid{} -> st
  DecommitFinalized{chainState, newVersion} ->
    case st of
      Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}}
        -- Only convergence bookkeeping: in particular an unrelated newer
        -- decommit may already be in flight and must be left alone. See #2741.
        | isReobservation newVersion chs ->
            Open
              os
                { chainState
                , coordinatedHeadState =
                    chs{finalizedDecommit = retainFinalizedDecommit (chainStateSlot chainState) newVersion confirmedSnapshot chs.finalizedDecommit}
                }
        | otherwise ->
            Open
              os
                { chainState
                , coordinatedHeadState =
                    chs
                      { decommitTx = Nothing
                      , version = newVersion
                      , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                      , -- Retain the signed decrementing snapshot: if a rollback
                        -- erases the just observed decrement, 'confirmedSnapshot'
                        -- may have advanced past it and this is the only snapshot
                        -- that can settle the decommit on-chain.
                        finalizedDecommit = retainFinalizedDecommit (chainStateSlot chainState) newVersion confirmedSnapshot chs.finalizedDecommit
                      }
                }
      _otherState -> st
  HeadClosed{chainState, contestationDeadline} ->
    case st of
      Open
        OpenState
          { parameters
          , coordinatedHeadState =
            CoordinatedHeadState
              { confirmedSnapshot
              , seenSnapshot
              , version
              }
          , headId
          , headSeed
          } ->
          Closed
            ClosedState
              { parameters
              , confirmedSnapshot
              , contestationDeadline
              , readyToFanoutSent = False
              , chainState
              , headId
              , headSeed
              , version
              , -- Keep the snapshot still collecting AckSns: a later contest
                -- may carry its full multisignature.
                unconfirmedSnapshot = case seenSnapshot of
                  SeenSnapshot{snapshot} -> Just snapshot
                  _ -> Nothing
              }
      _otherState -> st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version, unconfirmedSnapshot} ->
        Closed
          ClosedState
            { parameters
            , confirmedSnapshot
            , contestationDeadline
            , readyToFanoutSent
            , chainState
            , headId
            , headSeed
            , version
            , unconfirmedSnapshot
            }
      _otherState -> st
  HeadFannedOut{chainState} ->
    case st of
      Closed _ ->
        Idle $ IdleState{chainState}
      FanoutProgress _ ->
        Idle $ IdleState{chainState}
      _otherState -> st
  HeadFanoutInitiated{remainingOutputs} ->
    case st of
      -- This node initiated a full automatic fanout: become the driver in
      -- 'AutoDrain' mode so its observations auto-continue to completion.
      Closed cst@ClosedState{chainState} ->
        closedToFanoutProgress cst chainState remainingOutputs mempty AutoDrain
      -- A target covering the whole remainder before anything landed is a full
      -- fanout too ('nextFanoutStep'), so the driver switches to draining
      -- automatically.
      FanoutProgress pfs -> FanoutProgress pfs{mode = AutoDrain, remainingOutputs}
      _otherState -> st
  HeadPartialFanoutSelected{remainingOutputs, selection} ->
    case st of
      -- First selective partial fanout from a freshly closed head: enter the
      -- 'PartialFanout' state with nothing distributed yet.
      Closed cst@ClosedState{chainState} ->
        closedToFanoutProgress cst chainState remainingOutputs mempty (recordedSelectionMode mempty remainingOutputs selection)
      -- Continuing: just record the new active selection.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs} ->
        FanoutProgress pfs{mode = recordedSelectionMode distributedOutputs remainingOutputs selection}
      _otherState -> st
  HeadFanoutReverted{} ->
    case st of
      -- Roll the optimistic transition back: the initiating fanout tx failed to
      -- post and nothing landed on chain, so the head is really still 'Closed'.
      FanoutProgress pfs -> Closed (fanoutProgressToClosed pfs)
      _otherState -> st
  HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
    case st of
      -- First partial fanout observed by a passive observer: transition from
      -- 'Closed' into 'PartialFanout' (using the observed chain state).
      Closed cst ->
        closedToFanoutProgress cst chainState remainingOutputs newlyDistributed mode
      -- Subsequent steps: accumulate distributed outputs and update remaining/mode.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs = priorDistributed} ->
        FanoutProgress
          pfs
            { chainState
            , remainingOutputs
            , distributedOutputs = priorDistributed <> newlyDistributed
            , mode
            }
      _otherState -> st
  HeadIsReadyToFanout{} ->
    case st of
      Closed cst -> Closed cst{readyToFanoutSent = True}
      _otherState -> st
  ChainRolledBack{chainState} ->
    setChainState chainState st
  TickObserved{} -> st
  IgnoredHeadInitializing{} -> st
  TxInvalid{transaction} -> case st of
    Open ost@OpenState{coordinatedHeadState = coordState@CoordinatedHeadState{allTxs = allTransactions}} ->
      Open ost{coordinatedHeadState = coordState{allTxs = Map.delete (txId transaction) allTransactions}}
    _otherState -> st
  Checkpoint nodeState -> headState nodeState
  NodeSynced{} -> st
  NodeUnsynced{} -> st

aggregateState ::
  IsChainState tx =>
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState s outcome =
  foldl' aggregateNodeState s $ collectStateChanged outcome
 where
  collectStateChanged :: Outcome tx -> [StateChanged tx]
  collectStateChanged = \case
    Error{} -> []
    Wait{stateChanges} -> stateChanges
    Continue{stateChanges} -> stateChanges

aggregateChainStateHistory :: IsChainState tx => ChainStateHistory tx -> StateChanged tx -> ChainStateHistory tx
aggregateChainStateHistory history = \case
  NetworkConnected -> history
  NetworkDisconnected -> history
  NetworkVersionMismatch{} -> history
  NetworkClusterIDMismatch{} -> history
  NetworkBroadcastStalled{} -> history
  NetworkBroadcastResumed -> history
  PeerConnected{} -> history
  PeerDisconnected{} -> history
  HeadOpened{chainState} -> pushNewState chainState history
  TransactionAppliedToLocalUTxO{} -> history
  SnapshotRequestDecided{} -> history
  SnapshotRequested{} -> history
  TransactionReceived{} -> history
  PartySignedSnapshot{} -> history
  SnapshotConfirmed{} -> history
  DepositRecorded{chainState} -> pushNewState chainState history
  DepositActivated{} -> history
  DepositExpired{} -> history
  DepositRecovered{chainState} -> pushNewState chainState history
  CommitFinalized{chainState} -> pushNewState chainState history
  DecommitRecorded{} -> history
  DecommitFinalized{chainState} -> pushNewState chainState history
  HeadClosed{chainState} -> pushNewState chainState history
  HeadContested{chainState} -> pushNewState chainState history
  HeadIsReadyToFanout{} -> history
  HeadFannedOut{chainState} -> pushNewState chainState history
  HeadPartialFannedOut{chainState} -> pushNewState chainState history
  HeadFanoutInitiated{} -> history
  HeadPartialFanoutSelected{} -> history
  HeadFanoutReverted{} -> history
  ChainRolledBack{chainState} -> rollbackHistory (chainStateSlot chainState) history
  TickObserved{chainPoint} -> setLastKnown chainPoint history
  CommitApproved{} -> history
  DecommitApproved{} -> history
  DecommitInvalid{} -> history
  IgnoredHeadInitializing{} -> history
  TxInvalid{} -> history
  LocalStateCleared{} -> history
  -- FIXME: This makes chain sync starting after rollbacks past the chain state impossible
  Checkpoint nodeState -> initHistory $ getChainState nodeState.headState
  NodeUnsynced{} -> history
  NodeSynced{} -> history
