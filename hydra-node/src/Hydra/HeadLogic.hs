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
-- behavior, one would find the decision logic in 'update' while state updates
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
import Hydra.API.ClientInput (ClientInput (..))
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
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  HeadState (..),
  IdleState (IdleState, chainState),
  OpenState (..),
  SeenSnapshot (..),
  getChainState,
  isCollectingAcks,
  mkSeenSnapshot,
  seenSnapshotNumber,
  setChainState,
  snapshotInFlight,
 )
import Hydra.Ledger (Ledger (..), applyTransactions)
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, SyncedStatus (..), depositsForHead, syncedStatus)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..))
import Hydra.Tx (
  HeadId,
  HeadSeed,
  IsTx (..),
  TxIdType,
  UTxOType,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (
  Signature,
  Verified (..),
  aggregateInOrder,
  sign,
  verifyMultiSignature,
  verifyMultiSignatureBytes,
 )
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

-- * The Coordinated Head protocol

-- | Maximum number of transaction ids per snapshot. This effectively limits our
-- "block size" and ensures it does not grow arbitrarily with the backlog of
-- pending transactions (localTxs).
-- TODO: Investigate what a good value is for this, in relation to memory
-- usage
maxTxsPerSnapshot :: Int
maxTxsPerSnapshot = 100

-- ** On-Chain Protocol

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  Outcome tx
onIdleClientInit env =
  cause OnChainEffect{postChainTx = InitTx{participants, headParameters}}
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
  Outcome tx
onIdleChainInitTx env newChainState headId headSeed headParameters participants
  | configuredParties == initializedParties
      && party `member` initializedParties
      && configuredContestationPeriod == contestationPeriod
      && Set.fromList configuredParticipants == Set.fromList participants =
      newState
        HeadOpened
          { parameters = headParameters
          , chainState = newChainState
          , headId
          , headSeed
          , parties
          }
  | otherwise =
      newState
        IgnoredHeadInitializing
          { headId
          , contestationPeriod
          , parties
          , participants
          }
 where
  initializedParties = Set.fromList parties

  configuredParties = Set.fromList (party : otherParties)

  HeadParameters{parties, contestationPeriod} = headParameters

  Environment
    { party
    , otherParties
    , contestationPeriod = configuredContestationPeriod
    , participants = configuredParticipants
    } = env

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenClientNewTx tx =
  cause . NetworkEffect $ ReqTx tx

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  ChainSlot ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger currentSlot st ttl pendingDeposits tx =
  -- Keep track of transactions by-id
  (newState TransactionReceived{tx} <>) $
    -- Spec: wait L̂ ◦ tx ≠ ⊥
    waitApplyTx $
      -- Spec: T̂ ← T̂ ⋃ {tx}
      --       L̂  ← L̂ ◦ tx
      newState TransactionAppliedToLocalUTxO{headId, tx}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        & maybeRequestSnapshot (confirmedSn + 1)
 where
  waitApplyTx cont =
    case applyTransactions currentSlot localUTxO [tx] of
      Right _ -> cont
      Left (_, err)
        | ttl > 0 ->
            wait (WaitOnNotApplicableTx err)
        | otherwise ->
            -- XXX: We are removing invalid txs from allTxs here to
            -- prevent them piling up infinitely. However, this is not really
            -- covered by the spec and this could be problematic in case of
            -- conflicting transactions paired with network latency and/or
            -- message resubmission. For example: Assume tx2 depends on tx1, but
            -- only tx2 is seen by a participant and eventually times out
            -- because of network latency when receiving tx1. The leader,
            -- however, saw both as valid and requests a snapshot including
            -- both. This is a valid request and it could make the head stuck.
            newState TxInvalid{headId, utxo = localUTxO, transaction = tx, validationError = err}

  maybeRequestSnapshot nextSn outcome =
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then
        outcome
          -- XXX: This state update has no equivalence in the
          -- spec. Do we really need to store that we have
          -- requested a snapshot? If yes, should update spec.
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause
            ( NetworkEffect $
                ReqSn
                  version
                  nextSn
                  (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs')
                  decommitTx
                  ( selectNextDeposit
                      pendingDeposits
                      currentDepositTxId
                      decommitTx
                      (getSnapshot confirmedSnapshot).utxoToCommit
                  )
            )
      else outcome

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  CoordinatedHeadState
    { localTxs
    , localUTxO
    , confirmedSnapshot
    , seenSnapshot
    , decommitTx
    , version
    , currentDepositTxId
    } = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  OpenState{coordinatedHeadState, headId, parameters} = st

  -- NOTE: Order of transactions is important here. See also
  -- 'pruneTransactions'.
  localTxs' = localTxs Seq.|> tx

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
  IsTx tx =>
  Environment ->
  Ledger tx ->
  PendingDeposits tx ->
  ChainSlot ->
  OpenState tx ->
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
  Outcome tx
onOpenNetworkReqSn env ledger pendingDeposits currentSlot st otherParty sv sn requestedTxIds mDecommitTx mDepositTxId =
  -- Spec: require v = v̂ ∧ s = ŝ + 1 ∧ leader(s) = j
  requireReqSn $
    -- Spec: wait ŝ = ̅S.s
    waitNoSnapshotInFlight $
      -- TODO: is this really needed?
      -- Spec: wait v = v̂
      waitOnSnapshotVersion $
        -- TODO: this is missing!? Spec: require tx𝜔 = ⊥ ∨ tx𝛼 = ⊥
        -- Require any pending utxo to decommit to be consistent
        requireApplicableDecommitTx $ \(activeUTxOAfterDecommit, mUtxoToDecommit) ->
          -- Wait for the deposit and require any pending commit to be consistent
          waitForDeposit activeUTxOAfterDecommit $ \(activeUTxO, mUtxoToCommit) ->
            -- Resolve transactions by-id
            waitResolvableTxs $ \requestedTxs -> do
              -- Spec: require 𝑈_active ◦ Treq ≠ ⊥
              --       𝑈 ← 𝑈_active ◦ Treq
              requireApplyTxs activeUTxO requestedTxs $ \u ->
                let snapshotUTxO = u `withoutUTxO` fromMaybe mempty mUtxoToCommit
                    accumulator = Accumulator.buildFromSnapshotUTxOs snapshotUTxO mUtxoToCommit mUtxoToDecommit
                 in requireValidAccumulatorSize accumulator $ do
                      -- Spec: ŝ ← ̅S.s + 1
                      -- NOTE: confSn == seenSn == sn here
                      let nextSnapshot =
                            Snapshot
                              { headId
                              , version = version
                              , number = sn
                              , confirmed = requestedTxs
                              , utxo = snapshotUTxO
                              , utxoToCommit = mUtxoToCommit
                              , utxoToDecommit = mUtxoToDecommit
                              , accumulator
                              }

                      -- Spec: 𝜂 ← combine(𝑈)
                      --       𝜂𝛼 ← combine(𝑈𝛼)
                      --       𝜂𝜔 ← combine(outputs(tx𝜔 ))
                      --       σᵢ ← MS-Sign(kₕˢⁱᵍ, (cid‖v‖ŝ‖η‖η𝛼‖ηω))
                      let snapshotSignature = sign signingKey nextSnapshot
                      -- Spec: multicast (ackSn, ŝ, σᵢ)
                      (cause (NetworkEffect $ AckSn snapshotSignature sn) <>) $ do
                        -- Spec: ̂Σ ← ∅
                        --       L̂ ← 𝑈
                        --       𝑋 ← T
                        --       T̂ ← ∅
                        --       for tx ∈ 𝑋 : L̂ ◦ tx ≠ ⊥
                        --         T̂ ← T̂ ⋃ {tx}
                        --         L̂ ← L̂ ◦ tx
                        let newLocalTxs = pruneTransactions u
                        newState
                          SnapshotRequested
                            { requestedSnapshot = nextSnapshot
                            , newLocalTxs
                            , newCurrentDepositTxId = mDepositTxId
                            }
 where
  requireReqSn continue
    | sv /= version =
        Error $ RequireFailed $ ReqSvNumberInvalid{requestedSv = sv, lastSeenSv = version}
    | sn /= seenSn + 1 =
        Error $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    | not (isLeader parameters otherParty sn) =
        Error $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
    | otherwise =
        continue

  waitNoSnapshotInFlight continue
    | confSn == seenSn =
        continue
    | otherwise =
        wait $ WaitOnSnapshotNumber seenSn

  waitOnSnapshotVersion continue
    | version == sv =
        continue
    | otherwise =
        wait $ WaitOnSnapshotVersion sv

  waitResolvableTxs continue =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> continue $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
      unseen -> wait $ WaitOnTxs unseen

  waitForDeposit activeUTxOAfterDecommit cont =
    case mDepositTxId of
      Nothing -> cont (activeUTxOAfterDecommit, Nothing)
      Just depositTxId ->
        case Map.lookup depositTxId pendingDeposits of
          Nothing ->
            -- Error out in case we receive a ReqSn that doesn't match local deposit
            Error $ RequireFailed RequestedDepositNotFoundLocally{depositTxId}
          Just Deposit{status, deposited}
            | status == Inactive -> wait WaitOnDepositActivation{depositTxId}
            | status == Expired -> Error $ RequireFailed RequestedDepositExpired{depositTxId}
            | otherwise ->
                -- NOTE: this makes the commits sequential in a sense that you can't
                -- commit unless the previous commit is settled.
                if sv == confVersion && isJust confUTxOToCommit
                  then
                    if confUTxOToCommit == Just deposited
                      then cont (activeUTxOAfterDecommit <> deposited, confUTxOToCommit)
                      else Error $ RequireFailed ReqSnCommitNotSettled
                  else do
                    let activeUTxOAfterCommit = activeUTxOAfterDecommit <> deposited
                    cont (activeUTxOAfterCommit, Just deposited)

  requireApplicableDecommitTx cont =
    case mDecommitTx of
      Nothing -> cont (confirmedUTxO, Nothing)
      Just decommitTx ->
        -- Spec:
        -- require tx𝜔 = ⊥ ∨ 𝑈𝛼 = ∅
        -- require 𝑣 = 𝑣 ̂ ∧ 𝑠 = 𝑠 ̂ + 1 ∧ leader(𝑠) = 𝑗
        -- wait 𝑠 ̂ = 𝒮.𝑠
        if sv == confVersion && isJust confUTxOToDecommit
          then
            if confUTxOToDecommit == Just (utxoFromTx decommitTx)
              then cont (confirmedUTxO, confUTxOToDecommit)
              else Error $ RequireFailed ReqSnDecommitNotSettled
          else case applyTransactions ledger currentSlot confirmedUTxO [decommitTx] of
            Left (_, err) ->
              Error $ RequireFailed $ SnapshotDoesNotApply sn (txId decommitTx) err
            Right newConfirmedUTxO -> do
              let utxoToDecommit = utxoFromTx decommitTx
              let activeUTxO = newConfirmedUTxO `withoutUTxO` utxoToDecommit
              cont (activeUTxO, Just utxoToDecommit)

  -- NOTE: at this point we know those transactions apply on the localUTxO because they
  -- are part of the localTxs. The snapshot can contain less transactions than the ones
  -- we have seen at this stage, but they all _must_ apply correctly to the latest
  -- snapshot's UTxO set, eg. it's illegal for a snapshot leader to request a snapshot
  -- containing transactions that do not apply cleanly.
  requireApplyTxs utxo requestedTxs cont =
    case applyTransactions ledger currentSlot utxo requestedTxs of
      Left (tx, err) ->
        Error $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
      Right u -> cont u

  requireValidAccumulatorSize :: Accumulator.HydraAccumulator -> Outcome tx -> Outcome tx
  requireValidAccumulatorSize accumulator continue
    | Accumulator.accumulatorSize accumulator > Accumulator.maxAccumulatorSize =
        Error $
          RequireFailed $
            ReqSnUTxOSetTooLarge
              { utxoCount = Accumulator.accumulatorSize accumulator
              , maxAllowed = Accumulator.maxAccumulatorSize
              }
    | otherwise =
        continue

  -- \| Filter 'localTxs' to those that still apply against the running UTxO
  -- after each previous successful tx. The post-snapshot UTxO is not returned:
  -- aggregate will recompute it.
  pruneTransactions utxo0 = go utxo0 localTxs
   where
    go _ Seq.Empty = Seq.empty
    go u (tx Seq.:<| rest) =
      -- XXX: We prune transactions on any error, while only some of them are
      -- actually expected.
      -- For example: `OutsideValidityIntervalUTxO` ledger errors are expected
      -- here when a tx becomes invalid.
      case applyTransactions ledger currentSlot u [tx] of
        Left _ -> go u rest
        Right u' -> tx Seq.<| go u' rest
  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  Snapshot{version = confVersion} = getSnapshot confirmedSnapshot

  confUTxOToCommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToCommit}} -> utxoToCommit

  confUTxOToDecommit = case confirmedSnapshot of
    InitialSnapshot{} -> Nothing
    ConfirmedSnapshot{snapshot = Snapshot{utxoToDecommit}} -> utxoToDecommit

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = case confirmedSnapshot of
    InitialSnapshot{} -> mempty
    ConfirmedSnapshot{snapshot = Snapshot{utxo, utxoToCommit}} -> utxo <> fromMaybe mempty utxoToCommit

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, headId} = st

  Environment{signingKey} = env

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
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn Environment{party} pendingDeposits openState otherParty snapshotSignature sn =
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs snapshotBytes -> do
      -- Spec: require (j,⋅) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        -- Spec: ̂Σ[j] ← σⱼ
        (newState PartySignedSnapshot{snapshotNumber = snapshot.number, party = otherParty, signature = snapshotSignature} <>) $
          --       if ∀k ∈ [1..n] : (k,·) ∈ ̂Σ
          ifAllMembersHaveSigned snapshot sigs $ \sigs' -> do
            -- Spec: σ̃ ← MS-ASig(kₕˢᵉᵗᵘᵖ,̂Σ)
            let multisig = aggregateInOrder sigs' parties
            -- Spec: η ← combine(𝑈ˆ)
            --       𝜂𝛼 ← combine(𝑈𝛼)
            --       𝑈𝜔 ← outputs(tx𝜔 )
            --       ηω ← combine(𝑈𝜔)
            --       require MS-Verify(k ̃H, (cid‖v̂‖ŝ‖η‖η𝛼‖ηω), σ̃)
            requireVerifiedMultisignature multisig snapshotBytes $
              do
                -- Spec: ̅S ← snObj(v̂, ŝ, Û, T̂, 𝑈𝛼, 𝑈𝜔)
                --       ̅S.σ ← ̃σ
                newState SnapshotConfirmed{headId, snapshot = Nothing, signatures = multisig}
                -- Spec: if η𝛼 ≠ ⊥
                --         postTx (increment, v̂, ŝ, η, η𝛼, ηω)
                & maybePostIncrementTx snapshot multisig
                -- Spec: if txω ≠ ⊥
                --         postTx (decrement, v̂, ŝ, η, η𝛼, ηω)
                & maybePostDecrementTx snapshot multisig
                -- Spec: if leader(s + 1) = i ∧ T̂ ≠ ∅
                -- REVIEW: multicast (reqSn, v, ̅S.s + 1, T̂, S.𝑈𝛼, S.txω)
                & maybeRequestNextSnapshot snapshot
 where
  seenSn = seenSnapshotNumber seenSnapshot

  requireValidAckSn continue =
    if sn `elem` [seenSn, seenSn + 1]
      then continue
      else Error $ RequireFailed $ AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}

  waitOnSeenSnapshot continue =
    case seenSnapshot of
      -- NOTE: Ignore any redundant AckSn for snapshots we have already seen as
      -- confirmed. This is for example happening if a party runs multiple
      -- instances of hydra-node using the same keys.
      LastSeenSnapshot{lastSeen}
        | sn <= lastSeen -> noop
      SeenSnapshot{snapshot, signatories = sigs, signableBytes}
        | seenSn == sn -> continue snapshot sigs signableBytes
      _ -> wait WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    if not (Map.member otherParty sigs)
      then continue
      else Error $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys sigs, receivedSignature = otherParty}

  ifAllMembersHaveSigned snapshot sigs cont =
    let sigs' = Map.insert otherParty snapshotSignature sigs
     in if Map.keysSet sigs' == Set.fromList parties
          then cont sigs'
          else
            newState
              PartySignedSnapshot
                { snapshotNumber = snapshot.number
                , party = otherParty
                , signature = snapshotSignature
                }

  requireVerifiedMultisignature multisig msg cont =
    case verifyMultiSignatureBytes vkeys multisig msg of
      Verified -> cont
      FailedKeys failures ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys = failures}
      KeyNumberMismatch ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys}

  maybeRequestNextSnapshot previous outcome = do
    let nextSn = previous.number + 1
        nextDeposit = selectNextDeposit pendingDeposits currentDepositTxId decommitTx previous.utxoToCommit
    if isLeader parameters party nextSn && not (null localTxs)
      then
        outcome
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) decommitTx nextDeposit)
      else outcome

  maybePostIncrementTx snapshot@Snapshot{utxoToCommit} signatures outcome =
    -- TODO: check status (again)?
    case find (\(_, Deposit{deposited}) -> Just deposited == utxoToCommit) $ Map.toList pendingDeposits of
      Just (depositTxId, Deposit{deposited}) ->
        outcome
          <> newState CommitApproved{headId, utxoToCommit = deposited}
          <> cause
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
      _ -> outcome

  maybePostDecrementTx snapshot@Snapshot{utxoToDecommit} signatures outcome =
    case (decommitTx, utxoToDecommit) of
      (Just tx, Just utxo) ->
        outcome
          <> newState
            DecommitApproved
              { headId
              , decommitTxId = txId tx
              , utxoToDecommit = utxo
              }
          <> cause
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

-- | Client request to recover deposited UTxO.
--
-- __Transition__: 'OpenState' → 'OpenState'
onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Error $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited} ->
      causes
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
  Ledger tx ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  -- | Decommit transaction.
  tx ->
  Outcome tx
onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx =
  checkNoDecommitInFlight $
    checkValidDecommitTx $
      cause (NetworkEffect ReqDec{transaction = decommitTx})
 where
  checkNoDecommitInFlight continue =
    case mExistingDecommitTx of
      Just existingDecommitTx ->
        newState
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitAlreadyInFlight
                  { otherDecommitTxId = txId existingDecommitTx
                  }
            }
      Nothing -> continue

  checkValidDecommitTx cont =
    case applyTransactions ledger currentSlot localUTxO [decommitTx] of
      Left (_, err) ->
        newState
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
  Ledger tx ->
  TTL ->
  ChainSlot ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl currentSlot openState decommitTx =
  -- Spec: wait 𝑈𝛼 = ∅ ^ txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $
    -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
    -- Spec: txω ← tx
    newState DecommitRecorded{headId, decommitTx}
      -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
      --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
      <> maybeRequestSnapshot
 where
  waitOnApplicableDecommit cont =
    case mExistingDecommitTx of
      Nothing ->
        case applyTransactions currentSlot localUTxO [decommitTx] of
          Right _ -> cont
          Left (_, validationError)
            | ttl > 0 ->
                wait $
                  WaitOnNotApplicableDecommitTx
                    ServerOutput.DecommitTxInvalid{localUTxO, validationError}
            | otherwise ->
                newState
                  DecommitInvalid
                    { headId
                    , decommitTx
                    , decommitInvalidReason =
                        ServerOutput.DecommitTxInvalid{localUTxO, validationError}
                    }
      Just existingDecommitTx
        | ttl > 0 ->
            wait $
              WaitOnNotApplicableDecommitTx
                DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
        | otherwise ->
            newState
              DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason =
                    DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
                }

  maybeRequestSnapshot =
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then cause (NetworkEffect (ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) (Just decommitTx) Nothing))
      else noop

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  CoordinatedHeadState
    { decommitTx = mExistingDecommitTx
    , confirmedSnapshot
    , localTxs
    , localUTxO
    , version
    , seenSnapshot
    } = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
    } = openState

determineNextDepositStatus :: Ord (TxIdType tx) => Environment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  Map.foldlWithKey updateDeposit mempty pendingDeposits
 where
  updateDeposit nextSelected depositTxId deposit =
    let newStatus = determineStatus deposit
        d' = deposit{status = newStatus}
     in Map.insert depositTxId d' nextSelected

  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositPeriod = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  Environment{depositPeriod} = env

-- | Process the chain (and time) advancing in any head state.
--
-- __Transition__: 'AnyState' → 'AnyState'
--
-- This is primarily used to track deposits status changes.
onChainTick :: IsTx tx => Environment -> PendingDeposits tx -> UTCTime -> Outcome tx
onChainTick env pendingDeposits chainTime =
  mkDepositActivated newActive <> mkDepositExpired newExpired
 where
  -- XXX: This is a bit messy
  newActive = Map.difference nextActive pendingActive

  newExpired = Map.difference nextExpired pendingExpired

  pendingActive = Map.filter (\Deposit{status} -> status == Active) pendingDeposits

  pendingExpired = Map.filter (\Deposit{status} -> status == Expired) pendingDeposits

  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  nextActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  nextExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  mkDepositActivated m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositActivated{depositTxId, chainTime, deposit}

  mkDepositExpired m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositExpired{depositTxId, chainTime, deposit}

-- | Process the chain (and time) advancing in an open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- This is primarily used to track deposits and either drop them or request
-- snapshots for inclusion.
onOpenChainTick :: IsTx tx => Environment -> UTCTime -> PendingDeposits tx -> OpenState tx -> Outcome tx
onOpenChainTick env chainTime pendingDeposits st =
  -- Determine new active and new expired
  let nextDeposits = determineNextDepositStatus env pendingDeposits chainTime
      newActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits
      newExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits
   in -- Apply state changes and pick next active to request snapshot
      -- XXX: This is smelly as we rely on Map <> to override entries (left
      -- biased). This is also weird because we want to actually apply the state
      -- change and also to determine the next active.
      withNextActive (newActive <> newExpired <> pendingDeposits) $ \depositTxId ->
        -- REVIEW: this is not really a wait, but discard?
        -- TODO: Spec: wait tx𝜔 = ⊥ ∧ 𝑈𝛼 = ∅
        if isNothing decommitTx
          && isNothing currentDepositTxId
          && not (snapshotInFlight seenSnapshot)
          && isLeader parameters party nextSn
          then
            -- XXX: This state update has no equivalence in the
            -- spec. Do we really need to store that we have
            -- requested a snapshot? If yes, should update spec.
            newState SnapshotRequestDecided{snapshotNumber = nextSn}
              -- Spec: multicast (reqSn,̂ 𝑣,̄ 𝒮.𝑠 + 1,̂ 𝒯, 𝑈𝛼, ⊥)
              <> cause (NetworkEffect $ ReqSn version nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) Nothing (Just depositTxId))
          else
            noop
 where
  -- Pending active deposits are selected in arrival order (FIFO).
  withNextActive :: forall tx. (Eq (UTxOType tx), Monoid (UTxOType tx)) => Map (TxIdType tx) (Deposit tx) -> (TxIdType tx -> Outcome tx) -> Outcome tx
  withNextActive deposits cont = do
    -- NOTE: Do not consider empty deposits.
    let p :: (x, Deposit tx) -> Bool
        p (_, Deposit{deposited, status}) = deposited /= mempty && status == Active
    case filter p (Map.toList deposits) of
      [] -> noop
      xs -> cont (fst (minimumBy (comparing ((\Deposit{created} -> created) . snd)) xs))

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

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  OpenState{coordinatedHeadState, parameters} = st

-- | If this node is the snapshot leader and there are pending local transactions,
-- request the next snapshot with the bumped version after a commit or decommit
-- finalises on-chain.
--
-- Guards:
--   * Only fires when 'version /= newVersion' to avoid duplicate
--     'SnapshotRequestDecided' events when multiple parties post the same
--     on-chain tx and each posting produces a separate finalisation observation.
--   * Skips when AckSns are already being collected ('SeenSnapshot'): the
--     in-flight snapshot will complete and 'maybeRequestNextSnapshot' will chain
--     the next one with the bumped version. Firing here would use stale
--     'localTxs' and cause 'BadInputsUTxO' on other parties.
--   * Allows 'RequestedSnapshot': the in-flight ReqSn carries the old version
--     and will be rejected with 'ReqSvNumberInvalid', so we must re-request
--     immediately with the new version to avoid a permanently stuck head.
--
-- The optional 'depositTxId' argument is forwarded into 'ReqSn': commit
-- finalisation passes 'Nothing' (deposit already included), while decommit
-- finalisation passes the next queued deposit if one is pending.
maybeRequestSnapshotAfterVersionBump ::
  IsTx tx =>
  HeadParameters ->
  Party ->
  SnapshotNumber ->
  Seq tx ->
  SnapshotVersion ->
  SnapshotVersion ->
  SeenSnapshot tx ->
  Maybe (TxIdType tx) ->
  Outcome tx
maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot depositTxId =
  if isLeader parameters party nextSn && not (null localTxs) && version /= newVersion && not (isCollectingAcks seenSnapshot)
    then
      newState SnapshotRequestDecided{snapshotNumber = nextSn}
        <> cause (NetworkEffect $ ReqSn newVersion nextSn (toList $ txId <$> Seq.take maxTxsPerSnapshot localTxs) Nothing depositTxId)
    else noop

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
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Deposit TxId
  TxIdType tx ->
  Outcome tx
onOpenChainIncrementTx env openState newChainState newVersion depositTxId =
  newState CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
    <> maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot Nothing
 where
  OpenState{headId, parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, version, seenSnapshot} = coordinatedHeadState

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
  Outcome tx
onOpenChainDecrementTx env pendingDeposits openState newChainState newVersion distributedUTxO =
  newState
    DecommitFinalized
      { chainState = newChainState
      , headId
      , newVersion
      , distributedUTxO
      }
    <> maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot (setExistingDeposit pendingDeposits currentDepositTxId)
 where
  OpenState{headId, parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, currentDepositTxId, version, seenSnapshot} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

-- | On rollback, re-post the IncrementTx if there is a pending deposit whose
-- confirmed snapshot contains a matching utxoToCommit. The rollback may have
-- erased the original on-chain IncrementTx observation.
maybeRepostIncrementTx ::
  IsTx tx =>
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  ConfirmedSnapshot tx ->
  Outcome tx
maybeRepostIncrementTx headSeed headId parameters pendingDeposits mDepositTxId confirmedSnapshot =
  case (mDepositTxId, confirmedSnapshot) of
    (Just depositTxId, ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToCommit}, signatures}) ->
      case find (\(_, Deposit{deposited}) -> Just deposited == utxoToCommit) $ Map.toList pendingDeposits of
        Just (_, Deposit{}) ->
          cause
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
        _ -> noop
    _ -> noop

-- | On rollback, re-post the DecrementTx if there is a pending decommit whose
-- confirmed snapshot contains a matching utxoToDecommit. The rollback may have
-- erased the original on-chain DecrementTx observation.
maybeRepostDecrementTx ::
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  Maybe tx ->
  ConfirmedSnapshot tx ->
  Outcome tx
maybeRepostDecrementTx headSeed headId parameters mDecommitTx confirmedSnapshot =
  case (mDecommitTx, confirmedSnapshot) of
    (Just _, ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToDecommit = Just _}, signatures}) ->
      cause
        OnChainEffect
          { postChainTx =
              DecrementTx
                { headSeed
                , headId
                , headParameters = parameters
                , decrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
                }
          }
    _ -> noop

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
  Outcome tx
onOpenClientClose st =
  -- Spec: η ← combine(̅S.𝑈)
  --       η𝛼 ← combine(S.𝑈𝛼)
  --       ηω ← combine(S.𝑈ω)
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, η𝛼, ηω,ξ)
  cause
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
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  newState HeadClosed{headId, snapshotNumber = closedSnapshotNumber, chainState = newChainState, contestationDeadline}
    & maybePostContest
 where
  maybePostContest outcome =
    -- Spec: if ̅S.s > sc
    if number (getSnapshot confirmedSnapshot) > closedSnapshotNumber
      then
        outcome
          -- XXX: As we use 'version' in the contest here, this is implies
          -- that our last 'confirmedSnapshot' must match version or
          -- version-1. Assert this fact?
          -- Spec: η ← combine(̅S.𝑈)
          --       η𝛼 ← combine(S.𝑈𝛼)
          --       ηω ← combine(S.𝑈ω)
          --       ξ ← ̅S.σ
          --       postTx (contest, ̅S.v, ̅S.s, η, η𝛼, ηω, ξ)
          <> cause
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

  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

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
onOpenClientSideLoadSnapshot :: IsTx tx => OpenState tx -> ConfirmedSnapshot tx -> Outcome tx
onOpenClientSideLoadSnapshot openState requestedConfirmedSnapshot =
  case requestedConfirmedSnapshot of
    InitialSnapshot{} ->
      requireVerifiedSameSnapshot $
        newState LocalStateCleared{headId, snapshotNumber = requestedSn}
    ConfirmedSnapshot{snapshot, signatures} ->
      requireVerifiedSnapshotNumber $
        requireVerifiedL1Snapshot $
          requireVerifiedMultisignature snapshot signatures $
            changes
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
    , utxoToDecommit = lastSeenSd
    } = getSnapshot currentConfirmedSnapshot

  requestedSnapshot@Snapshot
    { version = requestedSv
    , number = requestedSn
    , utxoToCommit = requestedSc
    , utxoToDecommit = requestedSd
    } = getSnapshot requestedConfirmedSnapshot

  clientInput = SideLoadSnapshot requestedConfirmedSnapshot

  sideLoadFailed requirementFailure =
    cause . ClientEffect $
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
    | requestedSd /= lastSeenSd = sideLoadFailed SideLoadUTxOToDecommitInvalid{requestedSd, lastSeenSd}
    | otherwise = cont

  requireVerifiedMultisignature snapshot signatories cont =
    case verifyMultiSignature vkeys signatories snapshot of
      Verified -> cont
      FailedKeys failures ->
        sideLoadFailed SideLoadInvalidMultisignature{multisig = show signatories, vkeys = failures}
      KeyNumberMismatch ->
        sideLoadFailed SideLoadInvalidMultisignature{multisig = show signatories, vkeys}

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
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
  Outcome tx
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline =
  if
    | -- Spec: if ̅S.s > sc
      number (getSnapshot confirmedSnapshot) > snapshotNumber ->
        -- XXX: As we use 'version' in the contest here, this is implies
        -- that our last 'confirmedSnapshot' must match version or
        -- version-1. Assert this fact?
        -- Spec: η ← combine(̅S.𝑈)
        --       η𝛼 ← combine(S.𝑈𝛼)
        --       ηω ← combine(S.𝑈ω)
        --       ξ ← ̅S.σ
        --       postTx (contest, ̅S.v, ̅S.s, η, η𝛼, ηω, ξ)
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
          <> cause
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
    | snapshotNumber > number (getSnapshot confirmedSnapshot) ->
        -- TODO: A more recent snapshot number was successfully contested, we will
        -- not be able to fanout! We might want to communicate that to the client!
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
    | otherwise ->
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
 where
  ClosedState{parameters = headParameters, confirmedSnapshot, headId, version} = closedState

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- The node decides the fanout tx TYPE based solely on the fanout phase:
--
--  * 'FreshFanout' (no prior partial fanout observed): emit 'FanoutTx'. The
--    chain layer evaluates it and falls back to 'PartialFanoutTx' if it
--    exceeds the execution budget.
--  * 'FanoutInProgress' (prior partial fanouts observed): emit
--    'FinalPartialFanoutTx'. The chain layer similarly falls back to
--    'PartialFanoutTx' if the remaining set is still too large.
--
-- The chunk-size decision is fully dynamic in the chain layer — no threshold
-- constant is needed here.
--
-- If a partial fanout was already in progress (remainingFanoutUTxO is Just),
-- we fan out from the remaining UTxOs instead of the full snapshot.
--
-- __Transition__: 'ClosedState' → 'ClosedState' (partial) or 'ClosedState' → 'IdleState' (full)
onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState@ClosedState{remainingFanoutOutputs} =
  case remainingFanoutOutputs of
    Just remaining -> emitNextFanoutStep FanoutInProgress remaining closedState
    Nothing -> emitNextFanoutStep FreshFanout (Set.fromList $ outputsOfUTxO $ computeFullFanoutUTxO closedState) closedState

-- | Observe a fanout transaction by finalize the head state and notifying
-- clients about it.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  -- When partial fanouts preceded this final fanout, combine the output values
  -- distributed by each partial fanout with the final batch so clients receive
  -- the complete set.
  let allOutputs =
        distributedFanoutOutputs
          <> Set.fromList (outputsOfUTxO fanoutUTxO)
   in newState HeadFannedOut{headId, finalizedOutputs = allOutputs, chainState = newChainState}
 where
  ClosedState{headId, distributedFanoutOutputs} = closedState

-- | Observe a partial fanout transaction on chain. This stays in 'ClosedState'
-- with updated remaining UTxOs and automatically triggers the next fanout.
--
-- Automatically triggers the next fanout step: 'FinalPartialFanoutTx'. The
-- chain layer falls back to another 'PartialFanoutTx' if remaining UTxOs do
-- not fit in a single tx.
--
-- __Off-chain model__: 'ClosedState' → 'ClosedState' (the on-chain state transitions to FanoutProgress)
onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | Output values that were distributed in this partial fanout
  Set (TxOutType tx) ->
  Outcome tx
onClosedChainPartialFanoutTx closedState newChainState distributedOutputs =
  -- Compute the remaining outputs after removing the distributed ones.
  -- Use content-based Set.difference so an adversary distributing non-prefix
  -- items does not corrupt our tracking.
  let fullOutputs = resolveRemainingOutputs closedState
      remaining = Set.difference fullOutputs distributedOutputs
      stateChange =
        newState
          HeadPartialFannedOut
            { headId
            , distributedOutputs
            , remainingOutputs = remaining
            , chainState = newChainState
            }
   in stateChange <> emitNextFanoutStep FanoutInProgress remaining closedState
 where
  ClosedState{headId} = closedState

-- | Compute the full UTxO set to be fanned out, combining snapshot utxo
-- with utxoToCommit/utxoToDecommit based on version.
computeFullFanoutUTxO ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx
computeFullFanoutUTxO ClosedState{confirmedSnapshot, version} =
  utxo
    <> fromMaybe mempty effectiveCommit
    <> fromMaybe mempty effectiveDecommit
 where
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot
  effectiveCommit =
    if snapshotVersion == version
      then Nothing
      else utxoToCommit
  effectiveDecommit =
    if snapshotVersion == version
      then utxoToDecommit
      else Nothing

-- | Resolve the output set to fan out: the already-narrowed remaining set if a
-- partial fanout is in progress, or the full snapshot outputs otherwise.
resolveRemainingOutputs ::
  IsTx tx =>
  ClosedState tx ->
  Set (TxOutType tx)
resolveRemainingOutputs closedState@ClosedState{remainingFanoutOutputs} =
  case remainingFanoutOutputs of
    Just remaining -> remaining
    Nothing -> Set.fromList $ outputsOfUTxO $ computeFullFanoutUTxO closedState

-- | Tracks whether a partial fanout has already been observed on-chain.
-- Used to decide the final step: 'FanoutInProgress' heads go to
-- 'FinalPartialFanoutTx'; 'FreshFanout' heads can still use the direct 'FanoutTx'.
data PartialFanoutPhase = FreshFanout | FanoutInProgress
  deriving stock (Show)

-- | Emit the appropriate on-chain effect based solely on the fanout phase.
-- 'FreshFanout' → 'FanoutTx' (chain layer evaluates; may fall back to PartialFanoutTx).
-- 'FanoutInProgress' → 'FinalPartialFanoutTx' (head output already in FanoutProgress state).
emitNextFanoutStep ::
  IsTx tx =>
  PartialFanoutPhase ->
  Set (TxOutType tx) ->
  ClosedState tx ->
  Outcome tx
emitNextFanoutStep FreshFanout _ closedState =
  let ClosedState{confirmedSnapshot, version, headSeed, contestationDeadline} = closedState
      Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot
   in cause
        OnChainEffect
          { postChainTx =
              FanoutTx
                { utxo
                , -- Include utxoToCommit only if the increment has been
                  -- applied on chain (version bumped). Otherwise it was
                  -- never used and must not be distributed.
                  utxoToCommit =
                    if snapshotVersion == version
                      then Nothing
                      else utxoToCommit
                , -- Include utxoToDecommit only if the decrement has NOT
                  -- been applied on chain yet. If it was applied the
                  -- UTxO is gone and must not be re-distributed.
                  utxoToDecommit =
                    if snapshotVersion == version
                      then utxoToDecommit
                      else Nothing
                , headSeed
                , contestationDeadline
                }
          }
emitNextFanoutStep FanoutInProgress remaining closedState@ClosedState{headSeed, contestationDeadline} =
  cause
    OnChainEffect
      { postChainTx =
          FinalPartialFanoutTx
            { utxoToDistribute = filterUTxOByOutputs (computeFullFanoutUTxO closedState) remaining
            , headSeed
            , contestationDeadline
            }
      }

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
  Outcome tx
handleOutOfSync Environment{unsyncedPeriod} now chainPoint chainTime syncStatus =
  stateTransition <> outputIfNeeded
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
  stateTransition =
    case (syncStatus, newSyncStatus) of
      (InSync, CatchingUp) -> newState NodeUnsynced{chainSlot, chainTime, drift}
      (CatchingUp, InSync) -> newState NodeSynced{chainSlot, chainTime, drift}
      _ -> noop

  -- We have consumed 80% of the allowed drift
  nearThreshold = drift >= threshold * 0.8
  shouldOutput =
    case newSyncStatus of
      CatchingUp -> True
      InSync -> nearThreshold
  outputIfNeeded
    | shouldOutput = output newSyncStatus
    | otherwise = noop
  output synced =
    cause . ClientEffect $ ServerOutput.SyncedStatusReport{chainSlot, chainTime, drift, synced}

-- | Validate whether a current deposit in the local state actually exists
--   in the map of pending deposits.
--
--   * If 'currentDeposit' is 'Nothing', returns 'Nothing'.
--   * If 'currentDeposit' is @'Just' txId@ and @txId@ is present in 'pendingDeposits',
--     returns the original 'currentDeposit'.
--   * Otherwise, returns 'Nothing'.
--
--   This is typically used to confirm that a local deposit that is to be
--   requested in 'ReqSn' is indeed still pending and has not been processed or
--   removed.
setExistingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx)
setExistingDeposit pendingDeposits currentDeposit = do
  case currentDeposit of
    Nothing -> Nothing
    Just depositTxId ->
      case Map.lookup depositTxId pendingDeposits of
        Nothing -> Nothing
        Just _ -> currentDeposit

-- | Find the oldest non-empty active deposit, if any. Deposits are selected
-- in FIFO order by their 'created' timestamp. This mirrors the selection
-- logic in 'withNextActive' used by 'onOpenChainTick'.
nextActiveDepositId :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx)
nextActiveDepositId deposits =
  case filter (\(_, Deposit{deposited, status}) -> deposited /= mempty && status == Active) (Map.toList deposits) of
    [] -> Nothing
    xs -> Just (fst (minimumBy (comparing ((.created) . snd)) xs))

-- | Select the deposit to include in the next snapshot.
--
-- Prefers a deposit already tracked in 'currentDepositTxId' (if still pending).
-- Falls back to the oldest active deposit from 'pendingDeposits', but only
-- when neither a decommit is pending nor the last confirmed snapshot already
-- included a deposit (to avoid double-posting 'IncrementTx' before
-- 'CommitFinalized' removes the deposit).
selectNextDeposit ::
  IsTx tx =>
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | utxoToCommit of the last relevant confirmed snapshot
  Maybe (UTxOType tx) ->
  Maybe (TxIdType tx)
selectNextDeposit pendingDeposits currentDepositTxId mDecommitTx mConfirmedUtxoToCommit =
  setExistingDeposit pendingDeposits currentDepositTxId
    <|> case (mDecommitTx, mConfirmedUtxoToCommit) of
      (Nothing, Nothing) -> nextActiveDepositId pendingDeposits
      _ -> Nothing

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
  case nodeState of
    NodeCatchingUp{headState, pendingDeposits, chainPointTime} ->
      updateCatchingUpHead env ledger now chainPointTime pendingDeposits headState ev (syncedStatus nodeState)
    NodeInSync{headState, pendingDeposits, chainPointTime} ->
      updateInSyncHead env ledger now chainPointTime pendingDeposits headState ev (syncedStatus nodeState)

updateCatchingUpHead ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
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
  Outcome tx
updateCatchingUpHead env ledger now chainPointTime pendingDeposits st ev syncStatus =
  case ev of
    ChainInput{} ->
      handleChainInput env ledger now chainPointTime pendingDeposits st ev syncStatus
    ClientInput{clientInput} ->
      cause . ClientEffect $ ServerOutput.RejectedInputBecauseUnsynced clientInput drift
    NetworkInput{} ->
      wait WaitOnNodeInSync{currentSlot}
 where
  ChainPointTime{currentSlot, drift} = chainPointTime

updateInSyncHead ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
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
  Outcome tx
updateInSyncHead env ledger now chainPointTime pendingDeposits st ev syncStatus =
  case ev of
    ChainInput{} ->
      handleChainInput env ledger now chainPointTime pendingDeposits st ev syncStatus
    ClientInput{} ->
      handleClientInput env ledger chainPointTime pendingDeposits st ev
    NetworkInput{} ->
      handleNetworkInput env ledger chainPointTime pendingDeposits st ev

-- * Input Handlers

handleChainInput ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
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
  Outcome tx
handleChainInput env _ledger now _chainPointTime pendingDeposits st ev syncStatus = case (st, ev) of
  (Idle _, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState}) ->
    onIdleChainInitTx env newChainState headId headSeed headParameters participants
  -- Open
  ( Open openState@OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Tick{chainTime, chainPoint}) ->
    -- XXX: We originally forgot the normal TickObserved state event here and so
    -- time did not advance in an open head anymore. This is a hint that we
    -- should compose event handling better.
    newState TickObserved{chainPoint}
      <> handleOutOfSync env now chainPoint chainTime syncStatus
      <> onChainTick env pendingDeposits chainTime
      <> onOpenChainTick env chainTime (depositsForHead ourHeadId pendingDeposits) openState
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId}, newChainState})
    | ourHeadId == headId ->
        onOpenChainIncrementTx env openState newChainState newVersion depositTxId
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO}, newChainState})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx env (depositsForHead ourHeadId pendingDeposits) openState newChainState newVersion distributedUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed ClosedState{contestationDeadline, readyToFanoutSent, headId}, ChainInput Tick{chainTime, chainPoint})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        newState TickObserved{chainPoint}
          <> handleOutOfSync env now chainPoint chainTime syncStatus
          <> onChainTick env pendingDeposits chainTime
          <> newState HeadIsReadyToFanout{headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onClosedChainFanoutTx closedState newChainState fanoutUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onClosedChainPartialFanoutTx closedState newChainState distributedOutputs
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Node-level
  (_, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState}) ->
    newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState}) ->
    newState DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
  -- Open + Rollback: re-post IncrementTx/DecrementTx if they were in-flight
  ( Open
      OpenState
        { headSeed
        , headId
        , parameters
        , coordinatedHeadState =
          CoordinatedHeadState
            { currentDepositTxId
            , confirmedSnapshot
            , decommitTx
            }
        }
    , ChainInput Rollback{rolledBackChainState, chainTime}
    ) ->
      newState ChainRolledBack{chainState = rolledBackChainState}
        <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
        <> maybeRepostIncrementTx headSeed headId parameters (depositsForHead headId pendingDeposits) currentDepositTxId confirmedSnapshot
        <> maybeRepostDecrementTx headSeed headId parameters decommitTx confirmedSnapshot
  -- General
  (_, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
      <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
  (_, ChainInput Tick{chainTime, chainPoint}) ->
    newState TickObserved{chainPoint}
      <> handleOutOfSync env now chainPoint chainTime syncStatus
      <> onChainTick env pendingDeposits chainTime
  (_, ChainInput PostTxError{postTxError = StalePartialFanoutTx}) ->
    -- The chain advanced past this step before we could post it (another node
    -- was faster). The chain observation loop already emitted the correct next
    -- step, so this is safe to ignore.
    noop
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    cause . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  _ ->
    Error $ UnhandledInput ev st

handleNetworkInput ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
handleNetworkInput env ledger ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  -- Open
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env ledger currentSlot openState ttl (depositsForHead ourHeadId pendingDeposits) tx
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput _ (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    onOpenNetworkReqSn env ledger (depositsForHead ourHeadId pendingDeposits) currentSlot openState sender sv sn txIds decommitTx depositTxId
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    onOpenNetworkAckSn env (depositsForHead ourHeadId pendingDeposits) openState sender snapshotSignature sn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ledger ttl currentSlot openState transaction
  _ ->
    Error $ UnhandledInput ev st

onConnectionEvent :: Text -> Network.Connectivity -> Outcome tx
onConnectionEvent misconfiguredPeers = \case
  Network.NetworkConnected ->
    newState NetworkConnected
  Network.NetworkDisconnected ->
    newState NetworkDisconnected
  Network.VersionMismatch{ourVersion, theirVersion} ->
    newState NetworkVersionMismatch{ourVersion, theirVersion}
  Network.ClusterIDMismatch{clusterPeers} ->
    newState NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers}
  Network.PeerConnected{peer} ->
    newState PeerConnected{peer}
  Network.PeerDisconnected{peer} ->
    newState PeerDisconnected{peer}

handleClientInput ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Last known chain point time
  ChainPointTime ->
  PendingDeposits tx ->
  -- | Current NodeState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
handleClientInput env ledger ChainPointTime{currentSlot} pendingDeposits st ev = case (st, ev) of
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
          else Error NotOurHead{ourHeadId, otherHeadId}
  (Open OpenState{headId, coordinatedHeadState}, ClientInput Decommit{decommitTx}) -> do
    onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx
  -- Closed
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  -- Node-level
  (_, ClientInput Recover{recoverTxId}) -> do
    onClientRecover currentSlot pendingDeposits recoverTxId
  -- General
  (_, ClientInput{clientInput}) ->
    cause . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    Error $ UnhandledInput ev st

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
      let currentPendingDeposits = pendingDeposits nodeState
          st = applyEvent (headState nodeState) sc
          chainPointTimeState = chainPointTime nodeState
       in case sc of
            HeadOpened{chainState} ->
              nodeState
                { headState = st
                , chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}
                }
            DepositRecorded{headId, depositTxId, deposited, created, deadline} ->
              nodeState
                { headState = st
                , pendingDeposits = Map.insert depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} currentPendingDeposits
                }
            DepositActivated{depositTxId, deposit} ->
              nodeState
                { headState = st
                , pendingDeposits = Map.insert depositTxId deposit currentPendingDeposits
                }
            DepositExpired{depositTxId, deposit} ->
              nodeState
                { headState = st
                , -- NB: We keep expired deposits in a map since we actually need it when Recovering.
                  -- There is a corresponding error RequestedDepositExpired which gives users context on stale ReqSn.
                  pendingDeposits = Map.insert depositTxId deposit currentPendingDeposits
                }
            DepositRecovered{depositTxId} ->
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
                    , pendingDeposits = Map.delete depositTxId currentPendingDeposits
                    }
                _ ->
                  nodeState
                    { headState = st
                    , pendingDeposits = Map.delete depositTxId currentPendingDeposits
                    }
            CommitFinalized{chainState, newVersion, depositTxId} ->
              case st of
                Open os@OpenState{coordinatedHeadState} ->
                  let deposit = Map.lookup depositTxId currentPendingDeposits
                      newUTxO = maybe mempty (\Deposit{deposited} -> deposited) deposit
                   in nodeState
                        { headState =
                            Open
                              os
                                { chainState
                                , coordinatedHeadState =
                                    coordinatedHeadState
                                      { version = newVersion
                                      , -- NOTE: This must correspond to the just finalized
                                        -- depositTxId, but we should not verify this here.
                                        currentDepositTxId = Nothing
                                      , localUTxO = localUTxO <> newUTxO
                                      , -- If a snapshot is already in SeenSnapshot, all parties
                                        -- have processed the ReqSn and sent AckSns — preserve it
                                        -- so that snapshot can still complete and chain the next
                                        -- one with the bumped version. Only reset when nothing
                                        -- is in-flight.
                                        seenSnapshot = case seenSnapshot of
                                          SeenSnapshot{} -> seenSnapshot
                                          _ -> LastSeenSnapshot{lastSeen = confirmedSn}
                                      }
                                }
                        , pendingDeposits = Map.delete depositTxId currentPendingDeposits
                        }
                 where
                  CoordinatedHeadState{localUTxO, confirmedSnapshot, seenSnapshot} = coordinatedHeadState
                  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot
                _ ->
                  nodeState
                    { headState = st
                    , pendingDeposits = Map.delete depositTxId currentPendingDeposits
                    }
            TickObserved{chainPoint} ->
              nodeState{headState = st, chainPointTime = chainPointTimeState{currentSlot = chainPointSlot chainPoint}}
            ChainRolledBack{chainState} ->
              nodeState{headState = st, chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}}
            NodeUnsynced{chainSlot, chainTime, drift} ->
              NodeCatchingUp{headState = st, pendingDeposits = currentPendingDeposits, chainPointTime = ChainPointTime chainSlot chainTime drift}
            NodeSynced{chainSlot, chainTime, drift} ->
              NodeInSync{headState = st, pendingDeposits = currentPendingDeposits, chainPointTime = ChainPointTime chainSlot chainTime drift}
            _ ->
              nodeState{headState = st}

-- * HeadState aggregate helpers

-- | Extract the 'HeadId' from a 'StateChanged' event, if the event carries one.
-- Events that do not carry a 'HeadId' always pass through 'aggregateNodeState' unchanged.
eventHeadId :: StateChanged tx -> Maybe HeadId
eventHeadId = \case
  HeadOpened{headId} -> Just headId
  TransactionAppliedToLocalUTxO{headId} -> Just headId
  SnapshotConfirmed{headId} -> Just headId
  LocalStateCleared{headId} -> Just headId
  DepositRecorded{headId} -> Just headId
  DepositRecovered{headId} -> Just headId
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
  Checkpoint{} -> Nothing
  NodeUnsynced{} -> Nothing
  NodeSynced{} -> Nothing

-- | Extract the 'HeadId' from the current 'HeadState', if any.
headIdOf :: HeadState tx -> Maybe HeadId
headIdOf = \case
  Idle _ -> Nothing
  Open OpenState{headId} -> Just headId
  Closed ClosedState{headId} -> Just headId

applyEvent :: IsChainState tx => HeadState tx -> StateChanged tx -> HeadState tx
applyEvent st = \case
  NetworkConnected -> st
  NetworkDisconnected -> st
  NetworkVersionMismatch{} -> st
  NetworkClusterIDMismatch{} -> st
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
                    localUTxO =
                      let activeUTxO = snapshot.utxo <> fromMaybe mempty snapshot.utxoToCommit
                       in foldl' (flip applyTxTo) activeUTxO newLocalTxs
                  , allTxs = foldr (Map.delete . txId) allTxs snapshot.confirmed
                  , currentDepositTxId = newCurrentDepositTxId
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
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
      Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{seenSnapshot}} ->
        case mSnapshot <|> snapshotFromSeen seenSnapshot of
          Just snapshot ->
            Open
              os
                { coordinatedHeadState =
                    chs
                      { confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures}
                      , seenSnapshot = LastSeenSnapshot snapshot.number
                      }
                }
          Nothing -> Hydra.Prelude.error "applyEvent: SnapshotConfirmed but no snapshot in event or seenSnapshot"
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
                  ConfirmedSnapshot{snapshot = Snapshot{utxo, utxoToCommit, version = snapshotVersion}} ->
                    coordinatedHeadState
                      { -- NOTE: Include utxoToCommit in localUTxO when the corresponding
                        -- increment has been finalized on-chain (i.e. the chain-observed
                        -- version has advanced past the snapshot's version). Without this,
                        -- a side-loaded deposit snapshot would leave the head unable to
                        -- spend the deposited UTxO.
                        localUTxO =
                          if currentVersion > snapshotVersion
                            then utxo <> fromMaybe mempty utxoToCommit
                            else utxo
                      , localTxs = mempty
                      , allTxs = mempty
                      , seenSnapshot = LastSeenSnapshot snapshotNumber
                      , decommitTx = Nothing
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
            Nothing -> Open os{coordinatedHeadState = chs{currentDepositTxId = chs.currentDepositTxId <|> Just depositTxId}}
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
      Open
        os@OpenState{coordinatedHeadState = coordinatedHeadState@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}} ->
          let Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot
           in Open
                os
                  { chainState
                  , coordinatedHeadState =
                      coordinatedHeadState
                        { decommitTx = Nothing
                        , version = newVersion
                        , -- If a snapshot is already in SeenSnapshot, all parties have
                          -- processed the ReqSn and sent AckSns — preserve it so that
                          -- snapshot can still complete and chain the next one with the
                          -- bumped version. Only reset when nothing is in-flight.
                          seenSnapshot = case seenSnapshot of
                            SeenSnapshot{} -> seenSnapshot
                            _ -> LastSeenSnapshot{lastSeen = confirmedSn}
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
              , remainingFanoutOutputs = Nothing
              , distributedFanoutOutputs = mempty
              }
      _otherState -> st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version, remainingFanoutOutputs, distributedFanoutOutputs} ->
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
            , remainingFanoutOutputs
            , distributedFanoutOutputs
            }
      _otherState -> st
  HeadFannedOut{chainState} ->
    case st of
      Closed _ ->
        Idle $
          IdleState
            { chainState
            }
      _otherState -> st
  HeadPartialFannedOut{distributedOutputs, remainingOutputs, chainState} ->
    case st of
      Closed cst ->
        Closed
          cst
            { chainState
            , remainingFanoutOutputs = Just remainingOutputs
            , distributedFanoutOutputs = distributedFanoutOutputs cst <> distributedOutputs
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
