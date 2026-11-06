{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen complete input handling before the remaining Program migration.
-- Earlier migrated rules use their frozen test implementations. Pure selection
-- helpers and primitive implementations remain shared.
module Test.Hydra.HeadLogic.LegacyHandlers (update) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.API.ClientInput (ClientInput (..), validateClientInput)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
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
  changes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.Program (AcknowledgementProgram, SnapshotProgram, TransactionProgram)
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment,
  ruleEnvironment,
 )
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FinalizedSnapshot (..),
  HeadState (..),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  isCollectingAcks,
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Operations (
  finalizedDepositTxId,
  isSubMultisetOf,
  nullOutputs,
  removeDistributedOutputs,
  sameOutputs,
  settledUTxO,
 )
import Hydra.Ledger (Ledger (..), ValidationError (..), applyTransactions)
import Hydra.Network qualified as Network
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..), mkHeadParameters)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, SyncedStatus (..), depositsForHead, syncedStatus)
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
import Hydra.Tx.Crypto (
  MultiSignature,
  Signature,
  Verified (..),
  verifyMultiSignature,
  verifyMultiSignatureBytes,
 )
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)
import Test.Hydra.HeadLogic.LegacyIncremental qualified as LegacyIncremental
import Test.Hydra.HeadLogic.LegacyPrograms qualified as LegacyPrograms
import Test.Hydra.HeadLogic.LegacySnapshotRequest (
  SettlingCommit (ContinueCommit, DropCommit),
  SnapshotRequest (..),
  chainedSnapshotRequest,
  isLeader,
  nextSnapshotRequest,
  scopedDeposits,
  settlingCommitAt,
 )

-- * The Coordinated Head protocol

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
      && configuredDepositPeriod == depositPeriod
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
  forall tx.
  IsTx tx =>
  Environment ->
  Ledger tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger chainTime st ttl pendingDeposits tx =
  Interpreter.runProgram ledger $ onOpenNetworkReqTxProgram (ruleEnvironment env) chainTime st ttl pendingDeposits tx

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
onOpenNetworkReqTxProgram = LegacyPrograms.onOpenNetworkReqTxProgram
{-# INLINE onOpenNetworkReqTxProgram #-}

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
  Ledger tx ->
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
  Outcome tx
onOpenNetworkReqSn env ledger pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit =
  Interpreter.runProgramWith (Interpreter.snapshotConfig env ledger) $
    onOpenNetworkReqSnProgram (ruleEnvironment env) pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit

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
onOpenNetworkReqSnProgram = LegacyPrograms.onOpenNetworkReqSnProgram
{-# INLINE onOpenNetworkReqSnProgram #-}

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
  Outcome tx
onOpenNetworkAckSn env chainTime pendingDeposits st otherParty signature sn =
  Interpreter.runProgramWith Interpreter.basicConfig $
    onOpenNetworkAckSnProgram (ruleEnvironment env) chainTime pendingDeposits st otherParty signature sn

onOpenNetworkAckSnProgram ::
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
onOpenNetworkAckSnProgram = LegacyPrograms.onOpenNetworkAckSnProgram
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
  Outcome tx
onClientRecover = LegacyIncremental.onClientRecover

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
      requireDecommitOutputs headId localUTxO decommitTx $
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
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec = LegacyIncremental.onOpenNetworkReqDec

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
onOpenChainTick :: forall tx. IsTx tx => Environment -> UTCTime -> PendingDeposits tx -> OpenState tx -> Outcome tx
onOpenChainTick env chainTime pendingDeposits st
  | snapshotInFlight seenSnapshot || not (isLeader parameters party nextSn) = noop
  | otherwise =
      let settling = settlingCommitAt env version chainTime deposits confirmed
          built = nextSnapshotRequest (settledUTxO version (getSnapshot confirmedSnapshot)) localTxs deposits currentDepositTxId decommitTx settling
       in case (settling, built) of
            -- The commit is still settling; requests carrying txs continue it.
            (Just (ContinueCommit _), _) -> noop
            -- The commit can no longer land: drop it so other deposits can be claimed.
            (Just DropCommit, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}) -> request requestTxs requestDecommitTx requestDepositTxId
            -- Spec: wait txω = ⊥ ∧ Uα = ∅.
            (Nothing, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId = Just depositTxId})
              | isNothing decommitTx -> request requestTxs requestDecommitTx (Just depositTxId)
            _ -> noop
 where
  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  newActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  newExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  -- All deposits, with their status updated to this tick's time. Empty
  -- deposits are ignored, because an empty increment cannot be posted.
  deposits = Map.filter (\Deposit{deposited} -> deposited /= mempty) (newActive <> newExpired <> pendingDeposits)

  request :: [tx] -> Maybe tx -> Maybe (TxIdType tx) -> Outcome tx
  request txs mDecommitTx mDepositTxId =
    newState SnapshotRequestDecided{snapshotNumber = nextSn}
      <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> txs) mDecommitTx mDepositTxId)

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
  Outcome tx
maybeRequestSnapshotAfterVersionBump parameters party nextSn mRequest version newVersion seenSnapshot =
  case mRequest of
    Just SnapshotRequest{requestTxs, requestDepositTxId}
      | isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot) ->
          newState SnapshotRequestDecided{snapshotNumber = nextSn}
            <> cause (NetworkEffect $ ReqSn newVersion nextSn (txId <$> requestTxs) Nothing requestDepositTxId)
    _ -> noop

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
  Outcome tx
onOpenChainIncrementTx env pendingDeposits openState newChainState newVersion depositTxId observedSnapshotNumber observedSignatures =
  newState CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
    <> adoptObservedSnapshotOrRequestNext
 where
  -- The increment proves on-chain that the snapshot it settled is fully
  -- signed. If that is the one we are still collecting AckSns for, adopt it
  -- with the on-chain multisignature instead of waiting on a peer.
  adoptObservedSnapshotOrRequestNext =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber, version = seenSnapshotVersion, depositTxId = seenDepositTxId}, signableBytes}
        | observedSnapshotNumber == seenSnNumber
        , seenSnapshotVersion + 1 == newVersion
        , seenDepositTxId == Just depositTxId
        , Verified <- verifyMultiSignatureBytes vkeys observedSignatures signableBytes ->
            newState SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
              <> maybeRequestSnapshotAfterVersionBump parameters party (seenSnNumber + 1) (requestOn snapshot) version newVersion (LastSeenSnapshot seenSnNumber)
      _ ->
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
  Outcome tx
onOpenChainDecrementTx env pendingDeposits openState newChainState newVersion distributedUTxO observedSnapshotNumber observedSignatures =
  newState
    DecommitFinalized
      { chainState = newChainState
      , headId
      , newVersion
      , distributedUTxO
      }
    <> adoptObservedSnapshotOrRequestNext
 where
  -- Same reasoning as in 'onOpenChainIncrementTx': the decrement proves the
  -- snapshot it settled is fully signed, so adopt it if it is the one in flight.
  adoptObservedSnapshotOrRequestNext =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber, version = seenSnapshotVersion, utxoToDecommit = seenDecommit}, signableBytes}
        | observedSnapshotNumber == seenSnNumber
        , seenSnapshotVersion + 1 == newVersion
        , isJust seenDecommit
        , Verified <- verifyMultiSignatureBytes vkeys observedSignatures signableBytes ->
            newState SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
              <> maybeRequestSnapshotAfterVersionBump parameters party (seenSnNumber + 1) (requestOn snapshot) version newVersion (LastSeenSnapshot seenSnNumber)
      _ ->
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
  Outcome tx
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
        _ -> noop
 where
  repost incrementingSnapshot depositTxId =
    cause
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
  Outcome tx
maybeRepostDecrementTx headSeed headId parameters mDecommitTx confirmedSnapshot finalizedDecommit rolledBackSlot =
  case (mDecommitTx, confirmedSnapshot) of
    (Just _, ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToDecommit = Just _}, signatures}) ->
      repost ConfirmedSnapshot{snapshot, signatures}
    _ ->
      case finalizedDecommit of
        Just FinalizedSnapshot{snapshot, observedAtSlot}
          | rolledBackSlot < observedAtSlot ->
              repost snapshot
        _ -> noop
 where
  repost decrementingSnapshot =
    cause
      OnChainEffect
        { postChainTx =
            DecrementTx
              { headSeed
              , headId
              , headParameters = parameters
              , decrementingSnapshot
              }
        }

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, ξ)
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
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline observedSignatures =
  adoptClosedSnapshot
    <> newState HeadClosed{headId, snapshotNumber = closedSnapshotNumber, chainState = newChainState, contestationDeadline}
    & maybePostContest
 where
  -- The multisignature verifying over our own signable bytes proves the head
  -- closed with exactly the snapshot we saw. Anything else leaves the confirmed
  -- snapshot as it is.
  adoptClosedSnapshot =
    case seenSnapshot of
      SeenSnapshot{snapshot = snapshot@Snapshot{number = seenSnNumber}, signableBytes}
        | seenSnNumber == closedSnapshotNumber
        , Verified <- verifyMultiSignatureBytes vkeys observedSignatures signableBytes ->
            newState SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
      _ -> noop

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
    -- The pending commit is L1-relevant state, and since the binding change it is
    -- the deposit that identifies it, not the committed content.
    | requestedDeposit /= lastSeenDeposit = sideLoadFailed SideLoadDepositTxIdInvalid{requestedDeposit, lastSeenDeposit}
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
  Outcome tx
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline observedSignatures =
  if
    | -- The multisignature verifying over our own signable bytes proves the
      -- head was contested with exactly the snapshot we saw.
      Just snapshot@Snapshot{number = unconfirmedSn} <- unconfirmedSnapshot
    , unconfirmedSn == snapshotNumber
    , Verified <- verifyMultiSignature vkeys observedSignatures snapshot ->
        newState SnapshotConfirmed{headId, snapshot = Just snapshot, signatures = observedSignatures}
          <> newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
    | -- Spec: if ̅S.s > sc
      number (getSnapshot confirmedSnapshot) > snapshotNumber ->
        -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
        --       ξ ← ̅S.σ
        --       postTx (contest, ̅S.v, ̅S.s, η, ξ)
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
  Outcome tx
onClosedClientFanout closedState =
  -- A plain 'Fanout' is a full fanout by definition, and is only reachable from
  -- 'Closed'.
  fanoutStepStateChange headId FullFanoutStep fullUTxO
    <> emitFanoutStep FullFanoutStep confirmedSnapshot version headSeed contestationDeadline
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
  Outcome tx
fanoutStepStateChange headId step remainingOutputs =
  case step of
    -- The selection is not distributed as one: it becomes a full fanout, and this
    -- node drives the rest of it.
    FullFanoutStep -> newState HeadFanoutInitiated{headId, remainingOutputs}
    -- 'FinalStep' posts a transaction and nothing else, so the selection has to
    -- be recorded for it too: a rollback or restart before that transaction
    -- lands otherwise leaves the driver with no selection to resume from.
    FinalStep{stepDistribute} -> recordSelection stepDistribute
    PartialStep{stepDistribute} -> recordSelection stepDistribute
 where
  recordSelection selection = newState HeadPartialFanoutSelected{headId, remainingOutputs, selection}

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
  Outcome tx
onClosedClientPartialFanout closedState selection
  | nullOutputs selection || not (selection `isSubMultisetOf` fullUTxO) =
      cause . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (Closed closedState)
  | otherwise =
      -- A selection covering the whole head becomes a full fanout - what the user
      -- means by "fan out everything", and the only option for a head holding a
      -- single UTxO, since a non-final batch has to leave one behind.
      fanoutStepStateChange headId step fullUTxO
        <> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
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
  Outcome tx
onPartialFanoutClientPartialFanout pfs selection
  | nullOutputs selection || not (selection `isSubMultisetOf` remainingOutputs) =
      cause . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (FanoutProgress pfs)
  | otherwise =
      fanoutStepStateChange headId step remainingOutputs
        <> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
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
  Outcome tx
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  newState HeadFannedOut{headId, finalizedOutputs = fanoutUTxO, chainState = newChainState}
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
  Outcome tx
onClosedChainPartialFanoutTx closedState newChainState observedDistributed =
  let fullUTxO = computeFullFanoutUTxO closedState
      remaining = removeDistributedOutputs (outputsOfUTxO observedDistributed) fullUTxO
      distributedUTxO = withoutUTxO fullUTxO remaining
   in newState
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
  Outcome tx
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
        newState
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
            AwaitingSelection -> noop
   in record <> continue
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
  Outcome tx
onPartialFanoutChainFanoutTx pfs newChainState fanoutUTxO =
  newState HeadFannedOut{headId, finalizedOutputs = distributedOutputs <> fanoutUTxO, chainState = newChainState}
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

-- | Post the transaction a decided 'NextFanoutStep' calls for. The chain layer
-- chunks a full fanout, and sizes a partial one, dynamically.
--
-- Transactions only: any accompanying state change belongs to the caller that
-- owns the head state, via 'fanoutStepStateChange'. That keeps this safe to call
-- where effects can be applied but state changes cannot, as in the startup
-- re-post in 'Hydra.Node.runHydraNode'.
emitFanoutStep ::
  IsTx tx =>
  -- | The step to emit, from 'nextFanoutStep'.
  NextFanoutStep tx ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  Outcome tx
emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline =
  case step of
    FinalStep{stepDistribute} ->
      cause
        OnChainEffect
          { postChainTx =
              FinalPartialFanoutTx
                { utxoToDistribute = stepDistribute
                , headSeed
                , contestationDeadline
                }
          }
    FullFanoutStep ->
      cause OnChainEffect{postChainTx = mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline}
    PartialStep{stepDistribute, stepProof} ->
      cause
        OnChainEffect
          { postChainTx =
              PartialFanoutTx
                { utxoToDistribute = stepDistribute
                , utxoForProof = stepProof
                , headSeed
                , contestationDeadline
                }
          }

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
repostFanoutStep :: IsTx tx => PartialFanoutState tx -> Outcome tx
repostFanoutStep pfs =
  case currentFanoutStep pfs of
    -- Manual mode paused on the user: nothing to re-post, wait for the next
    -- 'PartialFanout' command.
    Nothing -> noop
    -- Only the transaction is re-posted: this node is the driver already, and
    -- re-posting is not a new decision to record. So this emits no state
    -- changes, which is what lets 'Hydra.Node.runHydraNode' apply its effects
    -- alone at startup.
    Just step ->
      emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
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
  Outcome tx
handleOutOfSync Environment{unsyncedPeriod} now chainPoint chainTime syncStatus =
  -- Emit only on an actual sync-status transition, rather than on every tick, so
  -- clients are not flooded (see issue #2749). The continuous drift value is
  -- exposed as a metric ('hydra_chain_drift_seconds') instead.
  case (syncStatus, newSyncStatus) of
    (InSync, CatchingUp) -> newState NodeUnsynced{chainSlot, chainTime, drift}
    (CatchingUp, InSync) -> newState NodeSynced{chainSlot, chainTime, drift}
    _ -> noop
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
  Outcome tx ->
  Outcome tx
requireDecommitOutputs headId localUTxO decommitTx continue
  | utxoFromTx decommitTx == mempty =
      newState
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
update env ledger now nodeState ev
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
      Error . SideLoadSnapshotFailed $ SideLoadUTxOSetTooLarge{utxoCount, maxAllowed}
  | otherwise =
      case nodeState of
        NodeCatchingUp{headState, chainPointTime} ->
          updateCatchingUpHead env ledger now chainPointTime nodeState.pendingDeposits headState ev (syncedStatus nodeState)
        NodeInSync{headState, chainPointTime} ->
          updateInSyncHead env ledger now chainPointTime nodeState.pendingDeposits headState ev (syncedStatus nodeState)

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
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline, signatures}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline signatures
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState, ChainInput Tick{chainTime, chainPoint}) ->
    -- XXX: We originally forgot the normal TickObserved state event here and so
    -- time did not advance in an open head anymore. This is a hint that we
    -- should compose event handling better.
    newState TickObserved{chainPoint, chainTime}
      <> handleOutOfSync env now chainPoint chainTime syncStatus
      <> onChainTick env pendingDeposits chainTime
      <> onOpenChainTick env chainTime (scopedDeposits openState pendingDeposits) openState
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnIncrementTx{headId, newVersion, depositTxId, snapshotNumber, signatures}, newChainState})
    | ourHeadId == headId ->
        onOpenChainIncrementTx env (scopedDeposits openState pendingDeposits) openState newChainState newVersion depositTxId snapshotNumber signatures
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedUTxO, snapshotNumber, signatures}, newChainState})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx env (scopedDeposits openState pendingDeposits) openState newChainState newVersion distributedUTxO snapshotNumber signatures
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline, signatures}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline signatures
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed ClosedState{contestationDeadline, readyToFanoutSent, headId}, ChainInput Tick{chainTime, chainPoint})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        newState TickObserved{chainPoint, chainTime}
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
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnPartialFanoutTx{headId, distributedOutputs}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainPartialFanoutTx partialFanoutState newChainState distributedOutputs
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (FanoutProgress partialFanoutState@PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId, fanoutUTxO}, newChainState})
    | ourHeadId == headId ->
        onPartialFanoutChainFanoutTx partialFanoutState newChainState fanoutUTxO
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Node-level: deposit/recover observations scoped to our head
  ( Open OpenState{headId = ourHeadId, headSeed, parameters, coordinatedHeadState = CoordinatedHeadState{finalizedCommit}}
    , ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState}
    )
      | ourHeadId == headId ->
          newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
            -- The finalized deposit observed *again* means a rollback erased
            -- both the deposit and its finalized increment, and the deposit tx
            -- just re-landed on the new chain. The increment re-post issued at
            -- rollback time necessarily failed (the deposit UTxO did not exist
            -- on the new chain yet) and is not retried anywhere else, while
            -- every alternative settlement of this deposit is deliberately
            -- blocked (see 'finalizedDepositTxId'): re-post the increment
            -- now that the deposit is on chain again, see #2741.
            <> case finalizedCommit of
              Just FinalizedSnapshot{snapshot}
                | finalizedDepositTxId finalizedCommit == Just depositTxId ->
                    cause
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
              _ -> Continue [] []
      | otherwise ->
          Continue [] []
  (Closed ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        Continue [] []
  -- Mirror the 'Closed' case while mid-fanout: a deposit observed during a
  -- (partial) fanout must still be recorded so it remains recoverable via
  -- 'Recover'. Without this the input falls through to 'Error' and is dropped.
  (FanoutProgress PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{headId, depositTxId, deposited, created, deadline}, newChainState})
    | ourHeadId == headId ->
        newState DepositRecorded{chainState = newChainState, headId, depositTxId, deposited, created, deadline}
    | otherwise ->
        Continue [] []
  (Idle _, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    Continue [] []
  -- Deposit recovery is node-level: emit DepositRecovered for any tracked deposit
  -- regardless of which head is currently active. Previous-head deposits survive
  -- fanout in 'pendingDeposits', so recovery works even while a new head is Open.
  -- Unrelated deposits (never in pendingDeposits) are silently ignored.
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState})
    | Map.member recoveredTxId pendingDeposits ->
        newState DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
    | otherwise ->
        Continue [] []
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
      newState ChainRolledBack{chainState = rolledBackChainState}
        <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
        <> maybeRepostIncrementTx headSeed headId parameters (depositsForHead headId pendingDeposits) confirmedSnapshot finalizedCommit (chainStateSlot rolledBackChainState)
        <> maybeRepostDecrementTx headSeed headId parameters decommitTx confirmedSnapshot finalizedDecommit (chainStateSlot rolledBackChainState)
  -- FanoutProgress + Rollback: re-post the next fanout step so the fanout
  -- resumes rather than stalling (the in-flight fanout tx may have been rolled
  -- back). Mirrors the Open re-post above.
  (FanoutProgress partialFanoutState, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
      <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
      <> repostFanoutStep partialFanoutState
  -- General
  (_, ChainInput Rollback{rolledBackChainState, chainTime}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
      <> handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
  (_, ChainInput Tick{chainTime, chainPoint}) ->
    newState TickObserved{chainPoint, chainTime}
      <> handleOutOfSync env now chainPoint chainTime syncStatus
      <> onChainTick env pendingDeposits chainTime
  (_, ChainInput PostTxError{postTxError = StalePartialFanoutTx}) ->
    -- The chain advanced past this step before we could post it (another node
    -- was faster). The chain observation loop already emitted the correct next
    -- step, so this is safe to ignore.
    noop
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
        newState HeadFanoutReverted{headId}
          <> cause (ClientEffect ServerOutput.PostTxOnChainFailed{postChainTx, postTxError})
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
handleNetworkInput env ledger chainPointTime pendingDeposits st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent env.configuredPeers conn
  -- Open
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env ledger chainPointTime openState ttl (scopedDeposits openState pendingDeposits) tx
  -- NOTE: 'ReqSn' gets the unfiltered (per-head) deposits: it distinguishes a
  -- requested deposit that is blocked by a finalized increment (hard error)
  -- from one that is simply not known locally (wait).
  (Open openState@OpenState{headId = ourHeadId}, NetworkInput ttl (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx depositTxId})) ->
    onOpenNetworkReqSn env ledger (depositsForHead ourHeadId pendingDeposits) chainPointTime openState ttl sender sv sn txIds decommitTx depositTxId
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    onOpenNetworkAckSn env chainPointTime (scopedDeposits openState pendingDeposits) openState sender snapshotSignature sn
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ledger ttl chainPointTime (scopedDeposits openState pendingDeposits) openState transaction
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
  Network.BroadcastStalled{pendingBroadcasts, stallReason} ->
    newState NetworkBroadcastStalled{pendingBroadcasts, stallReason}
  Network.BroadcastResumed ->
    newState NetworkBroadcastResumed

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
    cause . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    Error $ UnhandledInput ev st
