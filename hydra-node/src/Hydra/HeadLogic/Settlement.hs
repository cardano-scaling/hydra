{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Settling funds into and out of an open head: deposits and their status,
-- decommits, the increment and decrement transactions that finalize them, and
-- re-posting a settlement a rollback erased.
--
-- The functions here decide the transactions to post and the 'StateChanged'
-- events to record, as 'Hydra.HeadLogic.update' dispatches them. The folds that
-- apply those events live in "Hydra.HeadLogic.Aggregate".
module Hydra.HeadLogic.Settlement where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  PostChainTx (..),
 )
import Hydra.Chain.ChainState (
  ChainSlot (..),
  IsChainState (..),
  chainStateSlot,
 )
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
 )
import Hydra.HeadLogic.Input (TTL)
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
import Hydra.HeadLogic.Snapshot.Request (
  SettlingCommit (..),
  SnapshotRequest (..),
  chainedSnapshotRequest,
  isLeader,
  maybeRequestSnapshotAfterVersionBump,
  nextSnapshotRequest,
  settledUTxO,
  settlingCommitAt,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (..),
  HeadState (..),
  OpenState (..),
  SeenSnapshot (..),
  Settlement (..),
  SettlementConflict,
  SettlementStatus (..),
  Settlements,
  UnretainedSettlements,
  adjustSettlementStatus,
  deleteUnretained,
  firstErasedSettlement,
  insertUnretained,
  mapSettlementStatuses,
  mapUnretainedStatuses,
  pruneRetainedSettlements,
  pruneUnretained,
  settlementDeposits,
  settlementEntries,
  snapshotInFlight,
  tryInsertSettlement,
 )
import Hydra.Ledger (
  Ledger (..),
  ValidationError (..),
 )
import Hydra.Network.Message (
  Message (..),
 )
import Hydra.Node.Environment (
  Environment (..),
 )
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  DepositStatus (..),
  PendingDeposits,
  SyncedStatus (..),
  depositsForHead,
  retentionCutoff,
 )
import Hydra.Tx (
  HeadId,
  HeadSeed,
  IsTx (..),
 )
import Hydra.Tx.Crypto (
  MultiSignature,
  Verified (..),
  verifyMultiSignatureBytes,
 )
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (..))
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
 )

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
  -- | Deposits claimed by the retained increments of an open head (see
  -- 'openRetainedDeposits').
  Set (TxIdType tx) ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits blockedDeposits recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Error $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited}
      -- The deposit resurfaced because its finalized increment was rolled
      -- back: the deposited funds are already merged into the head, so
      -- recovering them on-chain would corrupt the L2 ledger. Only re-posting
      -- the increment settles this deposit, see #2741.
      | recoverTxId `Set.member` blockedDeposits ->
          Error $ RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}
      | otherwise ->
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
onOpenNetworkReqDec env ledger ttl ChainPointTime{currentSlot, currentChainTime} pendingDeposits openState decommitTx =
  -- Spec: wait 𝑈𝛼 = ∅ ^ txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $
    requireDecommitOutputs headId localUTxO decommitTx $
      -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
      -- Spec: txω ← tx
      newState DecommitRecorded{headId, decommitTx}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        <> maybeRequestSnapshot
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
          then wait $ WaitOnUnresolvedCommit{commitUTxO}
          else
            newState
              DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason = DepositInFlight{depositTxId, commitUTxO}
                }
    | otherwise =
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

  -- May carry a settling commit instead of the decommit.
  maybeRequestSnapshot =
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then cause (NetworkEffect (ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId))
      else noop

  SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} =
    nextSnapshotRequest confirmedUTxO localTxs pendingDeposits currentDepositTxId (Just decommitTx) (settlingCommitAt env version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot))

  Environment{party} = env

  Ledger{applyTransactions} = ledger

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

determineNextDepositStatus :: forall tx. Environment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  -- NOTE: the annotation (and hence the forall) disambiguates the record
  -- update: 'status' is also a field of 'Settlement'.
  (\deposit -> (deposit :: Deposit tx){status = determineStatus deposit}) <$> pendingDeposits
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

-- | Observe a increment transaction. If the outputs match the ones of the
-- pending commit UTxO, then we consider the deposit/increment finalized, and remove the
-- increment UTxO from 'pendingDeposits' from the local state.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainIncrementTx ::
  IsChainState tx =>
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
    <> repostAfterRelanding pendingDeposits openState newChainState newVersion
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
    chainedSnapshotRequest (settledUTxO newVersion snapshot) localTxs (Map.delete depositTxId (eligibleDeposits openState pendingDeposits)) Nothing decommitTx Nothing

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
  IsChainState tx =>
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
    <> repostAfterRelanding pendingDeposits openState newChainState newVersion
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
    chainedSnapshotRequest (settledUTxO newVersion snapshot) localTxs (eligibleDeposits openState pendingDeposits) currentDepositTxId decommitTx Nothing

  HeadParameters{parties} = parameters

  vkeys = vkey <$> parties

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

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

-- ** Settlement retention and re-posting (#2741)

-- | The deposits claimed by retained increments. Such a deposit is only pending
-- again because a rollback erased its increment, and only re-posting that
-- increment settles it ('repostErased'). It must never be proposed for a
-- snapshot again, since the head already counts its funds, nor recovered,
-- which would corrupt the L2 ledger. See #2741.
retainedDeposits :: Settlements tx -> Set (TxIdType tx)
retainedDeposits =
  settlementDeposits

-- | The deposits handlers of an open head may act on: scoped to the head and
-- excluding the deposits claimed by retained increments (see
-- 'retainedDeposits').
eligibleDeposits :: IsTx tx => OpenState tx -> PendingDeposits tx -> PendingDeposits tx
eligibleDeposits OpenState{headId, coordinatedHeadState} =
  (`Map.withoutKeys` blocked) . depositsForHead headId
 where
  confirmed = getSnapshot coordinatedHeadState.confirmedSnapshot
  blocked = retainedDeposits coordinatedHeadState.settlements <> alreadyApplied
  alreadyApplied =
    if coordinatedHeadState.version > confirmed.version
      then maybe mempty Set.singleton confirmed.depositTxId
      else mempty

-- | The deposits blocked by the retained increments of an open head.
--
-- Empty for any other head state on purpose. An increment can only settle into
-- an open head, so once the head closes the retained snapshots can never claim
-- their deposits again. The way out for such a deposit is then to recover it
-- after its deadline and leave the deposited outputs out of the fanout with
-- 'PartialFanout', so 'Recover' must not stay blocked after close.
openRetainedDeposits :: IsTx tx => HeadState tx -> Set (TxIdType tx)
openRetainedDeposits = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{settlements}} -> retainedDeposits settlements
  _ -> mempty

-- | Retain the snapshot whose increment or decrement was seen bumping the
-- on-chain version to @newVersion@, if that is the locally confirmed snapshot
-- (based on the version just below, carrying a commit or decommit). Only that
-- snapshot can settle again if a rollback erases the settlement, and
-- 'confirmedSnapshot' may move past it. Its confirmed txs are blanked, see
-- 'Settlement'.
--
-- Nothing is retained when the observation came before the snapshot confirmed
-- locally, because another party collected the last signature and posted
-- first. Retention then happens when the snapshot confirms, see the
-- 'SnapshotConfirmed' branch of 'applyEvent'. See #2741.
retainSettlement ::
  IsTx tx =>
  ChainSlot ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Settlements tx ->
  Either (SettlementConflict tx) (Settlements tx)
retainSettlement slot = retainSettlementWith (Landed slot)

-- | 'retainSettlement' with the status the entry gets: landed at the
-- observation slot, or the status noted for a version bump whose snapshot had
-- not confirmed yet (see 'UnretainedSettlements').
retainSettlementWith ::
  IsTx tx =>
  SettlementStatus ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Settlements tx ->
  Either (SettlementConflict tx) (Settlements tx)
retainSettlementWith status newVersion confirmedSnapshot settlements =
  case confirmedSnapshot of
    ConfirmedSnapshot{snapshot = snapshot@Snapshot{version, utxoToCommit, utxoToDecommit}, signatures}
      | version + 1 == newVersion
      , isJust utxoToCommit || isJust utxoToDecommit ->
          -- Keep an entry already retained for this version. That entry is the
          -- snapshot whose settlement was actually observed, with its real slot
          -- and its erased mark. A later snapshot at the same version carries
          -- the same action but is not the one that settled, and this function
          -- is also called with our own version from the 'SnapshotConfirmed'
          -- race branch. Replacing the entry would re-stamp the slot and undo
          -- 'markErased'; 'nextErasedSettlement' would then find nothing,
          -- 'repostErased' would post nothing, and our version would stay above
          -- the chain's for good, which a close cannot express. A settlement
          -- that really landed again is re-stamped by 'landSettlement' instead.
          tryInsertSettlement
            Settlement{snapshot = ConfirmedSnapshot{snapshot = snapshot{confirmed = []}, signatures}, status}
            settlements
    _ -> pure settlements

-- | Record a settlement seen bumping the on-chain version to @newVersion@ at
-- @slot@: retain the confirmed snapshot if it is the one that settled
-- ('retainSettlement'), stamp the retained entry as landed ('landSettlement'),
-- and if nothing is retained at that version, note the bump for the snapshot
-- still to confirm ('recordUnretained'). The observed action must match before
-- retaining the current snapshot; another action can settle at the same version.
recordSettlement :: IsTx tx => ChainSlot -> SnapshotVersion -> (Snapshot tx -> Bool) -> CoordinatedHeadState tx -> Either (SettlementConflict tx) (CoordinatedHeadState tx)
recordSettlement slot newVersion matchesObserved chs@CoordinatedHeadState{confirmedSnapshot, settlements, unretained} = do
  retained <-
    if matchesObserved (getSnapshot confirmedSnapshot)
      then retainSettlement slot newVersion confirmedSnapshot settlements
      else pure settlements
  let settlements' = landSettlement slot newVersion retained
  pure chs{settlements = settlements', unretained = recordUnretained slot newVersion settlements' unretained}

-- | Note a bump to @newVersion@ seen at @slot@ whose snapshot is not retained,
-- because it has not confirmed locally yet, see 'UnretainedSettlements'. Once
-- that version is retained the note is dropped.
recordUnretained :: ChainSlot -> SnapshotVersion -> Settlements tx -> UnretainedSettlements -> UnretainedSettlements
recordUnretained slot newVersion settlements unretained
  | newVersion == 0 = unretained
  | Map.member key (settlementEntries settlements) = deleteUnretained key unretained
  | otherwise = insertUnretained key (Landed slot) unretained
 where
  key = newVersion - 1

-- | Record that the retained settlement bumping the on-chain version to
-- @newVersion@ landed, or landed again, at the given slot, so that a rollback
-- erasing it again still leads to a re-post.
landSettlement :: ChainSlot -> SnapshotVersion -> Settlements tx -> Settlements tx
landSettlement slot newVersion settlements
  | newVersion == 0 = settlements
  | otherwise = adjustSettlementStatus (newVersion - 1) (Landed slot) settlements

-- | Mark the retained settlements and notes that a rollback to the given slot
-- erased. The rollback point is the last block both chains share, so a
-- settlement observed exactly there is still on chain: strictly (<).
markErased :: ChainSlot -> CoordinatedHeadState tx -> CoordinatedHeadState tx
markErased rolledBackSlot chs@CoordinatedHeadState{settlements, unretained} =
  chs
    { settlements = mapSettlementStatuses erase settlements
    , unretained = mapUnretainedStatuses erase unretained
    }
 where
  erase = \case
    Landed{observedAtSlot} | rolledBackSlot < observedAtSlot -> Erased
    status -> status

-- | Drop the retained settlements and notes no rollback can reach anymore, see
-- 'retentionCutoff'. This bounds the map; without it an open head would keep
-- one snapshot per settlement forever.
--
-- Erased entries are never dropped: they still have to be re-posted, and the
-- lowest one holds back every later re-post ('nextErasedSettlement').
pruneSettlements ::
  IsTx tx =>
  -- | Rollback horizon
  ChainSlot ->
  -- | Current slot
  ChainSlot ->
  CoordinatedHeadState tx ->
  CoordinatedHeadState tx
pruneSettlements horizon slot chs@CoordinatedHeadState{settlements, unretained} =
  chs
    { settlements = pruneRetainedSettlements cutoff settlements
    , unretained = pruneUnretained cutoff unretained
    }
 where
  cutoff = retentionCutoff horizon slot

-- | Update the coordinated head state of an open head; any other head state is
-- left alone.
onCoordinatedHeadState :: (CoordinatedHeadState tx -> CoordinatedHeadState tx) -> HeadState tx -> HeadState tx
onCoordinatedHeadState f = \case
  Open os@OpenState{coordinatedHeadState} -> Open os{coordinatedHeadState = f coordinatedHeadState}
  other -> other

-- | The erased settlement with the lowest version, which is the one the chain
-- accepts next: the rollback took the on-chain version back to its base, and
-- each settlement bumps the version by one.
--
-- Only 'markErased' marks an entry erased, from an exact observation slot, so
-- an erased entry always means a settlement that must land again; the lowest
-- one holds back all later re-posts.
nextErasedSettlement :: Settlements tx -> Maybe (Settlement tx)
nextErasedSettlement = firstErasedSettlement

-- | Post the increment or decrement of a signed snapshot.
postSettlement :: IsTx tx => HeadSeed -> HeadId -> HeadParameters -> ConfirmedSnapshot tx -> Outcome tx
postSettlement headSeed headId headParameters snapshot =
  case getSnapshot snapshot of
    Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId} ->
      cause OnChainEffect{postChainTx = IncrementTx{headSeed, headId, headParameters, incrementingSnapshot = snapshot, depositTxId}}
    Snapshot{utxoToDecommit = Just _} ->
      cause OnChainEffect{postChainTx = DecrementTx{headSeed, headId, headParameters, decrementingSnapshot = snapshot}}
    _ -> noop

-- | Re-post the in-flight settlement of the confirmed snapshot, given the
-- retained settlements as they are after a rollback, or after one landed
-- again. Its
-- increment or decrement was posted when the snapshot confirmed and never
-- observed, so it may have been in a rolled back block.
--
-- Nothing is posted while a retained settlement is erased. The chain only
-- accepts the lowest version, so the erased ones must land first, in order,
-- which the ticks do ('repostErased'). The in-flight one follows when the last
-- erased one is seen landing again ('repostAfterRelanding'). See #2741.
repostInFlightSettlement ::
  IsTx tx =>
  OpenState tx ->
  PendingDeposits tx ->
  Settlements tx ->
  Outcome tx
repostInFlightSettlement OpenState{headSeed, headId, parameters, coordinatedHeadState} pendingDeposits settlements
  | isJust (nextErasedSettlement settlements) = noop
  -- The confirmed snapshot is retained, so its settlement was observed and is
  -- on chain: nothing is in flight.
  | Map.member (getSnapshot confirmedSnapshot).version (settlementEntries settlements) = noop
  | otherwise = repostInFlight
 where
  CoordinatedHeadState{confirmedSnapshot, decommitTx} = coordinatedHeadState

  -- NOTE: the deposit comes from the confirmed snapshot itself, not from
  -- 'currentDepositTxId'. Only the deposit bound into the signed snapshot can be
  -- claimed on-chain, and 'DepositActivated' can set 'currentDepositTxId' to an
  -- unrelated deposit after that snapshot was confirmed. A deposit still
  -- pending means its increment did not settle yet.
  repostInFlight = case getSnapshot confirmedSnapshot of
    Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId}
      | Map.member depositTxId pendingDeposits ->
          postSettlement headSeed headId parameters confirmedSnapshot
    Snapshot{utxoToDecommit = Just _}
      | isJust decommitTx ->
          postSettlement headSeed headId parameters confirmedSnapshot
    _ -> noop

-- | Post, on a tick, the erased settlement due next, while it can land: a
-- decrement always can, an increment only once its deposit is back on the
-- chain we follow, since the rollback may have erased the deposit tx too.
--
-- Erased settlements are posted here and nowhere else. Every block brings a
-- tick after its observations, so this runs once per block until the
-- settlement is seen landing again, which marks it landed. A post that failed
-- for a passing reason is retried on the next block. A node restarted with an
-- erased entry resumes by itself. And the first block of the new fork gets to
-- bring the settlement back on its own before anything is posted, which a
-- fork built from the same mempool usually does. A duplicate of one already
-- landing again is refused by the chain, at no cost but a
-- 'PostTxOnChainFailed'. The next erased one follows one block after the
-- previous one landed. See #2741.
--
-- Nothing is posted while this node is catching up. Rolling forward through
-- history brings a tick per block replayed, as fast as the node can process
-- them, so posting here would submit the same transaction once per block the
-- node is behind. Those submissions are built against a chain view that is
-- behind as well, and the settlement may already have landed on the part of
-- the chain the node has not reached yet. The first tick after the node is in
-- sync posts it.
repostErased :: IsTx tx => SyncedStatus -> OpenState tx -> PendingDeposits tx -> Outcome tx
repostErased syncStatus OpenState{headSeed, headId, parameters, coordinatedHeadState = CoordinatedHeadState{settlements}} pendingDeposits =
  case (syncStatus, nextErasedSettlement settlements) of
    (InSync, Just Settlement{snapshot})
      | canLand (getSnapshot snapshot) -> postSettlement headSeed headId parameters snapshot
    _ -> noop
 where
  canLand Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId} = Map.member depositTxId pendingDeposits
  canLand _ = True

-- | After an increment or decrement was observed: if it landed again (its
-- 'newVersion' is not ahead of our 'version', which never goes down, so this
-- settlement was applied before and then erased by a rollback) and it was the
-- last erased one, post the in-flight settlement it was holding back, see
-- 'repostInFlightSettlement'.
repostAfterRelanding ::
  IsChainState tx =>
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  SnapshotVersion ->
  Outcome tx
repostAfterRelanding pendingDeposits openState newChainState newVersion
  | newVersion <= version =
      repostInFlightSettlement openState pendingDeposits (landSettlement (chainStateSlot newChainState) newVersion settlements)
  | otherwise = noop
 where
  OpenState{coordinatedHeadState = CoordinatedHeadState{version, settlements}} = openState

-- * HeadState aggregate helpers

-- | Whether a finalized increment or decrement at 'newVersion' was applied
-- before and re-landed after a rollback. The local version never rolls back.
isReobservation :: SnapshotVersion -> CoordinatedHeadState tx -> Bool
isReobservation newVersion CoordinatedHeadState{version} = newVersion <= version
