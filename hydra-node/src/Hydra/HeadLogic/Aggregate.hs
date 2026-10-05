{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Folding 'StateChanged' events into the 'NodeState': the aggregate side of
-- the head logic, where 'Hydra.HeadLogic.update' is the decision side.
module Hydra.HeadLogic.Aggregate where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Hydra.Chain (
  ChainStateHistory,
  initHistory,
  pushNewState,
  rollbackHistory,
  setLastKnown,
 )
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Close (closedToFanoutProgress, fanoutProgressToClosed, recordedSelectionMode, rewindFanoutProgress)
import Hydra.HeadLogic.Outcome (Outcome (..), StateChanged (..))
import Hydra.HeadLogic.Settlement (
  isReobservation,
  markErased,
  onCoordinatedHeadState,
  pruneSettlements,
  recordSettlement,
  retainSettlementWith,
  retainedDeposits,
  settledUTxO,
 )
import Hydra.HeadLogic.Snapshot.Request (seenSnapshotAfterVersionBump)
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FanoutStepLanded (..),
  HeadState (..),
  IdleState (IdleState, chainState),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  SettlementStatus (..),
  getChainState,
  mkSeenSnapshot,
  seenSnapshotNumber,
  setChainState,
 )
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), consumeDeposit, recordDeposit, rollbackDeposits, updateDeposit)
import Hydra.Tx (HeadId, IsTx (..), txId, utxoFromTx, withoutUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotVersion)

-- | Reflect 'StateChanged' events onto the 'NodeState' aggregateNodeState.
-- Events carrying a 'HeadId' that does not match the current state are silently
-- ignored, preventing cross-head state contamination during event replay.
-- Events without a 'HeadId' are always applied.
aggregateNodeState ::
  IsChainState tx =>
  -- | Rollback horizon of the network, see 'Hydra.Node.Environment.rollbackHorizon'
  ChainSlot ->
  NodeState tx ->
  StateChanged tx ->
  NodeState tx
aggregateNodeState rollbackHorizon nodeState sc =
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
              recordDeposit rollbackHorizon (chainStateSlot chainState) depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} $
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
              consumeDeposit rollbackHorizon (chainStateSlot chainState) depositTxId $
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
              consumeDeposit rollbackHorizon (chainStateSlot chainState) depositTxId $ case st of
                Open os ->
                  nodeState
                    { headState =
                        Open $
                          settlementFinalized
                            chainState
                            newVersion
                            -- Only convergence bookkeeping: in particular 'localUTxO'
                            -- must not absorb the deposit again (its outputs may have
                            -- been spent on L2 in the meantime and the union would
                            -- resurrect them) and an unrelated deposit already parked in
                            -- 'currentDepositTxId' for the next snapshot must be left
                            -- alone. See #2741.
                            (\chs -> chs{currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId})
                            ( \chs ->
                                chs
                                  { -- NOTE: This must correspond to the just finalized
                                    -- depositTxId, but we should not verify this here.
                                    currentDepositTxId = Nothing
                                  , localUTxO = chs.localUTxO <> maybe mempty (.deposited) (Map.lookup depositTxId nodeState.pendingDeposits)
                                  }
                            )
                            os
                    }
                _ ->
                  nodeState{headState = st}
            TickObserved{chainPoint, chainTime} ->
              -- Retained settlements no rollback can reach anymore are dropped.
              nodeState{headState = onCoordinatedHeadState (pruneSettlements rollbackHorizon (chainPointSlot chainPoint)) st, chainPointTime = chainPointTimeState{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
            ChainRolledBack{chainState} ->
              -- Deposits are derived from L1: restore the view at the rolled
              -- back slot. A deposit whose increment or recover was erased is
              -- pending again, a deposit whose deposit tx was erased is gone,
              -- and observing the new chain brings the view back in line. See
              -- #2741. Retained settlements observed after the rolled back slot
              -- are no longer on chain: mark them, so the ticks re-post them in
              -- order, see 'repostErased'.
              rollbackDeposits (chainStateSlot chainState) $
                nodeState{headState = onCoordinatedHeadState (markErased (chainStateSlot chainState)) st, chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}}
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

-- | Fold a finalized increment or decrement at 'newVersion' into an open head.
-- Observed again after a rollback erased it ('isReobservation'), only the
-- settlement bookkeeping changes. Otherwise the head moves to 'newVersion' and
-- a snapshot not being signed is reset ('seenSnapshotAfterVersionBump'). What
-- else each kind of settlement clears comes from the caller.
settlementFinalized ::
  IsChainState tx =>
  ChainStateType tx ->
  SnapshotVersion ->
  -- | Bookkeeping on a re-observation
  (CoordinatedHeadState tx -> CoordinatedHeadState tx) ->
  -- | Bookkeeping on the version bump
  (CoordinatedHeadState tx -> CoordinatedHeadState tx) ->
  OpenState tx ->
  OpenState tx
settlementFinalized chainState newVersion onReobservation onBump os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}}
  | isReobservation newVersion chs =
      os{chainState, coordinatedHeadState = onReobservation recorded}
  | otherwise =
      os
        { chainState
        , coordinatedHeadState =
            onBump
              recorded
                { version = newVersion
                , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                }
        }
 where
  recorded = recordSettlement (chainStateSlot chainState) newVersion chs

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
              , settlements = mempty
              , unretained = mempty
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
            -- recognizable by 'version' already being one past the snapshot's.
            -- Nothing could be retained at observation time (see
            -- 'retainSettlement'), so retain now, with the status the bump was
            -- noted with ('UnretainedSettlements'). A rollback in between marked
            -- that note erased. Stamping the current chain slot instead would
            -- call the erased settlement landed, since the chain state was
            -- rewound too, and it would never be re-posted.
            let confirmed = ConfirmedSnapshot{snapshot, signatures}
                status = fromMaybe (Landed (chainStateSlot chainState)) (Map.lookup snapshot.version chs.unretained)
             in Open
                  os
                    { coordinatedHeadState =
                        chs
                          { confirmedSnapshot = confirmed
                          , seenSnapshot = LastSeenSnapshot snapshot.number
                          , settlements = retainSettlementWith status version confirmed chs.settlements
                          , -- Snapshots confirm in order, so every note at or below
                            -- this version is used up or dead. Dropping them keeps
                            -- the notes bounded.
                            unretained = Map.filterWithKey (\k _ -> k > snapshot.version) chs.unretained
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
                  ConfirmedSnapshot{snapshot} ->
                    coordinatedHeadState
                      { localUTxO = settledUTxO currentVersion snapshot
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
            Nothing
              -- The finalized deposit can be re-activated when a rollback rewinds
              -- the deposit view to before its activation: never park it in
              -- 'currentDepositTxId' — it is settled by re-posting the increment,
              -- not by a new snapshot, see 'retainedDeposits' and #2741.
              | depositTxId `Set.member` retainedDeposits chs.settlements -> st
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
      Open os ->
        Open $
          settlementFinalized
            chainState
            newVersion
            -- Only convergence bookkeeping: in particular an unrelated newer
            -- decommit may already be in flight and must be left alone. See #2741.
            id
            (\chs -> chs{decommitTx = Nothing})
            os
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
              }
      _otherState -> st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version} ->
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
        closedToFanoutProgress cst chainState remainingOutputs mempty AutoDrain []
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
        closedToFanoutProgress cst chainState remainingOutputs mempty (recordedSelectionMode mempty remainingOutputs selection) []
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
      -- The observer had no mode to replace: waiting is what its erased step
      -- rewinds to.
      Closed cst ->
        closedToFanoutProgress cst chainState remainingOutputs newlyDistributed mode [landed AwaitingSelection]
      -- Subsequent steps: accumulate distributed outputs, update remaining/mode
      -- and record the step with the mode it replaces.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs = priorDistributed, mode = priorMode, stepsLanded} ->
        FanoutProgress
          pfs
            { chainState
            , remainingOutputs
            , distributedOutputs = priorDistributed <> newlyDistributed
            , mode
            , stepsLanded = stepsLanded <> [landed priorMode]
            , everLanded = True
            }
      _otherState -> st
   where
    landed modeBefore = FanoutStepLanded{landedAt = chainStateSlot chainState, stepOutputs = newlyDistributed, modeBefore}
  HeadIsReadyToFanout{} ->
    case st of
      Closed cst -> Closed cst{readyToFanoutSent = True}
      _otherState -> st
  ChainRolledBack{chainState} ->
    case st of
      -- The fanout's progress is chain-derived: rewind it to the steps still
      -- on the chain this node follows, see 'rewindFanoutProgress'.
      FanoutProgress pfs -> FanoutProgress (rewindFanoutProgress (chainStateSlot chainState) pfs){chainState}
      _otherState -> setChainState chainState st
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
  -- | Rollback horizon, see 'aggregateNodeState'
  ChainSlot ->
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState rollbackHorizon s outcome =
  foldl' (aggregateNodeState rollbackHorizon) s $ collectStateChanged outcome
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
