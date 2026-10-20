{-# LANGUAGE DuplicateRecordFields #-}
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
import Hydra.Chain.ChainState (
  ChainSlot (..),
  IsChainState (..),
  chainStateSlot,
 )
import Hydra.HeadLogic.Close (
  closedToFanoutProgress,
  fanoutProgressToClosed,
  recordedSelectionMode,
  rewindFanoutProgress,
  sameOutputs,
 )
import Hydra.HeadLogic.Outcome (
  Outcome (..),
  StateChanged (..),
 )
import Hydra.HeadLogic.Settlement (
  isReobservation,
  markErased,
  onCoordinatedHeadState,
  pruneSettlements,
  recordSettlement,
  retainSettlementWith,
  retainedDeposits,
 )
import Hydra.HeadLogic.Snapshot.Request (
  absorbable,
  maxTxsPerSnapshot,
  nextRequestUTxO,
  seenSnapshotAfterVersionBump,
  settledUTxO,
 )
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FanoutStepLanded (..),
  HeadState (..),
  IdleState (..),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  SettlementConflict,
  SettlementStatus (..),
  emptySettlements,
  getChainState,
  mkSeenSnapshot,
  seenSnapshotNumber,
  selectedOutputsInStep,
  setChainState,
  unretainedAfter,
  unretainedEntries,
 )
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  DepositStatus (..),
  NodeState (..),
  consumeDeposit,
  pruneConsumedDeposits,
  recordDeposit,
  rollbackDeposits,
  updateDeposit,
 )
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
 )

-- * NodeState aggregate

-- | Reflect 'StateChanged' events onto the 'NodeState' aggregateNodeState.
-- Events carrying a 'HeadId' that does not match the current state are silently
-- ignored, preventing cross-head state contamination during event replay.
-- Events without a 'HeadId' are always applied.
aggregateNodeState ::
  IsChainState tx =>
  -- | Legacy environment value, see 'Hydra.Node.Environment.rollbackHorizon'
  ChainSlot ->
  NodeState tx ->
  StateChanged tx ->
  Either (SettlementConflict tx) (NodeState tx)
aggregateNodeState rollbackHorizon nodeState sc =
  case (headIdOf (headState nodeState), eventHeadId sc) of
    (Just sid, Just eid) | sid /= eid -> pure nodeState
    _ -> do
      st <- applyEvent (headState nodeState) sc
      let chainPointTimeState = chainPointTime nodeState
      case sc of
        HeadOpened{chainState} ->
          pure $
            nodeState
              { headState = st
              , chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}
              }
        DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
          pure $
            recordDeposit rollbackHorizon (chainStateSlot chainState) depositTxId Deposit{headId, deposited, created, deadline, status = Inactive} $
              nodeState{headState = st}
        DepositActivated{depositTxId, deposit} ->
          pure $
            updateDeposit depositTxId deposit $
              nodeState{headState = st}
        DepositExpired{depositTxId, deposit} ->
          pure $
            -- NB: We keep expired deposits since we actually need them when Recovering.
            -- There is a corresponding error RequestedDepositExpired which gives users context on stale ReqSn.
            updateDeposit depositTxId deposit $
              nodeState{headState = st}
        DepositRecovered{chainState, depositTxId} ->
          pure $
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
          fmap (consumeDeposit rollbackHorizon (chainStateSlot chainState) depositTxId) $ case st of
            Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{localUTxO, confirmedSnapshot, seenSnapshot}}
              -- Only convergence bookkeeping: in particular 'localUTxO'
              -- must not absorb the deposit again (its outputs may have
              -- been spent on L2 in the meantime and the union would
              -- resurrect them) and an unrelated deposit already parked in
              -- 'currentDepositTxId' for the next snapshot must be left
              -- alone. See #2741.
              | isReobservation newVersion chs -> do
                  recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesIncrement chs
                  pure $
                    nodeState
                      { headState =
                          Open
                            os
                              { chainState
                              , coordinatedHeadState =
                                  recorded
                                    { currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId
                                    }
                              }
                      }
              | otherwise -> do
                  recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesIncrement chs
                  pure $
                    nodeState
                      { headState =
                          Open
                            os
                              { chainState
                              , coordinatedHeadState =
                                  recorded
                                    { version = newVersion
                                    , -- NOTE: This must correspond to the just finalized
                                      -- depositTxId, but we should not verify this here.
                                      currentDepositTxId = Nothing
                                    , localUTxO = localUTxO <> maybe mempty (.deposited) (Map.lookup depositTxId nodeState.pendingDeposits)
                                    , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                                    }
                              }
                      }
            _ ->
              pure $ nodeState{headState = st}
         where
          matchesIncrement snapshot = snapshot.depositTxId == Just depositTxId && isJust snapshot.utxoToCommit
        TickObserved{chainPoint, chainTime} ->
          pure $
            nodeState{headState = st, chainPointTime = chainPointTimeState{currentSlot = chainPointSlot chainPoint, currentChainTime = chainTime}}
        ChainFinalized{finalizedSlot} ->
          pure nodeState{headState = onCoordinatedHeadState (pruneSettlements finalizedSlot) st, deposits = pruneConsumedDeposits finalizedSlot (deposits nodeState)}
        ChainRolledBack{chainState} ->
          pure $
            -- Deposits are L1-derived: restore the view at the rolled-back
            -- slot. Deposits whose consuming tx (increment/recover) was
            -- erased resurface, deposits whose deposit tx was erased vanish;
            -- forward re-observation converges the view again. See #2741.
            -- Retained settlements observed after the rolled back slot are
            -- no longer on chain: mark them so they get re-posted in order,
            -- see 'repostErased'.
            rollbackDeposits (chainStateSlot chainState) $
              nodeState{headState = onCoordinatedHeadState (markErased (chainStateSlot chainState)) st, chainPointTime = chainPointTimeState{currentSlot = chainStateSlot chainState}}
        NodeUnsynced{chainSlot, chainTime, drift} ->
          pure $
            NodeCatchingUp{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
        NodeSynced{chainSlot, chainTime, drift} ->
          pure $
            NodeInSync{headState = st, deposits = deposits nodeState, chainPointTime = ChainPointTime chainSlot chainTime drift}
        -- Restore the full snapshot: a checkpoint carries the aggregated
        -- 'pendingDeposits' and 'chainPointTime' (and synced constructor),
        -- which the default arm below would otherwise drop on replay.
        Checkpoint checkpointedNodeState ->
          pure checkpointedNodeState
        _ ->
          pure $
            nodeState{headState = st}

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
  ChainFinalized{} -> Nothing
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

applyEvent :: IsChainState tx => HeadState tx -> StateChanged tx -> Either (SettlementConflict tx) (HeadState tx)
applyEvent st = \case
  NetworkConnected -> pure st
  NetworkDisconnected -> pure st
  NetworkVersionMismatch{} -> pure st
  NetworkClusterIDMismatch{} -> pure st
  NetworkBroadcastStalled{} -> pure st
  NetworkBroadcastResumed -> pure st
  PeerConnected{} -> pure st
  PeerDisconnected{} -> pure st
  HeadOpened{headSeed, headId, parameters, chainState} ->
    pure $
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
                , settlements = emptySettlements
                , unretained = mempty
                }
          , chainState
          }
  TransactionReceived{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState
                    { allTxs = Map.insert (txId tx) tx allTxs
                    }
              }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> pure st
  TransactionAppliedToLocalUTxO{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
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
      _otherState -> pure st
  SnapshotRequestDecided{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
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
      _otherState -> pure st
  SnapshotRequested{requestedSnapshot = snapshot, newLocalTxs, newCurrentDepositTxId} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        pure $
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState
                    { seenSnapshot = mkSeenSnapshot snapshot mempty
                    , localTxs = newLocalTxs
                    , localUTxO =
                        case (decommitTx, snapshot.utxoToDecommit) of
                          -- A commit can defer the recorded decommit to a later
                          -- snapshot. Reserve its inputs after reapplying local
                          -- transactions, which may produce those inputs.
                          (Just tx, Nothing) -> applyTxTo tx rebuiltUTxO `withoutUTxO` utxoFromTx tx
                          _ -> rebuiltUTxO
                    , allTxs = foldr (Map.delete . txId) allTxs snapshot.confirmed
                    , currentDepositTxId = newCurrentDepositTxId
                    }
              }
       where
        -- 'pruneTransactions' validates these transactions before the event.
        rebuiltUTxO = foldl' (flip applyTxTo) (settledUTxO version snapshot) newLocalTxs
        CoordinatedHeadState{allTxs, version, decommitTx} = coordinatedHeadState
      _otherState -> pure st
  PartySignedSnapshot{party, signature} ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState =
            chs@CoordinatedHeadState
              { seenSnapshot = ss@SeenSnapshot{signatories}
              }
          } ->
          pure $
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
      _otherState -> pure st
  SnapshotConfirmed{snapshot = mSnapshot, signatures} ->
    case st of
      Open os@OpenState{chainState, coordinatedHeadState = chs@CoordinatedHeadState{seenSnapshot, version}} ->
        case mSnapshot <|> snapshotFromSeen seenSnapshot of
          Just snapshot -> do
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
                status = fromMaybe (Landed (chainStateSlot chainState)) (Map.lookup snapshot.version chs.unretained.unretainedEntries)
            settlements <-
              retainSettlementWith
                status
                version
                confirmed
                chs.settlements
            pure $
              Open
                os
                  { coordinatedHeadState =
                      chs
                        { confirmedSnapshot = confirmed
                        , seenSnapshot = LastSeenSnapshot snapshot.number
                        , settlements
                        , -- Confirmation consumes its note before dropping older notes.
                          unretained = unretainedAfter snapshot.version chs.unretained
                        }
                  }
          -- Nothing to confirm: leave the state as it is rather than crashing
          -- the node, which would also happen on every replay of the event.
          Nothing -> pure st
      -- Only emitted, with the snapshot, for the 'unconfirmedSnapshot' adopted
      -- from the multisignature of an observed contest. Anything else leaves
      -- the closed head as it is, as it did before this case existed.
      Closed cst ->
        case mSnapshot of
          Just snapshot ->
            pure $
              Closed
                cst
                  { confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures}
                  , unconfirmedSnapshot = Nothing
                  }
          Nothing -> pure st
      _otherState -> pure st
   where
    snapshotFromSeen :: SeenSnapshot tx -> Maybe (Snapshot tx)
    snapshotFromSeen (SeenSnapshot sn _ _) = Just sn
    snapshotFromSeen _ = Nothing
  LocalStateCleared{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState = coordinatedHeadState@CoordinatedHeadState{confirmedSnapshot, version = currentVersion}} ->
        pure $
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
      _otherState -> pure st
  DepositRecorded{} -> pure st
  DepositActivated{depositTxId, deposit} -> case st of
    Open os@OpenState{headId = ourHeadId, coordinatedHeadState = chs}
      | deposit.headId == ourHeadId ->
          -- Spec: txω = ⊥ ∨ txα = ⊥ — deposit and decommit are mutually exclusive.
          -- Only queue the deposit when no decommit is pending; otherwise the tick
          -- will pick it up once the decommit completes.
          case chs.decommitTx of
            Just _ -> pure st
            Nothing
              -- The finalized deposit can be re-activated when a rollback rewinds
              -- the deposit view to before its activation: never park it in
              -- 'currentDepositTxId' — it is settled by re-posting the increment,
              -- not by a new snapshot, see 'retainedDeposits' and #2741.
              | depositTxId `Set.member` retainedDeposits chs.settlements -> pure st
              -- Also do not park a deposit the next snapshot request cannot
              -- include, see 'absorbable'.
              | not (absorbable (nextRequestUTxO chs) (toList (Seq.take maxTxsPerSnapshot chs.localTxs)) deposit.deposited) -> pure st
              | otherwise -> pure $ Open os{coordinatedHeadState = chs{currentDepositTxId = chs.currentDepositTxId <|> Just depositTxId}}
    _ -> pure st
  DepositExpired{} -> pure st
  CommitApproved{} -> pure st
  DepositRecovered{} -> pure st
  CommitFinalized{} -> pure st
  DecommitRecorded{decommitTx} -> case st of
    Open
      os@OpenState{coordinatedHeadState} ->
        pure $
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
    _otherState -> pure st
  DecommitApproved{} -> pure st
  DecommitInvalid{} -> pure st
  DecommitFinalized{chainState, newVersion, distributedUTxO} ->
    case st of
      Open os@OpenState{coordinatedHeadState = chs@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}}
        -- Only convergence bookkeeping: in particular an unrelated newer
        -- decommit may already be in flight and must be left alone. See #2741.
        | isReobservation newVersion chs -> do
            recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesDecrement chs
            pure $
              Open
                os
                  { chainState
                  , coordinatedHeadState =
                      recorded
                  }
        | otherwise -> do
            recorded <- recordSettlement (chainStateSlot chainState) newVersion matchesDecrement chs
            pure $
              Open
                os
                  { chainState
                  , coordinatedHeadState =
                      recorded
                        { decommitTx = Nothing
                        , version = newVersion
                        , seenSnapshot = seenSnapshotAfterVersionBump confirmedSnapshot seenSnapshot
                        }
                  }
      _otherState -> pure st
   where
    -- The chain observer relabels outputs, so compare their content and counts.
    matchesDecrement snapshot = maybe False (`sameOutputs` distributedUTxO) snapshot.utxoToDecommit
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
          pure $
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
      _otherState -> pure st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version, unconfirmedSnapshot} ->
        pure $
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
      _otherState -> pure st
  HeadFannedOut{chainState} ->
    case st of
      Closed _ ->
        pure $ Idle $ IdleState{chainState}
      FanoutProgress _ ->
        pure $ Idle $ IdleState{chainState}
      _otherState -> pure st
  HeadFanoutInitiated{remainingOutputs} ->
    case st of
      -- This node initiated a full automatic fanout: become the driver in
      -- 'AutoDrain' mode so its observations auto-continue to completion.
      Closed cst@ClosedState{chainState} ->
        pure $ closedToFanoutProgress cst chainState remainingOutputs mempty AutoDrain []
      -- A target covering the whole remainder before anything landed is a full
      -- fanout too ('nextFanoutStep'), so the driver switches to draining
      -- automatically.
      FanoutProgress pfs -> pure $ FanoutProgress pfs{mode = AutoDrain, remainingOutputs}
      _otherState -> pure st
  HeadPartialFanoutSelected{remainingOutputs, selection} ->
    case st of
      -- First selective partial fanout from a freshly closed head: enter the
      -- 'PartialFanout' state with nothing distributed yet.
      Closed cst@ClosedState{chainState} ->
        pure $ closedToFanoutProgress cst chainState remainingOutputs mempty (recordedSelectionMode mempty remainingOutputs selection) []
      -- Continuing: just record the new active selection.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs} ->
        pure $ FanoutProgress pfs{mode = recordedSelectionMode distributedOutputs remainingOutputs selection}
      _otherState -> pure st
  HeadFanoutReverted{} ->
    case st of
      -- Roll the optimistic transition back: the initiating fanout tx failed to
      -- post and nothing landed on chain, so the head is really still 'Closed'.
      FanoutProgress pfs -> pure $ Closed (fanoutProgressToClosed pfs)
      _otherState -> pure st
  HeadPartialFannedOut{distributedOutputs = newlyDistributed, remainingOutputs, chainState, mode} ->
    case st of
      -- First partial fanout observed by a passive observer: transition from
      -- 'Closed' into 'PartialFanout' (using the observed chain state).
      -- The observer had no mode to replace: waiting is what its erased step
      -- rewinds to.
      Closed cst ->
        pure $ closedToFanoutProgress cst chainState remainingOutputs newlyDistributed mode [landed AwaitingSelection]
      -- Subsequent steps: accumulate distributed outputs, update remaining/mode
      -- and record the step with the mode it replaces.
      FanoutProgress pfs@PartialFanoutState{distributedOutputs = priorDistributed, mode = priorMode, stepsLanded} ->
        pure $
          FanoutProgress
            pfs
              { chainState
              , remainingOutputs
              , distributedOutputs = priorDistributed <> newlyDistributed
              , mode
              , stepsLanded = stepsLanded <> [landed priorMode]
              , everLanded = True
              }
      _otherState -> pure st
   where
    landed priorMode = FanoutStepLanded{landedAt = chainStateSlot chainState, stepOutputs = newlyDistributed, selectedOutputs = selectedOutputsInStep newlyDistributed priorMode}
  HeadIsReadyToFanout{} ->
    case st of
      Closed cst -> pure $ Closed cst{readyToFanoutSent = True}
      _otherState -> pure st
  ChainRolledBack{chainState} ->
    case st of
      -- The fanout's progress is chain-derived: rewind it to the steps still
      -- on the chain this node follows, see 'rewindFanoutProgress'.
      FanoutProgress pfs -> pure $ FanoutProgress (rewindFanoutProgress (chainStateSlot chainState) pfs){chainState}
      _otherState -> pure $ setChainState chainState st
  ChainFinalized{} -> pure st
  TickObserved{} -> pure st
  IgnoredHeadInitializing{} -> pure st
  TxInvalid{transaction} -> case st of
    Open ost@OpenState{coordinatedHeadState = coordState@CoordinatedHeadState{allTxs = allTransactions}} ->
      pure $ Open ost{coordinatedHeadState = coordState{allTxs = Map.delete (txId transaction) allTransactions}}
    _otherState -> pure st
  Checkpoint nodeState -> pure $ headState nodeState
  NodeSynced{} -> pure st
  NodeUnsynced{} -> pure st

aggregateState ::
  IsChainState tx =>
  -- | Rollback horizon, see 'aggregateNodeState'
  ChainSlot ->
  NodeState tx ->
  Outcome tx ->
  Either (SettlementConflict tx) (NodeState tx)
aggregateState rollbackHorizon s outcome =
  foldlM (aggregateNodeState rollbackHorizon) s $ collectStateChanged outcome
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
  ChainFinalized{} -> history
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
