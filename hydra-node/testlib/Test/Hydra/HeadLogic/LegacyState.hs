{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen event reducers for the crem migration. Pure UTxO, fanout and
-- settlement helpers remain shared; the event dispatch and updates are fixed.
module Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState, legacyApplyEvent) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState, chainPointSlot, chainStateSlot)
import Hydra.HeadLogic
import Hydra.Node.Deposit qualified as Deposit
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), TrackedDeposit (..), TrackedDeposits, depositRetentionHorizon)
import Hydra.Prelude
import Hydra.Tx (IsTx (..), txId, utxoFromTx, withoutUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))

legacyAggregateNodeState :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
legacyAggregateNodeState nodeState sc =
  case (headIdOf (headState nodeState), eventHeadId sc) of
    (Just sid, Just eid) | sid /= eid -> nodeState
    _ ->
      let st = legacyApplyEvent (headState nodeState) sc
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

legacyApplyEvent :: IsChainState tx => HeadState tx -> StateChanged tx -> HeadState tx
legacyApplyEvent st = \case
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

recordDeposit :: IsTx tx => ChainSlot -> TxIdType tx -> Deposit tx -> NodeState tx -> NodeState tx
recordDeposit slot depositTxId deposit nodeState =
  nodeState
    { deposits =
        Map.insert depositTxId TrackedDeposit{deposit, recordedAt = slot, consumedAt = Nothing} $
          pruneConsumedDeposits slot (deposits nodeState)
    }

-- | Update a tracked deposit (e.g. on status changes); its lifecycle slots are
-- unaffected. Unknown ids are ignored.
updateDeposit :: forall tx. IsTx tx => TxIdType tx -> Deposit tx -> NodeState tx -> NodeState tx
updateDeposit depositTxId deposit nodeState =
  nodeState{deposits = Map.adjust (\(tracked :: TrackedDeposit tx) -> tracked{Deposit.deposit = deposit}) depositTxId (deposits nodeState)}

-- | Mark a deposit consumed at the given slot: its increment or recover was
-- observed on chain. Re-consuming (the consuming transaction re-landed after a
-- rollback) re-stamps the slot, so a rollback of the re-landed transaction
-- still resurfaces the deposit.
consumeDeposit :: IsTx tx => ChainSlot -> TxIdType tx -> NodeState tx -> NodeState tx
consumeDeposit slot depositTxId nodeState =
  nodeState
    { deposits =
        Map.adjust (\tracked -> tracked{consumedAt = Just slot}) depositTxId $
          pruneConsumedDeposits slot (deposits nodeState)
    }

-- | Rewind the deposit view to the given (rolled back) slot, see
-- 'TrackedDeposit'.
rollbackDeposits :: ChainSlot -> NodeState tx -> NodeState tx
rollbackDeposits slot nodeState =
  nodeState{deposits = Map.mapMaybe rollbackOne (deposits nodeState)}
 where
  rollbackOne tracked@TrackedDeposit{recordedAt, consumedAt}
    | recordedAt > slot = Nothing
    | otherwise = Just tracked{consumedAt = mfilter (<= slot) consumedAt}

-- | Drop consumed deposits beyond 'depositRetentionHorizon': no rollback can
-- resurface them anymore, so retaining them would only grow persisted state
-- with every deposit ever settled. Called on the deposit write paths, which is
-- enough because only deposit churn creates consumed entries. Unconsumed
-- deposits are never pruned — an expired deposit stays recoverable
-- indefinitely.
pruneConsumedDeposits :: ChainSlot -> TrackedDeposits tx -> TrackedDeposits tx
pruneConsumedDeposits (ChainSlot slot) =
  Map.filter (\TrackedDeposit{consumedAt} -> maybe True (> cutoff) consumedAt)
 where
  cutoff =
    case depositRetentionHorizon of
      ChainSlot horizon -> ChainSlot (if slot > horizon then slot - horizon else 0)
