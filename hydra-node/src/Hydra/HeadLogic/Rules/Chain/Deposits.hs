{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Deposits where

import Data.Map.Strict qualified as Map (
  difference,
  filter,
  member,
  toAscList,
 )
import Hydra.Chain (
  ChainEvent (Observation, newChainState, observedTx),
  OnChainTx (
    OnDepositTx,
    OnRecoverTx,
    created,
    deadline,
    depositTxId,
    deposited,
    headId,
    recoveredTxId,
    recoveredUTxO
  ),
  PostChainTx (
    IncrementTx,
    depositTxId,
    headId,
    headParameters,
    headSeed,
    incrementingSnapshot
  ),
 )
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (LogicError (..))
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, emit, reject)
import Hydra.HeadLogic.Rules.Coordinated (sendSnapshot)
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (
    RuleEnvironment,
    depositActivation,
    depositPeriod,
    party,
    snapshotLimits
  ),
 )
import Hydra.HeadLogic.Snapshot.Request (
  DecommitStatus (ReadyToDecommit),
  SettlingCommit (ContinueCommit, DropCommit),
  buildRequest,
  contextDecommit,
  contextSettlingCommit,
  currentRequestContext,
  isLeader,
  requestDepositTxId,
 )
import Hydra.HeadLogic.Snapshot.Settlement (finalizedDepositTxId)
import Hydra.HeadLogic.State (
  ClosedState (ClosedState, headId),
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    decommitTx,
    finalizedCommit,
    seenSnapshot
  ),
  FinalizedSnapshot (FinalizedSnapshot, snapshot),
  HeadState (..),
  OpenState (
    OpenState,
    coordinatedHeadState,
    headId,
    headSeed,
    parameters
  ),
  PartialFanoutState (PartialFanoutState, headId),
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  start,
 )
import Hydra.Node.State (
  Deposit (Deposit, created, deadline, deposited, status),
  DepositStatus (Active, Expired, Inactive),
  PendingDeposits,
 )
import Hydra.Prelude
import Hydra.Tx (IsTx)
import Hydra.Tx.DepositPeriod (
  toNominalDiffTime,
 )
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

determineNextDepositStatus :: RuleEnvironment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  (\deposit -> deposit{status = determineStatus deposit}) <$> pendingDeposits
 where
  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositActivation = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  RuleEnvironment{depositPeriod, depositActivation} = env

-- | Either activate or expire deposits that meet those criteria.
updateDepositStates ::
  IsTx tx =>
  RuleEnvironment ->
  PendingDeposits tx ->
  UTCTime ->
  Program tx (PendingDeposits tx)
updateDepositStates env pendingDeposits chainTime =
  mkDepositActivated newActive *> mkDepositExpired newExpired $> (nextActive <> nextExpired <> pendingDeposits)
 where
  newActive = Map.difference nextActive pendingActive

  newExpired = Map.difference nextExpired pendingExpired

  pendingActive = Map.filter (\Deposit{status} -> status == Active) pendingDeposits

  pendingExpired = Map.filter (\Deposit{status} -> status == Expired) pendingDeposits

  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  nextActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  nextExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  mkDepositActivated m = forM_ (Map.toAscList m) $ \(depositTxId, deposit) ->
    change DepositActivated{depositTxId, chainTime, deposit}

  mkDepositExpired m = forM_ (Map.toAscList m) $ \(depositTxId, deposit) ->
    change DepositExpired{depositTxId, chainTime, deposit}

-- | Track deposits and either drop them or request snapshots for inclusion.
maybeRequestDepositSnapshot ::
  forall tx.
  IsChainState tx =>
  RuleEnvironment ->
  UTCTime ->
  PendingDeposits tx ->
  OpenState tx ->
  Program tx ()
maybeRequestDepositSnapshot env chainTime pendingDeposits st
  | snapshotInFlight seenSnapshot || not (isLeader parameters party nextSn) = pure ()
  | otherwise =
      let settling = contextSettlingCommit context
          built = fst <$> buildRequest env.snapshotLimits context
       in case (settling, built) of
            -- The commit is still settling; requests carrying txs continue it.
            (Just (ContinueCommit _), _) -> pure ()
            -- The commit can no longer land: drop it so other deposits can be claimed.
            (Just DropCommit, Just request) -> sendSnapshot (Coordinated.start st) request
            -- Spec: wait txω = ⊥ ∧ Uα = ∅.
            (Nothing, Just request)
              | isJust (requestDepositTxId request), isNothing decommitTx -> sendSnapshot (Coordinated.start st) request
            (Nothing, Just request)
              | ReadyToDecommit{} <- decommit -> sendSnapshot (Coordinated.start st) request
            _ -> pure ()
 where
  context = currentRequestContext env.depositPeriod chainTime deposits coordinatedHeadState
  decommit = contextDecommit context

  deposits = Map.filter (\Deposit{deposited} -> deposited /= mempty) pendingDeposits

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  nextSn = confirmedSn + 1

  RuleEnvironment{party} = env

  CoordinatedHeadState{..} = coordinatedHeadState

  OpenState{coordinatedHeadState, parameters} = st

-- | Record deposit and recovery observations independently of snapshot selection.
handleDepositObservation :: IsChainState tx => PendingDeposits tx -> HeadState tx -> Input tx -> Program tx ()
handleDepositObservation pendingDeposits st ev = case (st, ev) of
  -- Node-level: deposit/recover observations scoped to our head
  ( Open OpenState{headId = ourHeadId, headSeed, parameters, coordinatedHeadState = CoordinatedHeadState{finalizedCommit}}
    , ChainInput Observation{observedTx = OnDepositTx{..}, newChainState}
    )
      | ourHeadId == headId ->
          change DepositRecorded{chainState = newChainState, ..}
            -- The finalized deposit observed *again* means a rollback erased
            -- both the deposit and its finalized increment, and the deposit tx
            -- just re-landed on the new chain. The increment re-post issued at
            -- rollback time necessarily failed (the deposit UTxO did not exist
            -- on the new chain yet) and is not retried anywhere else, while
            -- every alternative settlement of this deposit is deliberately
            -- blocked (see 'finalizedDepositTxId'): re-post the increment
            -- now that the deposit is on chain again, see #2741.
            *> case finalizedCommit of
              Just FinalizedSnapshot{snapshot}
                | finalizedDepositTxId finalizedCommit == Just depositTxId ->
                    emit
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
  (Closed ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{..}, newChainState})
    | ourHeadId == headId ->
        change DepositRecorded{chainState = newChainState, ..}
    | otherwise ->
        pure ()
  -- Mirror the 'Closed' case while mid-fanout: a deposit observed during a
  -- (partial) fanout must still be recorded so it remains recoverable via
  -- 'Recover'. Without this the input falls through to 'Error' and is dropped.
  (FanoutProgress PartialFanoutState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDepositTx{..}, newChainState})
    | ourHeadId == headId ->
        change DepositRecorded{chainState = newChainState, ..}
    | otherwise ->
        pure ()
  (Idle _, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    pure ()
  (Finalized{}, ChainInput Observation{observedTx = OnDepositTx{}}) ->
    pure ()
  -- Deposit recovery is node-level: emit DepositRecovered for any tracked deposit
  -- regardless of which head is currently active. Previous-head deposits survive
  -- fanout in 'pendingDeposits', so recovery works even while a new head is Open.
  -- Unrelated deposits (never in pendingDeposits) are silently ignored.
  (_, ChainInput Observation{observedTx = OnRecoverTx{headId, recoveredTxId, recoveredUTxO}, newChainState})
    | Map.member recoveredTxId pendingDeposits ->
        change DepositRecovered{chainState = newChainState, headId, depositTxId = recoveredTxId, recovered = recoveredUTxO}
    | otherwise ->
        pure ()
  _ -> reject $ UnhandledInput ev st
