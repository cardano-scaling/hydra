{-# LANGUAGE OverloadedRecordDot #-}

-- | Own the tracked deposit map. Entry updates retain chain observation slots; the
-- output carries a consumed deposit's value to the open head for settlement.
module Hydra.HeadLogic.State.Deposits (machine) where

import Crem.BaseMachine (
  BaseMachineT (..),
  InitialState (..),
  pureResult,
 )
import Crem.StateInspection (describeOnly)
import Crem.Topology (TrivialTopology)
import Data.Map.Strict qualified as Map (alter, lookup, mapMaybe)
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Deposit qualified as Deposit (
  consume,
  record,
  rollback,
  update,
 )
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State.Types (DepositOutput (..))
import Hydra.Node.State (
  Deposit (..),
  DepositStatus (Inactive),
  TrackedDeposit (..),
  TrackedDeposits,
  pruneConsumedDeposits,
 )
import Hydra.Prelude

data Registry tx phase where
  Registry :: TrackedDeposits tx -> Registry tx '()

machine :: (IsChainState tx, Applicative m) => TrackedDeposits tx -> BaseMachineT m (TrivialTopology @()) (StateChanged tx) (DepositOutput tx)
machine initial = BaseMachineT (InspectableState (describeOnly $ const "TrackedDeposits") $ Registry initial) $ \(Registry deposits) event ->
  let settledDeposit = case event of
        CommitFinalized{depositTxId} -> maybe mempty (.deposited) $ do
          tracked <- Map.lookup depositTxId deposits
          guard $ isNothing tracked.consumedAt
          pure tracked.deposit
        _ -> mempty
      updated = case event of
        DepositRecorded{chainState, headId, depositTxId, deposited, created, deadline} ->
          let slot = chainStateSlot chainState
           in Map.alter (Deposit.record slot Deposit{headId, deposited, created, deadline, status = Inactive}) depositTxId $ pruneConsumedDeposits slot deposits
        DepositActivated{depositTxId, deposit} -> Map.alter (Deposit.update deposit) depositTxId deposits
        DepositExpired{depositTxId, deposit} -> Map.alter (Deposit.update deposit) depositTxId deposits
        DepositRecovered{chainState, depositTxId} -> consume (chainStateSlot chainState) depositTxId
        CommitFinalized{chainState, depositTxId} -> consume (chainStateSlot chainState) depositTxId
        ChainRolledBack{chainState} -> Map.mapMaybe (Deposit.rollback (chainStateSlot chainState) . Just) deposits
        _ -> deposits
      consume slot depositId = Map.alter (Deposit.consume slot) depositId $ pruneConsumedDeposits slot deposits
   in pureResult DepositOutput{event, deposits = updated, settledDeposit} (Registry updated)
