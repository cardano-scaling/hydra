{-# LANGUAGE OverloadedRecordDot #-}

-- | Own the tracked deposit map. Each entry follows the deposit machine; the
-- output carries a consumed deposit's value to the open head for settlement.
module Hydra.HeadLogic.Machine.DepositRegistry (machine) where

import Hydra.Prelude

import Crem.BaseMachine (BaseMachineT (..), InitialState (..), pureResult)
import Crem.StateInspection (describeOnly)
import Crem.Topology (TrivialTopology)
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Machine.Deposit qualified as Deposit
import Hydra.HeadLogic.Machine.Types (DepositOutput (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.Node.State (Deposit (..), DepositStatus (Inactive), TrackedDeposit (..), TrackedDeposits, pruneConsumedDeposits)

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
           in Map.alter (Deposit.transition $ Deposit.Observe slot Deposit{headId, deposited, created, deadline, status = Inactive}) depositTxId $ pruneConsumedDeposits slot deposits
        DepositActivated{depositTxId, deposit} -> Map.alter (Deposit.transition $ Deposit.Update deposit) depositTxId deposits
        DepositExpired{depositTxId, deposit} -> Map.alter (Deposit.transition $ Deposit.Update deposit) depositTxId deposits
        DepositRecovered{chainState, depositTxId} -> consume (chainStateSlot chainState) depositTxId
        CommitFinalized{chainState, depositTxId} -> consume (chainStateSlot chainState) depositTxId
        ChainRolledBack{chainState} -> Map.mapMaybe (Deposit.transition (Deposit.Rollback $ chainStateSlot chainState) . Just) deposits
        _ -> deposits
      consume slot depositId = Map.alter (Deposit.transition $ Deposit.Consume slot) depositId $ pruneConsumedDeposits slot deposits
   in pureResult DepositOutput{event, deposits = updated, settledDeposit} (Registry updated)
