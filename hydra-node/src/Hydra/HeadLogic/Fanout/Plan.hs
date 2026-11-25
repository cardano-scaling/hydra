{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Fanout.Plan (
  FanoutPlan,
  fullPlan,
  closedSelectionPlan,
  remainingSelectionPlan,
  planChange,
  planTransaction,
) where

import Hydra.Chain (PostChainTx)
import Hydra.HeadLogic.Fanout.Outputs (isSubMultisetOf, nullOutputs)
import Hydra.HeadLogic.Fanout.Selection (
  NextFanoutStep (..),
  computeFullFanoutUTxO,
  fanoutTransaction,
  nextFanoutStep,
 )
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (
  ClosedState (..),
  OnChainFanoutDatum (..),
  PartialFanoutState (..),
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, HeadSeed, IsTx, UTxOType)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, SnapshotVersion)

data FanoutPlan tx = FanoutPlan (StateChanged tx) ~(PostChainTx tx)

planChange :: FanoutPlan tx -> StateChanged tx
planChange (FanoutPlan decision _) = decision

planTransaction :: FanoutPlan tx -> PostChainTx tx
planTransaction (FanoutPlan _ transaction) = transaction

fullPlan :: IsTx tx => ClosedState tx -> FanoutPlan tx
fullPlan closed =
  plan
    closed.headId
    closed.confirmedSnapshot
    closed.version
    closed.headSeed
    closed.contestationDeadline
    (computeFullFanoutUTxO closed)
    FullFanoutStep

closedSelectionPlan ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  Maybe (FanoutPlan tx)
closedSelectionPlan closed selection = do
  let remaining = computeFullFanoutUTxO closed
  step <- select closed.confirmedSnapshot closed.version selection remaining DatumClosed
  pure $
    plan
      closed.headId
      closed.confirmedSnapshot
      closed.version
      closed.headSeed
      closed.contestationDeadline
      remaining
      step

remainingSelectionPlan ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  Maybe (FanoutPlan tx)
remainingSelectionPlan progress selection = do
  step <- select progress.confirmedSnapshot progress.version selection progress.remainingOutputs progress.fanoutDatum
  pure $
    plan
      progress.headId
      progress.confirmedSnapshot
      progress.version
      progress.headSeed
      progress.contestationDeadline
      progress.remainingOutputs
      step

select ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  UTxOType tx ->
  UTxOType tx ->
  OnChainFanoutDatum ->
  Maybe (NextFanoutStep tx)
select snapshot version selection remaining datum = do
  guard $ not (nullOutputs selection) && selection `isSubMultisetOf` remaining
  pure $ nextFanoutStep snapshot version selection remaining datum

plan ::
  IsTx tx =>
  HeadId ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  UTxOType tx ->
  NextFanoutStep tx ->
  FanoutPlan tx
plan headId snapshot version seed deadline remainingOutputs step =
  FanoutPlan decision (fanoutTransaction step snapshot version seed deadline)
 where
  decision = case step of
    FullFanoutStep -> HeadFanoutInitiated{headId, remainingOutputs}
    FinalStep{stepDistribute} -> HeadPartialFanoutSelected{headId, remainingOutputs, selection = stepDistribute}
    PartialStep{stepDistribute} -> HeadPartialFanoutSelected{headId, remainingOutputs, selection = stepDistribute}
