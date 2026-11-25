{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Fanout where

import Hydra.API.ServerOutput qualified as ServerOutput (
  ClientMessage (PostTxOnChainFailed, postChainTx, postTxError),
 )
import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Fanout.Outputs (
  nullOutputs,
  removeDistributedOutputs,
 )
import Hydra.HeadLogic.Fanout.Selection (
  computeFullFanoutUTxO,
  currentFanoutStep,
  matchesFanoutStep,
  nextFanoutStep,
 )
import Hydra.HeadLogic.Outcome (
  Effect (ClientEffect),
  StateChanged (
    HeadFannedOut,
    HeadFanoutReverted,
    HeadPartialFannedOut,
    chainState,
    distributedOutputs,
    finalizedOutputs,
    headId,
    mode,
    remainingOutputs
  ),
 )
import Hydra.HeadLogic.Program (Program, change, emit)
import Hydra.HeadLogic.Rules.Fanout (emitFanoutStep)
import Hydra.HeadLogic.State (
  ClosedState (ClosedState, headId),
  FanoutMode (AutoDrain, AwaitingSelection, DistributingSelection),
  OnChainFanoutDatum (..),
  PartialFanoutState (
    PartialFanoutState,
    confirmedSnapshot,
    contestationDeadline,
    distributedOutputs,
    fanoutDatum,
    headId,
    headSeed,
    mode,
    remainingOutputs,
    version
  ),
 )
import Hydra.Prelude
import Hydra.Tx (IsTx (UTxOType, outputsOfUTxO, withoutUTxO))

-- | Observe a (full or final) fanout transaction, finalizing the head.
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Program tx ()
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  change HeadFannedOut{headId, finalizedOutputs = fanoutUTxO, chainState = newChainState}
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout without a local request. The driver already stores
-- pending fanout bookkeeping after its 'Fanout'/'PartialFanout' command, so this
-- handler is only reached by passive observers. Both remain in the protocol's
-- Closed phase until this observation.
--
-- The observer transitions into 'PartialFanout' in 'AwaitingSelection' mode and
-- does __not__ auto-drive the rest: only the driver advances the fanout. This is
-- what makes selective partial fanout work in a multi-party head — observers must
-- not steamroll the remaining UTxO the driver deliberately left.
onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Program tx ()
onClosedChainPartialFanoutTx closedState newChainState observedDistributed =
  let fullUTxO = computeFullFanoutUTxO closedState
      remaining = removeDistributedOutputs (outputsOfUTxO observedDistributed) fullUTxO
      distributedUTxO = withoutUTxO fullUTxO remaining
   in change
        HeadPartialFannedOut
          { headId
          , distributedOutputs = distributedUTxO
          , remainingOutputs = remaining
          , chainState = newChainState
          , mode = AwaitingSelection
          }
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while pending or already distributing. Updates the remaining and
-- distributed sets and, depending on the current 'FanoutMode', either continues
-- draining automatically (or within the active selection) or waits for the next
-- 'PartialFanout' command.
onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  Program tx ()
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
        change
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
        -- Intermediate fanout cannot empty the on-chain accumulator. If the
        -- local view is exhausted, retain the observation without posting an
        -- invalid zero-output final batch.
        | nullOutputs remaining = pure ()
        | otherwise = case newMode of
            AutoDrain -> finalize
            DistributingSelection sel' ->
              emitStep (nextFanoutStep confirmedSnapshot version sel' remaining DatumFanoutProgress)
            AwaitingSelection -> pure ()
   in record *> continue
 where
  emitStep step = emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline

  PartialFanoutState{..} = pfs

-- | Observe the final fanout while in 'PartialFanout', finalizing the head with
-- the accumulated distributed outputs plus this final batch.
onPartialFanoutChainFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Program tx ()
onPartialFanoutChainFanoutTx pfs newChainState fanoutUTxO =
  change HeadFannedOut{headId, finalizedOutputs = distributedOutputs <> fanoutUTxO, chainState = newChainState}
 where
  PartialFanoutState{headId, distributedOutputs} = pfs

-- | Undo an optimistic fanout only when the failed transaction is the current
-- step and no output has landed. A stale failure must not discard a newer
-- selection or turn observed fanout progress back into a closed head.
onFanoutPostTxError :: IsTx tx => PartialFanoutState tx -> PostChainTx tx -> PostTxError tx -> Program tx ()
onFanoutPostTxError pfs postChainTx postTxError = do
  when (pfs.fanoutDatum == DatumClosed && maybe False (\step -> matchesFanoutStep pfs step postChainTx) (currentFanoutStep pfs)) $
    change HeadFanoutReverted{headId = pfs.headId}
  emit $ ClientEffect ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
