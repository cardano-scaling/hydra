{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Client.Fanout where

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput (
  ClientMessage (CommandFailed),
 )
import Hydra.HeadLogic.Fanout.Outputs (
  isSubMultisetOf,
  nullOutputs,
 )
import Hydra.HeadLogic.Fanout.Selection (
  NextFanoutStep (FullFanoutStep),
  computeFullFanoutUTxO,
  nextFanoutStep,
 )
import Hydra.HeadLogic.Outcome (Effect (ClientEffect))
import Hydra.HeadLogic.Program (Program, emit)
import Hydra.HeadLogic.Rules.Fanout (
  emitFanoutStep,
  fanoutStepStateChange,
 )
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    confirmedSnapshot,
    contestationDeadline,
    headId,
    headSeed,
    version
  ),
  HeadState (Closed, FanoutProgress),
  OnChainFanoutDatum (..),
  PartialFanoutState (
    PartialFanoutState,
    confirmedSnapshot,
    contestationDeadline,
    fanoutDatum,
    headId,
    headSeed,
    remainingOutputs,
    version
  ),
 )
import Hydra.Prelude
import Hydra.Tx (IsTx (UTxOType))

onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  Program tx ()
onClosedClientFanout closedState =
  -- A plain 'Fanout' is a full fanout by definition, and is only reachable from
  -- 'Closed'.
  fanoutStepStateChange headId FullFanoutStep fullUTxO
    *> emitFanoutStep FullFanoutStep confirmedSnapshot version headSeed contestationDeadline
 where
  fullUTxO = computeFullFanoutUTxO closedState

  ClosedState{..} = closedState

onClosedClientPartialFanout ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  Program tx ()
onClosedClientPartialFanout closedState selection
  | nullOutputs selection || not (selection `isSubMultisetOf` fullUTxO) =
      emit . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (Closed closedState)
  | otherwise =
      -- A selection covering the whole head becomes a full fanout - what the user
      -- means by "fan out everything", and the only option for a head holding a
      -- single UTxO, since a non-final batch has to leave one behind.
      fanoutStepStateChange headId step fullUTxO
        *> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- Fresh head: the on-chain datum is still @Closed@.
  step = nextFanoutStep confirmedSnapshot version selection fullUTxO DatumClosed

  fullUTxO = computeFullFanoutUTxO closedState
  ClosedState{..} = closedState

-- | Client request to continue a selective partial fanout. Validates the
-- selection against the current 'remainingOutputs' and emits whichever step
-- 'nextFanoutStep' decides on, which is not always a recorded selection: one
-- covering the whole remainder finalizes the head, or, if nothing has been
-- distributed yet, becomes a full fanout that drains it automatically.
onPartialFanoutClientPartialFanout ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  Program tx ()
onPartialFanoutClientPartialFanout pfs selection
  | nullOutputs selection || not (selection `isSubMultisetOf` remainingOutputs) =
      emit . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (FanoutProgress pfs)
  | otherwise =
      fanoutStepStateChange headId step remainingOutputs
        *> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- The on-chain datum is only @FanoutProgress@ once a partial fanout has
  -- actually landed, regardless of which outputs match our local snapshot.
  -- A pending request still spends @Closed@, as does its rollback re-post.
  step = nextFanoutStep confirmedSnapshot version selection remainingOutputs fanoutDatum

  PartialFanoutState{..} = pfs
