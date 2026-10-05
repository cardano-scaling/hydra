{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Closing the head: the close and contest transactions, and distributing the
-- head's final UTxO set through a full or partial fanout.
--
-- The functions here decide the transactions to post and the 'StateChanged'
-- events to record, as 'Hydra.HeadLogic.update' dispatches them. The folds that
-- apply those events live in "Hydra.HeadLogic.Aggregate".
module Hydra.HeadLogic.Close where

import Hydra.Prelude

import Data.List (partition)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.HeadLogic.Outcome (Effect (..), Outcome, StateChanged (..), cause, newState, noop)
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FanoutStepLanded (..),
  HeadState (..),
  OpenState (..),
  PartialFanoutState (..),
 )
import Hydra.Tx (HeadId, HeadSeed, IsTx (..), UTxOType, combinedUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, ξ)
  cause
    OnChainEffect
      { postChainTx =
          CloseTx
            { headId
            , headParameters = parameters
            , openVersion = version
            , closingSnapshot = confirmedSnapshot
            }
      }
 where
  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId, parameters} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  IsTx tx =>
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  newState HeadClosed{headId, snapshotNumber = closedSnapshotNumber, chainState = newChainState, contestationDeadline}
    & maybePostContest
 where
  maybePostContest outcome =
    -- Spec: if ̅S.s > sc
    if number (getSnapshot confirmedSnapshot) > closedSnapshotNumber
      then
        outcome
          -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
          --       ξ ← ̅S.σ
          --       postTx (contest, ̅S.v, ̅S.s, η, ξ)
          <> cause
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
      else outcome

  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{parameters = headParameters, headId, coordinatedHeadState} = openState

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state.
  ChainStateType tx ->
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline =
  if
    | -- Spec: if ̅S.s > sc
      number (getSnapshot confirmedSnapshot) > snapshotNumber ->
        -- Spec: η# ← ̅S.(η')#  (the confirmed snapshot's stored accumulator hash; not recomputed at close/contest)
        --       ξ ← ̅S.σ
        --       postTx (contest, ̅S.v, ̅S.s, η, ξ)
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
          <> cause
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
    | snapshotNumber > number (getSnapshot confirmedSnapshot) ->
        -- TODO: A more recent snapshot number was successfully contested, we will
        -- not be able to fanout! We might want to communicate that to the client!
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
    | otherwise ->
        newState HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
 where
  ClosedState{parameters = headParameters, confirmedSnapshot, headId, version} = closedState

-- | Client request to fanout the whole closed head automatically. Emits a
-- 'FanoutTx'; the chain layer either lands a single full fanout (→ 'IdleState')
-- or falls back to dynamically-chunked partial fanouts. The first observed
-- partial fanout transitions the head to 'PartialFanout' in 'AutoDrain' mode,
-- which keeps draining the rest automatically until the final (burning) step.
--
-- This node becomes the fanout /driver/: it transitions into 'PartialFanout' in
-- 'AutoDrain' mode so that, as the chain layer chunks the fanout, /this/ node
-- auto-continues to completion. Other parties that merely observe the resulting
-- partial fanout do not auto-drive (see 'onClosedChainPartialFanoutTx').
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState' (then → 'IdleState' once
-- the final fanout is observed).
onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  -- A plain 'Fanout' is a full fanout by definition, and is only reachable from
  -- 'Closed'.
  fanoutStepStateChange headId FullFanoutStep fullUTxO
    <> emitFanoutStep FullFanoutStep confirmedSnapshot version headSeed contestationDeadline
 where
  fullUTxO = computeFullFanoutUTxO closedState

  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | The state change that goes with a fanout step, for the callers that own the
-- head state: taking the driver role, or recording the selection being
-- distributed. Decided here once rather than in each handler.
--
-- The selection comes from the step rather than the caller, so a step can only
-- ever be recorded against the set it actually distributes.
fanoutStepStateChange ::
  HeadId ->
  NextFanoutStep tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  Outcome tx
fanoutStepStateChange headId step remainingOutputs =
  case step of
    -- The selection is not distributed as one: it becomes a full fanout, and this
    -- node drives the rest of it.
    FullFanoutStep -> newState HeadFanoutInitiated{headId, remainingOutputs}
    -- 'FinalStep' posts a transaction and nothing else, so the selection has to
    -- be recorded for it too: a rollback or restart before that transaction
    -- lands otherwise leaves the driver with no selection to resume from.
    FinalStep{stepDistribute} -> recordSelection stepDistribute
    PartialStep{stepDistribute} -> recordSelection stepDistribute
 where
  recordSelection selection = newState HeadPartialFanoutSelected{headId, remainingOutputs, selection}

-- | Which transaction the next fanout step has to be, carrying the sets that
-- transaction needs. Decided once by 'nextFanoutStep' so that callers deciding
-- what else to record and 'emitFanoutStep' deciding what to post cannot
-- disagree, and so that no caller can pair a step with sets it does not go with.
data NextFanoutStep tx
  = -- | The target is the whole remainder and the head is already in
    -- @FanoutProgress@ on chain: the final step, distributing the rest and
    -- burning the head tokens.
    FinalStep {stepDistribute :: UTxOType tx}
  | -- | The target is the whole remainder but the head output still carries the
    -- @Closed@ datum, which the final step is not valid against. Posted as a
    -- non-final step it would empty the head, which 'mustNotBeLastBatch' rejects,
    -- so the chunk search settles for one output less and the head needs a second
    -- transaction to finish — and cannot be drained at all when a single output
    -- is left. Covering everything is a full fanout, so post that (#2855).
    FullFanoutStep
  | -- | The target is a strict subset: a non-final chunk distributing it, proved
    -- against the set the head's current datum commits to.
    PartialStep {stepDistribute :: UTxOType tx, stepProof :: UTxOType tx}

-- | Decide the next fanout step. The multiset comparison here is the only one
-- needed: callers pass the result on rather than re-deriving it.
nextFanoutStep ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  -- | Chunk source for the next step (the user selection, or the whole remaining
  --   set when auto-draining)
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  OnChainFanoutDatum ->
  NextFanoutStep tx
nextFanoutStep confirmedSnapshot version target remaining onChainDatum
  | target `sameOutputs` remaining =
      case onChainDatum of
        DatumFanoutProgress -> FinalStep{stepDistribute = remaining}
        DatumClosed -> FullFanoutStep
  | otherwise = PartialStep{stepDistribute = target, stepProof = proofSet}
 where
  -- The set the on-chain datum commits to: the fan-out-able set from a @Closed@
  -- head, and the not-yet-distributed set once the head is in @FanoutProgress@,
  -- since every step removes exactly what it distributed.
  proofSet = case onChainDatum of
    DatumClosed -> fanoutUTxOFromSnapshot confirmedSnapshot version
    DatumFanoutProgress -> remaining

-- | Given the on-chain @version@ and a snapshot's own version, decide which of a
-- pending commit / decommit is still to be distributed on fanout. When the
-- increment has landed on chain (versions match) the commit was already applied
-- (drop it) while a pending decommit still applies; otherwise the commit still
-- applies and the decommit was already paid out. Centralises the version check
-- shared by 'mkFullFanoutTx' and 'fanoutUTxOFromSnapshot'.
effectiveCommitDecommit ::
  -- | On-chain version
  SnapshotVersion ->
  -- | Snapshot version
  SnapshotVersion ->
  -- | Pending commit
  Maybe (UTxOType tx) ->
  -- | Pending decommit
  Maybe (UTxOType tx) ->
  (Maybe (UTxOType tx), Maybe (UTxOType tx))
effectiveCommitDecommit onChainVersion snapshotVersion utxoToCommit utxoToDecommit
  | snapshotVersion == onChainVersion = (Nothing, utxoToDecommit)
  | otherwise = (utxoToCommit, Nothing)

-- | Build the full automatic 'FanoutTx' from a confirmed snapshot at the given
-- on-chain version. Shared by 'onClosedClientFanout' and the rollback re-post in
-- 'FanoutProgress' ('repostFanoutStep').
mkFullFanoutTx ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  PostChainTx tx
mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline =
  FanoutTx
    { utxo
    , utxoToCommit = effectiveCommit
    , utxoToDecommit = effectiveDecommit
    , headSeed
    , contestationDeadline
    }
 where
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot

-- | Client request to fan out a user-selected subset of a freshly closed head.
-- Validates the selection is a non-empty sub-multiset (by content) of the
-- fan-out-able UTxO, then transitions the head into 'PartialFanout' and posts
-- whichever step 'nextFanoutStep' decides on: a strict subset records the
-- selection and posts a 'PartialFanoutTx', while a selection covering the whole
-- head is a full fanout and drains it automatically instead.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedClientPartialFanout ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  Outcome tx
onClosedClientPartialFanout closedState selection
  | nullOutputs selection || not (selection `isSubMultisetOf` fullUTxO) =
      cause . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (Closed closedState)
  | otherwise =
      -- A selection covering the whole head becomes a full fanout - what the user
      -- means by "fan out everything", and the only option for a head holding a
      -- single UTxO, since a non-final batch has to leave one behind.
      fanoutStepStateChange headId step fullUTxO
        <> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- Fresh head: the on-chain datum is still @Closed@.
  step = nextFanoutStep confirmedSnapshot version selection fullUTxO DatumClosed

  fullUTxO = computeFullFanoutUTxO closedState
  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | Client request to continue a selective partial fanout. Validates the
-- selection against the current 'remainingOutputs' and emits whichever step
-- 'nextFanoutStep' decides on, which is not always a recorded selection: one
-- covering the whole remainder finalizes the head, or, if nothing has been
-- distributed yet, becomes a full fanout that drains it automatically.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutClientPartialFanout ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  Outcome tx
onPartialFanoutClientPartialFanout pfs selection
  | nullOutputs selection || not (selection `isSubMultisetOf` remainingOutputs) =
      cause . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (FanoutProgress pfs)
  | otherwise =
      fanoutStepStateChange headId step remainingOutputs
        <> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- The on-chain datum is only @FanoutProgress@ once a partial fanout has
  -- actually landed (some outputs distributed); until then it is still @Closed@
  -- and a 'FinalPartialFanoutTx' is not yet valid. Compute this the same way as
  -- 'repostFanoutStep' rather than assuming 'True'.
  step = nextFanoutStep confirmedSnapshot version selection remainingOutputs (onChainFanoutDatum distributedOutputs)

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, distributedOutputs} = pfs

-- | Observe a (full or final) fanout transaction, finalizing the head.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  newState HeadFannedOut{headId, finalizedOutputs = fanoutUTxO, chainState = newChainState}
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while this node is still 'Closed' — i.e. a partial
-- fanout this node did NOT initiate (another party did). The fanout driver moved
-- to 'PartialFanout' when it issued its 'Fanout'/'PartialFanout' command, so this
-- handler is only reached by passive observers.
--
-- The observer transitions into 'PartialFanout' in 'AwaitingSelection' mode and
-- does __not__ auto-drive the rest: only the driver advances the fanout. This is
-- what makes selective partial fanout work in a multi-party head — observers must
-- not steamroll the remaining UTxO the driver deliberately left.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Outcome tx
onClosedChainPartialFanoutTx closedState newChainState observedDistributed =
  let fullUTxO = computeFullFanoutUTxO closedState
      remaining = removeDistributedOutputs (outputsOfUTxO observedDistributed) fullUTxO
      distributedUTxO = withoutUTxO fullUTxO remaining
   in newState
        HeadPartialFannedOut
          { headId
          , distributedOutputs = distributedUTxO
          , remainingOutputs = remaining
          , chainState = newChainState
          , mode = AwaitingSelection
          }
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while in 'PartialFanout'. Updates the remaining and
-- distributed sets and, depending on the current 'FanoutMode', either continues
-- draining automatically (or within the active selection) or waits for the next
-- 'PartialFanout' command.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  Outcome tx
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
        newState
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
        -- The head's remaining set is now empty: emit the final (burning) step.
        -- This must happen regardless of mode — otherwise a selection that
        -- drains everything would stop at 'AwaitingSelection' and wedge the
        -- head, never burning the tokens.
        | nullOutputs remaining = finalize
        | otherwise = case newMode of
            AutoDrain -> finalize
            DistributingSelection sel' ->
              emitStep (nextFanoutStep confirmedSnapshot version sel' remaining DatumFanoutProgress)
            AwaitingSelection -> noop
   in record <> continue
 where
  emitStep step = emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, mode} = pfs

-- | Observe the final fanout while in 'PartialFanout', finalizing the head with
-- the accumulated distributed outputs plus this final batch.
--
-- __Transition__: 'PartialFanoutState' → 'IdleState'
onPartialFanoutChainFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Outcome tx
onPartialFanoutChainFanoutTx pfs newChainState fanoutUTxO =
  newState HeadFannedOut{headId, finalizedOutputs = distributedOutputs <> fanoutUTxO, chainState = newChainState}
 where
  PartialFanoutState{headId, distributedOutputs} = pfs

-- | Compute the full UTxO set to be fanned out, combining snapshot utxo
-- with utxoToCommit/utxoToDecommit based on version.
computeFullFanoutUTxO ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx
computeFullFanoutUTxO ClosedState{confirmedSnapshot, version} =
  fanoutUTxOFromSnapshot confirmedSnapshot version

-- | The fan-out-able UTxO of a confirmed snapshot at the given on-chain version:
-- the snapshot UTxO plus a pending commit (if the increment landed on chain) or a
-- pending decommit (if the decrement has not landed yet).
fanoutUTxOFromSnapshot ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  UTxOType tx
fanoutUTxOFromSnapshot confirmedSnapshot version =
  combinedUTxO utxo effectiveCommit effectiveDecommit
 where
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit

-- | Build the 'PartialFanout' ('FanoutProgress') head state from a closed head,
-- carrying over the snapshot/parameters and using the given chain state, remaining
-- and distributed UTxO and fanout 'mode'. Shared by the three @Closed →
-- FanoutProgress@ transitions in 'aggregateNodeState'.
closedToFanoutProgress ::
  ClosedState tx ->
  ChainStateType tx ->
  UTxOType tx ->
  UTxOType tx ->
  FanoutMode tx ->
  -- | The steps already observed, see 'FanoutStepLanded'
  [FanoutStepLanded tx] ->
  HeadState tx
closedToFanoutProgress closedState chainState remaining distributed mode stepsLanded =
  FanoutProgress
    PartialFanoutState
      { parameters
      , confirmedSnapshot
      , contestationDeadline
      , chainState
      , headId
      , headSeed
      , version
      , remainingOutputs = remaining
      , distributedOutputs = distributed
      , mode
      , stepsLanded
      , everLanded = not (null stepsLanded)
      }
 where
  ClosedState{parameters, confirmedSnapshot, contestationDeadline, headId, headSeed, version} = closedState

-- | Rebuild the 'ClosedState' from a 'PartialFanoutState' when reverting an
-- optimistic 'Closed' → 'PartialFanout' transition (see 'HeadFanoutReverted').
-- 'readyToFanoutSent' is restored to 'True' because a fanout is only reachable
-- after the head was announced 'ReadyToFanout'.
fanoutProgressToClosed :: PartialFanoutState tx -> ClosedState tx
fanoutProgressToClosed pfs =
  ClosedState
    { parameters
    , confirmedSnapshot
    , contestationDeadline
    , readyToFanoutSent = True
    , chainState
    , headId
    , headSeed
    , version
    }
 where
  PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version} = pfs

-- | The step this node is currently driving. 'Nothing' while a manual fanout
-- waits on the next selection, where nothing is pending.
currentFanoutStep :: IsTx tx => PartialFanoutState tx -> Maybe (NextFanoutStep tx)
currentFanoutStep PartialFanoutState{confirmedSnapshot, version, remainingOutputs, distributedOutputs, mode} =
  case mode of
    AwaitingSelection -> Nothing
    AutoDrain -> Just (step remainingOutputs)
    DistributingSelection selection -> Just (step selection)
 where
  step target = nextFanoutStep confirmedSnapshot version target remainingOutputs onChainDatum

  onChainDatum = onChainFanoutDatum distributedOutputs

-- | Whether a posted transaction is the one a given step posts, telling a failure
-- of the step this node is driving from one it has already moved past.
--
-- Compares the distributed set, not just the transaction shape, since two
-- selections can both be non-final steps. By content, because naming the same
-- outputs under other inputs is a valid way to ask for the same set.
matchesFanoutStep :: IsTx tx => NextFanoutStep tx -> PostChainTx tx -> Bool
matchesFanoutStep step postChainTx =
  case (step, postChainTx) of
    -- Only one full fanout is ever in flight: it covers the whole head.
    (FullFanoutStep, FanoutTx{}) -> True
    (FinalStep{stepDistribute}, FinalPartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    (PartialStep{stepDistribute}, PartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    _ -> False

removeDistributedOutputs :: IsTx tx => [TxOutType tx] -> UTxOType tx -> UTxOType tx
removeDistributedOutputs = flip (foldl' (flip removeOneOutputFromUTxO))

-- | Whether a UTxO has no outputs.
nullOutputs :: IsTx tx => UTxOType tx -> Bool
nullOutputs = null . outputsOfUTxO

-- | Whether the outputs of @sub@ are a sub-multiset (by content) of @sup@. This
-- mirrors how partial fanout tracks distributed UTxO by content rather than by
-- 'TxIn', so a user-provided selection is validated against what is actually
-- still in the head.
isSubMultisetOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
isSubMultisetOf sub sup =
  length (outputsOfUTxO (removeDistributedOutputs (outputsOfUTxO sub) sup))
    == length (outputsOfUTxO sup)
    - length (outputsOfUTxO sub)

-- | Whether two UTxO sets have the same outputs (by content, as a multiset).
-- The @a == b@ short-circuit avoids the O(n²) multiset comparison in the common
-- case where both arguments are the same tracked set (e.g. the auto-drain
-- @sameOutputs remaining remaining@ check on every observed chunk), which is
-- exactly the large-UTxO heads partial fanout targets.
sameOutputs :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
sameOutputs a b =
  a == b || (length (outputsOfUTxO a) == length (outputsOfUTxO b) && a `isSubMultisetOf` b)

-- | Which on-chain head datum the next fanout step will be posted against:
-- still @Closed@ (no partial fanout has landed yet) or already @FanoutProgress@.
-- A 'FinalPartialFanoutTx' (which burns the head tokens) is only valid once the
-- datum is 'DatumFanoutProgress'.
data OnChainFanoutDatum = DatumClosed | DatumFanoutProgress

-- | The on-chain datum implied by how much has been distributed so far: still
-- @Closed@ while nothing has landed, @FanoutProgress@ once some has.
onChainFanoutDatum :: IsTx tx => UTxOType tx -> OnChainFanoutDatum
onChainFanoutDatum distributed
  | nullOutputs distributed = DatumClosed
  | otherwise = DatumFanoutProgress

-- | The mode a recorded selection leaves the driver in. A selection covering the
-- whole remainder with nothing distributed yet is not distributed as a selection
-- at all: 'nextFanoutStep' routes it to a full fanout and the driver auto-drains
-- the rest, so that is the mode it has to be recorded under.
--
-- 'fanoutStepStateChange' never pairs the two - such a step is a
-- 'FullFanoutStep', recorded as 'HeadFanoutInitiated' - so this only normalises
-- a selection recorded before that routing existed and replayed here. Without it
-- the re-posts ('repostFanoutStep') would post the full fanout that
-- 'nextFanoutStep' decides on while the mode kept claiming a selection is being
-- distributed, which is what every 'HeadPartiallyFannedOut' then reports.
recordedSelectionMode ::
  IsTx tx =>
  -- | Distributed so far
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  -- | The recorded selection
  UTxOType tx ->
  FanoutMode tx
recordedSelectionMode distributed remaining selection
  | nullOutputs distributed && selection `sameOutputs` remaining = AutoDrain
  | otherwise = DistributingSelection selection

-- | Post the transaction a decided 'NextFanoutStep' calls for. The chain layer
-- chunks a full fanout, and sizes a partial one, dynamically.
--
-- Transactions only: any accompanying state change belongs to the caller that
-- owns the head state, via 'fanoutStepStateChange'. That keeps this safe to call
-- where effects can be applied but state changes cannot, as in the startup
-- re-post in 'Hydra.Node.runHydraNode'.
emitFanoutStep ::
  IsTx tx =>
  -- | The step to emit, from 'nextFanoutStep'.
  NextFanoutStep tx ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  Outcome tx
emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline =
  case step of
    FinalStep{stepDistribute} ->
      cause
        OnChainEffect
          { postChainTx =
              FinalPartialFanoutTx
                { utxoToDistribute = stepDistribute
                , headSeed
                , contestationDeadline
                }
          }
    FullFanoutStep ->
      cause OnChainEffect{postChainTx = mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline}
    PartialStep{stepDistribute, stepProof} ->
      cause
        OnChainEffect
          { postChainTx =
              PartialFanoutTx
                { utxoToDistribute = stepDistribute
                , utxoForProof = stepProof
                , headSeed
                , contestationDeadline
                }
          }

-- | Re-post the next fanout step after a chain rollback while in
-- 'FanoutProgress', so the fanout resumes instead of stalling with the
-- rolled-back transaction gone and nothing re-posted. This mirrors the
-- Increment/Decrement re-post ('repostErased'). The caller passes the progress
-- rewound to the steps still on chain ('rewindFanoutProgress'). Without the
-- rewind, automatic mode posted the step after the erased one, built against
-- a datum the chain no longer had, and manual mode posted nothing.
--
-- Which step that is comes from 'currentFanoutStep', the same derivation the
-- revert guard uses to tell this step's failure from a superseded one's.
repostFanoutStep :: IsTx tx => PartialFanoutState tx -> Outcome tx
repostFanoutStep pfs =
  case currentFanoutStep pfs of
    -- Manual mode paused on the user: nothing to re-post, wait for the next
    -- 'PartialFanout' command.
    Nothing -> noop
    -- Only the transaction is re-posted: this node is the driver already, and
    -- re-posting is not a new decision to record. So this emits no state
    -- changes, which is what lets 'Hydra.Node.runHydraNode' apply its effects
    -- alone at startup.
    Just step ->
      emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  PartialFanoutState{confirmedSnapshot, version, headSeed, contestationDeadline} = pfs

-- | Rewind the fanout's progress after a rollback to the given slot, to the
-- steps still on the chain we follow. The erased steps' outputs are back in
-- the head, and the driver's mode goes back to what those steps replaced. See
-- 'FanoutStepLanded'.
--
-- A step observed exactly at the rollback point is still on chain, so only
-- steps strictly after it are erased, as in 'markErased'.
rewindFanoutProgress :: IsTx tx => ChainSlot -> PartialFanoutState tx -> PartialFanoutState tx
rewindFanoutProgress rolledBackSlot pfs@PartialFanoutState{confirmedSnapshot, version, mode, stepsLanded, distributedOutputs}
  | null erased = pfs
  | otherwise =
      pfs
        { stepsLanded = kept
        , distributedOutputs = distributed
        , remainingOutputs = removeDistributedOutputs (outputsOfUTxO distributed) (fanoutUTxOFromSnapshot confirmedSnapshot version)
        , mode = rewoundMode
        }
 where
  (kept, erased) = partition (\FanoutStepLanded{landedAt} -> landedAt <= rolledBackSlot) stepsLanded

  -- Subtract the erased steps' outputs rather than sum the kept ones. A state
  -- persisted before steps were recorded has distributed outputs that no step
  -- accounts for, and those must stay distributed.
  distributed = removeDistributedOutputs (outputsOfUTxO (foldMap (\FanoutStepLanded{stepOutputs} -> stepOutputs) erased)) distributedOutputs

  -- The outputs of an erased step that one of this node's selections was
  -- distributing are its job again. A driver draining automatically covers
  -- them anyway, and an observer, whose steps replaced no selection, must not
  -- start driving.
  erasedSelected =
    foldMap (\FanoutStepLanded{stepOutputs} -> stepOutputs) [s | s@FanoutStepLanded{modeBefore = DistributingSelection{}} <- erased]

  rewoundMode = case mode of
    AutoDrain -> AutoDrain
    DistributingSelection selection
      | nullOutputs erasedSelected -> mode
      | otherwise -> DistributingSelection (erasedSelected <> selection)
    AwaitingSelection
      | nullOutputs erasedSelected -> AwaitingSelection
      | otherwise -> DistributingSelection erasedSelected
