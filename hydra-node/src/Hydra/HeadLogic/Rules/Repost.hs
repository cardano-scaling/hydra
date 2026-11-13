{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Repost where

import Data.Map.Strict qualified as Map
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, emit)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Fanout (repostFanoutStep)
import Hydra.HeadLogic.Rules.Synchronisation (handleOutOfSync)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    decommitTx,
    finalizedCommit,
    finalizedDecommit
  ),
  FinalizedSnapshot (..),
  HeadState (FanoutProgress, Open),
  OpenState (
    OpenState,
    coordinatedHeadState,
    headId,
    headSeed,
    parameters
  ),
 )
import Hydra.Node.State (Deposit (..), PendingDeposits, SyncedStatus, depositsForHead)
import Hydra.Prelude
import Hydra.Tx (HeadId, HeadSeed, IsTx)
import Hydra.Tx.HeadParameters (HeadParameters)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)

maybeRepostIncrementTx ::
  IsTx tx =>
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  PendingDeposits tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  -- | Slot rolled back to
  ChainSlot ->
  Program tx ()
maybeRepostIncrementTx headSeed headId parameters pendingDeposits confirmedSnapshot finalizedCommit rolledBackSlot =
  -- NOTE: the deposit comes from the confirmed snapshot itself, not from
  -- 'currentDepositTxId'. Only the deposit bound into the signed snapshot can be
  -- claimed on-chain, and 'DepositActivated' can set 'currentDepositTxId' to an
  -- unrelated deposit after that snapshot was confirmed.
  case confirmedSnapshot of
    ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId}, signatures}
      | Just Deposit{} <- Map.lookup depositTxId pendingDeposits ->
          repost ConfirmedSnapshot{snapshot, signatures} depositTxId
    _ ->
      case finalizedCommit of
        Just FinalizedSnapshot{snapshot, observedAtSlot}
          -- Strictly (<): the rollback point is the last common block, so
          -- rolling back TO the increment's slot means it is still on chain.
          | rolledBackSlot < observedAtSlot
          , Just depositTxId <- (getSnapshot snapshot).depositTxId ->
              repost snapshot depositTxId
        _ -> pure ()
 where
  repost incrementingSnapshot depositTxId =
    emit
      OnChainEffect
        { postChainTx =
            IncrementTx
              { headSeed
              , headId
              , headParameters = parameters
              , incrementingSnapshot
              , depositTxId
              }
        }

-- | On rollback, re-post the DecrementTx if there is a pending decommit whose
-- confirmed snapshot contains a matching utxoToDecommit. The rollback may have
-- erased the original on-chain DecrementTx observation.
--
-- If the decrement was already finalized ('DecommitFinalized' cleared
-- 'decommitTx' and 'confirmedSnapshot' may have advanced past the decrementing
-- snapshot), fall back to the retained 'finalizedDecommit' — but only when the
-- rollback reaches strictly before its observation slot. See #2741.
maybeRepostDecrementTx ::
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  Maybe tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  -- | Slot rolled back to
  ChainSlot ->
  Program tx ()
maybeRepostDecrementTx headSeed headId parameters mDecommitTx confirmedSnapshot finalizedDecommit rolledBackSlot =
  case (mDecommitTx, confirmedSnapshot) of
    (Just _, ConfirmedSnapshot{snapshot = snapshot@Snapshot{utxoToDecommit = Just _}, signatures}) ->
      repost ConfirmedSnapshot{snapshot, signatures}
    _ ->
      case finalizedDecommit of
        Just FinalizedSnapshot{snapshot, observedAtSlot}
          | rolledBackSlot < observedAtSlot ->
              repost snapshot
        _ -> pure ()
 where
  repost decrementingSnapshot =
    emit
      OnChainEffect
        { postChainTx =
            DecrementTx
              { headSeed
              , headId
              , headParameters = parameters
              , decrementingSnapshot
              }
        }

-- | Chain rollback preserves L2 state and reposts work selected from the retained
-- state. It does not reconstruct fanout progress on a divergent chain.
handleRollback :: IsChainState tx => RuleEnvironment -> UTCTime -> PendingDeposits tx -> HeadState tx -> ChainStateType tx -> UTCTime -> SyncedStatus -> Program tx ()
handleRollback env now pendingDeposits st rolledBackChainState chainTime syncStatus = do
  change ChainRolledBack{chainState = rolledBackChainState}
  handleOutOfSync env now (chainStatePoint rolledBackChainState) chainTime syncStatus
  case st of
    Open OpenState{headSeed, headId, parameters, coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot, decommitTx, finalizedCommit, finalizedDecommit}} -> do
      maybeRepostIncrementTx headSeed headId parameters (depositsForHead headId pendingDeposits) confirmedSnapshot finalizedCommit slot
      maybeRepostDecrementTx headSeed headId parameters decommitTx confirmedSnapshot finalizedDecommit slot
    FanoutProgress fanout -> repostFanoutStep fanout
    _ -> pure ()
 where
  slot = chainStateSlot rolledBackChainState
