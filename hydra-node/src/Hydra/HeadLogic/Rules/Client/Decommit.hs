{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Rules.Client.Decommit (
  Stage (..),
  DecommitTopology,
  handleDecommit,
  workflow,
) where

import Crem.Topology (Topology (Topology))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Limits (SnapshotLimits)
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (
  Program,
  applyTransaction,
  change,
  decide,
  emit,
 )
import Hydra.HeadLogic.Snapshot.Capacity (checkDecommitCapacity)
import Hydra.HeadLogic.Snapshot.Settlement (
  settledUTxO,
  settlingDecommit,
 )
import Hydra.HeadLogic.State (CoordinatedHeadState (..))
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (
  run,
  workflow,
 )
import Hydra.Ledger (ValidationError (..))
import Hydra.Network.Message (Message (..))
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId, IsTx, UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (getSnapshot)

data Stage
  = CheckPending
  | CheckOutputs
  | CheckCapacity
  | Broadcast
  | Finished

type DecommitTopology =
  'Topology
    '[ '( 'CheckPending, '[ 'CheckOutputs, 'Finished])
     , '( 'CheckOutputs, '[ 'CheckCapacity, 'Finished])
     , '( 'CheckCapacity, '[ 'Broadcast, 'Finished])
     , '( 'Broadcast, '[ 'Finished])
     ]
deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  Pending :: State tx 'CheckPending
  Outputs :: ~(UTxOType tx) -> State tx 'CheckOutputs
  Capacity :: ~(UTxOType tx) -> State tx 'CheckCapacity
  Send :: State tx 'Broadcast
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  Pending -> CheckPending
  Outputs{} -> CheckOutputs
  Capacity{} -> CheckCapacity
  Send -> Broadcast
  Done -> Finished

workflow ::
  forall tx.
  IsTx tx =>
  SnapshotLimits ->
  HeadId ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  tx ->
  Workflow tx DecommitTopology
workflow limits headId currentSlot CoordinatedHeadState{decommitTx = pending, localUTxO, version, confirmedSnapshot} decommitTx =
  Workflow.workflow (show . statePhase) Pending step
 where
  step :: State tx from -> Transition tx DecommitTopology (State tx) from
  step = \case
    Pending -> case pending of
      Just existing -> continue (const Done) $ do
        void $ decide "no decommit is in flight" False
        invalid DecommitAlreadyInFlight{otherDecommitTxId = txId existing}
      Nothing | settlingDecommit version confirmedSnapshot -> continue (const Done) $ do
        void $ decide "no decommit is in flight" False
        invalid DecommitTxInvalid{localUTxO, validationError = ValidationError "confirmed decommit awaits settlement"}
      Nothing -> choose $ do
        void $ decide "no decommit is in flight" True
        result <- applyTransaction currentSlot localUTxO decommitTx
        case result of
          Right after -> pure $ next $ Outputs after
          Left validationError -> do
            invalid DecommitTxInvalid{localUTxO, validationError}
            pure $ next Done
    Outputs after
      | utxoFromTx decommitTx == mempty -> continue (const Done) $ do
          void $ decide "decommit produces outputs" False
          invalid DecommitTxInvalid{localUTxO, validationError = ValidationError "decommit transaction has no outputs"}
      | otherwise -> continue (const $ Capacity after) $ decide "decommit produces outputs" True
    Capacity after -> case checkDecommitCapacity limits after (settledUTxO version $ getSnapshot confirmedSnapshot) decommitTx of
      Left validationError -> continue (const Done) $ do
        void $ decide "decommit fits local and confirmed capacity" False
        invalid DecommitTxInvalid{localUTxO, validationError}
      Right () -> continue (const Send) $ decide "decommit fits local and confirmed capacity" True
    Send -> continue (const Done) $ emit $ NetworkEffect ReqDec{transaction = decommitTx}
    Done -> finish Done

  invalid :: DecommitInvalidReason tx -> Program tx ()
  invalid reason = change DecommitInvalid{headId, decommitTx, decommitInvalidReason = reason}

handleDecommit ::
  forall tx.
  IsTx tx =>
  SnapshotLimits ->
  HeadId ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  tx ->
  Program tx ()
handleDecommit limits headId currentSlot coordinatedState decommitTx =
  Workflow.run $ workflow limits headId currentSlot coordinatedState decommitTx
