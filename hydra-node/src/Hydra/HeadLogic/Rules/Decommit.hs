{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Client admission for a decommit. Receiving ReqDec reserves its inputs later.
module Hydra.HeadLogic.Rules.Decommit (Phase (..), DecommitTopology, decommitTopology, sDecommitTopology, machine) where

import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotCapacity (checkDecommitCapacity)
import Hydra.HeadLogic.State (CoordinatedHeadState (..))
import Hydra.HeadLogic.State.Operations (settledUTxO, settlingDecommit)
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
  workflow,
 )
import Hydra.Ledger (ValidationError (..))
import Hydra.Network.Message (Message (..))
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId, IsTx, UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (getSnapshot)

$( singletons
    [d|
      data Phase = CheckPending | CheckOutputs | CheckCapacity | Broadcast | Finished

      decommitTopology :: Topology Phase
      decommitTopology =
        Topology
          [ (CheckPending, [CheckOutputs, Finished])
          , (CheckOutputs, [CheckCapacity, Finished])
          , (CheckCapacity, [Broadcast, Finished])
          , (Broadcast, [Finished])
          ]
      |]
 )
deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data State tx phase where
  Pending :: State tx 'CheckPending
  Outputs :: ~(UTxOType tx) -> State tx 'CheckOutputs
  Capacity :: ~(UTxOType tx) -> State tx 'CheckCapacity
  Send :: State tx 'Broadcast
  Done :: State tx 'Finished

statePhase :: State tx phase -> Phase
statePhase = \case
  Pending -> CheckPending
  Outputs{} -> CheckOutputs
  Capacity{} -> CheckCapacity
  Send -> Broadcast
  Done -> Finished

machine :: forall tx signing. IsTx tx => HeadId -> ChainSlot -> CoordinatedHeadState tx -> tx -> Workflow tx 'True signing DecommitTopology
machine headId currentSlot CoordinatedHeadState{decommitTx = pending, localUTxO, version, confirmedSnapshot} decommitTx =
  workflow (show . statePhase) Pending step
 where
  step :: State tx from -> Transition tx 'True signing DecommitTopology (State tx) from
  step = \case
    Pending -> case pending of
      Just existing -> continue (const Done) $ do
        void $ Program.decide "no decommit is in flight" False
        invalid DecommitAlreadyInFlight{otherDecommitTxId = txId existing}
      Nothing | settlingDecommit version confirmedSnapshot -> continue (const Done) $ do
        void $ Program.decide "no decommit is in flight" False
        invalid DecommitTxInvalid{localUTxO, validationError = ValidationError "confirmed decommit awaits settlement"}
      Nothing -> choose $ do
        void $ Program.decide "no decommit is in flight" True
        result <- Program.applyTransaction currentSlot localUTxO decommitTx
        case result of
          Right after -> pure $ next $ Outputs after
          Left validationError -> do
            invalid DecommitTxInvalid{localUTxO, validationError}
            pure $ next Done
    Outputs after
      | utxoFromTx decommitTx == mempty -> continue (const Done) $ do
          void $ Program.decide "decommit produces outputs" False
          invalid DecommitTxInvalid{localUTxO, validationError = ValidationError "decommit transaction has no outputs"}
      | otherwise -> continue (const $ Capacity after) $ Program.decide "decommit produces outputs" True
    Capacity after -> case checkDecommitCapacity after (settledUTxO version $ getSnapshot confirmedSnapshot) decommitTx of
      Left validationError -> continue (const Done) $ do
        void $ Program.decide "decommit fits local and confirmed capacity" False
        invalid DecommitTxInvalid{localUTxO, validationError}
      Right () -> continue (const Send) $ Program.decide "decommit fits local and confirmed capacity" True
    Send -> continue (const Done) $ Program.emit $ NetworkEffect ReqDec{transaction = decommitTx}
    Done -> finish Done

  invalid :: DecommitInvalidReason tx -> Program.Program tx 'True signing ()
  invalid reason = Program.change DecommitInvalid{headId, decommitTx, decommitInvalidReason = reason}
