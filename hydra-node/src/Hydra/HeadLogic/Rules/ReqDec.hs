{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Receive a decommit request. Deposit selection takes precedence over a
-- pending decommit, and ledger applicability takes precedence over empty outputs.
module Hydra.HeadLogic.Rules.ReqDec (Phase (..), ReqDecTopology, reqDecTopology, sReqDecTopology, machine) where

import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Map.Strict qualified as Map
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (depositPeriod, party),
 )
import Hydra.HeadLogic.Rules.SendSnapshot (broadcastSnapshot)
import Hydra.HeadLogic.Rules.SnapshotCapacity (checkDecommitCapacity)
import Hydra.HeadLogic.Rules.SnapshotRequest (
  buildSnapshotRequest,
  decommitStatus,
  isLeader,
  requestDepositTxId,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  snapshotInFlight,
 )
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
import Hydra.Node.State (ChainPointTime (..), Deposit (..), PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

$( singletons
    [d|
      data Phase = CheckCommit | CheckExistingDecommit | CheckOutputs | CheckCapacity | RecordAndRequest | Finished

      reqDecTopology :: Topology Phase
      reqDecTopology =
        Topology
          [ (CheckCommit, [CheckExistingDecommit, Finished])
          , (CheckExistingDecommit, [CheckOutputs, Finished])
          , (CheckOutputs, [CheckCapacity, Finished])
          , (CheckCapacity, [RecordAndRequest, Finished])
          , (RecordAndRequest, [Finished])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data State tx phase where
  CommitCheck :: State tx 'CheckCommit
  ExistingCheck :: State tx 'CheckExistingDecommit
  OutputCheck :: ~(UTxOType tx) -> State tx 'CheckOutputs
  CapacityCheck :: ~(UTxOType tx) -> State tx 'CheckCapacity
  Record :: State tx 'RecordAndRequest
  Done :: State tx 'Finished

statePhase :: State tx phase -> Phase
statePhase = \case
  CommitCheck -> CheckCommit
  ExistingCheck -> CheckExistingDecommit
  OutputCheck{} -> CheckOutputs
  CapacityCheck{} -> CheckCapacity
  Record -> RecordAndRequest
  Done -> Finished

machine :: forall tx signing. IsChainState tx => RuleEnvironment -> TTL -> ChainPointTime -> PendingDeposits tx -> OpenState tx -> tx -> Workflow tx 'True signing ReqDecTopology
machine env ttl ChainPointTime{currentSlot, currentChainTime} pendingDeposits openState decommitTx =
  workflow (show . statePhase) CommitCheck step
 where
  step :: State tx from -> Transition tx 'True signing ReqDecTopology (State tx) from
  step = \case
    CommitCheck
      | Just depositTxId <- selectedDeposit
      , Just Deposit{deposited = commitUTxO} <- Map.lookup depositTxId pendingDeposits -> continue (const Done) $ do
          void $ Program.decide "next snapshot carries a deposit" True
          canRetry <- Program.decide "retry budget remains" (ttl > 0)
          if canRetry
            then Program.suspend WaitOnUnresolvedCommit{commitUTxO}
            else invalid DepositInFlight{depositTxId, commitUTxO}
      | otherwise -> continue (const ExistingCheck) $ Program.decide "next snapshot carries a deposit" False
    ExistingCheck -> case mExistingDecommitTx of
      Just existing -> continue (const Done) $ do
        void $ Program.decide "no decommit is in flight" False
        notApplicable DecommitAlreadyInFlight{otherDecommitTxId = txId existing}
      Nothing | settlingDecommit version confirmedSnapshot -> continue (const Done) $ do
        void $ Program.decide "no decommit is in flight" False
        notApplicable DecommitTxInvalid{localUTxO, validationError = ValidationError "confirmed decommit awaits settlement"}
      Nothing -> choose $ do
        void $ Program.decide "no decommit is in flight" True
        result <- Program.applyTransaction currentSlot localUTxO decommitTx
        case result of
          Right after -> pure $ next $ OutputCheck after
          Left validationError -> do
            notApplicable DecommitTxInvalid{localUTxO, validationError}
            pure $ next Done
    OutputCheck after
      | utxoFromTx decommitTx == mempty -> continue (const Done) $ do
          void $ Program.decide "decommit produces outputs" False
          invalid DecommitTxInvalid{localUTxO, validationError = ValidationError "decommit transaction has no outputs"}
      | otherwise -> continue (const $ CapacityCheck after) $ Program.decide "decommit produces outputs" True
    CapacityCheck after -> case checkDecommitCapacity after (settledUTxO version previous) decommitTx of
      Left validationError -> continue (const Done) $ do
        void $ Program.decide "decommit fits local and confirmed capacity" False
        invalid DecommitTxInvalid{localUTxO, validationError}
      Right () -> continue (const Record) $ Program.decide "decommit fits local and confirmed capacity" True
    Record -> continue (const Done) $ do
      void $ Coordinated.stage (Coordinated.start openState) $ Coordinated.reserveDecommit decommitTx
      noSnapshot <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- Program.decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $ do
          void $ Program.decide "snapshot request has useful work" (maybe False snd selectedRequest)
          forM_ selectedRequest $ \(request, useful) -> when useful $ broadcastSnapshot request
    Done -> finish Done

  invalid :: DecommitInvalidReason tx -> Program.Program tx 'True signing ()
  invalid reason = Program.change DecommitInvalid{headId, decommitTx, decommitInvalidReason = reason}
  notApplicable :: DecommitInvalidReason tx -> Program.Program tx 'True signing ()
  notApplicable reason = do
    canRetry <- Program.decide "retry budget remains" (ttl > 0)
    if canRetry then Program.suspend $ WaitOnNotApplicableDecommitTx reason else invalid reason
  selectedDeposit = selectedRequest >>= requestDepositTxId . fst
  selectedRequest =
    buildSnapshotRequest version previous localTxs pendingDeposits currentDepositTxId (decommitStatus version previous $ Just decommitTx) (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous)
  previous = getSnapshot confirmedSnapshot
  nextSn = previous.number + 1
  CoordinatedHeadState{decommitTx = mExistingDecommitTx, confirmedSnapshot, localTxs, localUTxO, version, seenSnapshot, currentDepositTxId} = coordinatedHeadState
  OpenState{headId, parameters, coordinatedHeadState} = openState
