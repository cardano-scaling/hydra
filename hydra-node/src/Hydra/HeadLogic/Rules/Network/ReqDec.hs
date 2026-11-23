{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Network.ReqDec (
  Stage (..),
  ReqDecTopology,
  Args (..),
  workflow,
) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (lookup)
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program (
  Program,
  applyTransaction,
  change,
  decide,
  suspend,
 )
import Hydra.HeadLogic.Rules.Coordinated (broadcastSnapshot)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (depositPeriod, party, snapshotLimits),
 )
import Hydra.HeadLogic.Snapshot.Capacity (checkDecommitCapacity)
import Hydra.HeadLogic.Snapshot.Request (
  buildSnapshotRequest,
  decommitStatus,
  isLeader,
  requestDepositTxId,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Snapshot.Settlement (
  settledUTxO,
  settlingDecommit,
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
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  recordDecommit,
  start,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (
  workflow,
 )
import Hydra.Ledger (ValidationError (..))
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data Stage
  = CheckCommit
  | CheckExistingDecommit
  | CheckOutputs
  | CheckCapacity
  | RecordAndRequest
  | Finished

type ReqDecTopology =
  'Topology
    '[ '( 'CheckCommit, '[ 'CheckExistingDecommit, 'Finished])
     , '( 'CheckExistingDecommit, '[ 'CheckOutputs, 'Finished])
     , '( 'CheckOutputs, '[ 'CheckCapacity, 'Finished])
     , '( 'CheckCapacity, '[ 'RecordAndRequest, 'Finished])
     , '( 'RecordAndRequest, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  CommitCheck :: State tx 'CheckCommit
  ExistingCheck :: State tx 'CheckExistingDecommit
  OutputCheck :: ~(UTxOType tx) -> State tx 'CheckOutputs
  CapacityCheck :: ~(UTxOType tx) -> State tx 'CheckCapacity
  Record :: State tx 'RecordAndRequest
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  CommitCheck -> CheckCommit
  ExistingCheck -> CheckExistingDecommit
  OutputCheck{} -> CheckOutputs
  CapacityCheck{} -> CheckCapacity
  Record -> RecordAndRequest
  Done -> Finished

data Args tx = Args
  { env :: ~RuleEnvironment
  , ttl :: ~TTL
  , chainPointTime :: ~ChainPointTime
  , pendingDeposits :: ~(PendingDeposits tx)
  , openState :: ~(OpenState tx)
  , transaction :: ~tx
  }

workflow :: forall tx. IsChainState tx => Args tx -> Workflow tx ReqDecTopology
workflow Args{chainPointTime = ChainPointTime{currentSlot, currentChainTime}, transaction = decommitTx, ..} =
  Workflow.workflow (show . statePhase) CommitCheck step
 where
  step :: State tx from -> Transition tx ReqDecTopology (State tx) from
  step = \case
    CommitCheck
      | Just depositTxId <- selectedDeposit
      , Just Deposit{deposited = commitUTxO} <- Map.lookup depositTxId pendingDeposits -> continue (const Done) $ do
          void $ decide "next snapshot carries a deposit" True
          canRetry <- decide "retry budget remains" (ttl > 0)
          if canRetry
            then suspend WaitOnUnresolvedCommit{commitUTxO}
            else invalid DepositInFlight{..}
      | otherwise -> continue (const ExistingCheck) $ decide "next snapshot carries a deposit" False
    ExistingCheck -> case mExistingDecommitTx of
      Just existing -> continue (const Done) $ do
        void $ decide "no decommit is in flight" False
        notApplicable DecommitAlreadyInFlight{otherDecommitTxId = txId existing}
      Nothing | settlingDecommit version confirmedSnapshot -> continue (const Done) $ do
        void $ decide "no decommit is in flight" False
        notApplicable DecommitTxInvalid{validationError = ValidationError "confirmed decommit awaits settlement", ..}
      Nothing -> choose $ do
        void $ decide "no decommit is in flight" True
        result <- applyTransaction currentSlot localUTxO decommitTx
        case result of
          Right after -> pure $ next $ OutputCheck after
          Left validationError -> do
            notApplicable DecommitTxInvalid{..}
            pure $ next Done
    OutputCheck after
      | utxoFromTx decommitTx == mempty -> continue (const Done) $ do
          void $ decide "decommit produces outputs" False
          invalid DecommitTxInvalid{validationError = ValidationError "decommit transaction has no outputs", ..}
      | otherwise -> continue (const $ CapacityCheck after) $ decide "decommit produces outputs" True
    CapacityCheck after -> case checkDecommitCapacity env.snapshotLimits after (settledUTxO version previous) decommitTx of
      Left validationError -> continue (const Done) $ do
        void $ decide "decommit fits local and confirmed capacity" False
        invalid DecommitTxInvalid{..}
      Right () -> continue (const Record) $ decide "decommit fits local and confirmed capacity" True
    Record -> continue (const Done) $ do
      void $ Staging.stage (Coordinated.start openState) $ Coordinated.recordDecommit decommitTx
      noSnapshot <- decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $ do
          void $ decide "snapshot request has useful work" (maybe False snd selectedRequest)
          forM_ selectedRequest $ \(request, useful) -> when useful $ broadcastSnapshot request
    Done -> finish Done

  invalid :: DecommitInvalidReason tx -> Program tx ()
  invalid decommitInvalidReason = change DecommitInvalid{..}
  notApplicable :: DecommitInvalidReason tx -> Program tx ()
  notApplicable reason = do
    canRetry <- decide "retry budget remains" (ttl > 0)
    if canRetry then suspend $ WaitOnNotApplicableDecommitTx reason else invalid reason
  selectedDeposit = selectedRequest >>= requestDepositTxId . fst
  selectedRequest =
    buildSnapshotRequest
      env.snapshotLimits
      version
      previous
      localTxs
      pendingDeposits
      currentDepositTxId
      (decommitStatus version previous $ Just decommitTx)
      (settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous)
  previous = getSnapshot confirmedSnapshot
  nextSn = previous.number + 1
  CoordinatedHeadState{decommitTx = mExistingDecommitTx, ..} = coordinatedHeadState
  OpenState{..} = openState
