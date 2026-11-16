{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Network.ReqTx (
  Stage (..),
  ReqTxTopology,
  Args (..),
  workflow,
) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (filter, lookup, member)
import Data.Sequence qualified as Seq ((|>))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (WaitReason (..))
import Hydra.HeadLogic.Program (
  decide,
  suspend,
 )
import Hydra.HeadLogic.Rules.Coordinated (sendSnapshot)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.Transactions (
  ValidatedTransaction,
  validateTransaction,
 )
import Hydra.HeadLogic.Snapshot.Request (
  SettlingCommit (..),
  chainedSnapshotRequest,
  decommitStatus,
  isLeader,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Snapshot.Settlement (
  matchesSnapshotDecommit,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, parameters),
  SeenSnapshot (SeenSnapshot),
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  Runtime,
  applyTransaction,
  markTransactionInvalid,
  receiveTransaction,
  start,
 )
import Hydra.HeadLogic.Transaction.Outputs (namesNoneOf)
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
  DepositStatus (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data Stage
  = CheckDuplicate
  | CheckOutputs
  | CheckDeposits
  | SelectSnapshot
  | RetryOrInvalidate
  | Finished

type ReqTxTopology =
  'Topology
    '[ '( 'CheckDuplicate, '[ 'CheckOutputs, 'Finished])
     , '( 'CheckOutputs, '[ 'CheckDeposits, 'RetryOrInvalidate])
     , '( 'CheckDeposits, '[ 'SelectSnapshot, 'RetryOrInvalidate])
     , '( 'SelectSnapshot, '[ 'Finished])
     , '( 'RetryOrInvalidate, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  DuplicateCheck :: State tx 'CheckDuplicate
  OutputCheck :: Coordinated.Runtime tx -> State tx 'CheckOutputs
  DepositCheck :: Coordinated.Runtime tx -> ValidatedTransaction tx -> State tx 'CheckDeposits
  Select :: Coordinated.Runtime tx -> State tx 'SelectSnapshot
  Retry :: Coordinated.Runtime tx -> ValidationError -> State tx 'RetryOrInvalidate
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  DuplicateCheck -> CheckDuplicate
  OutputCheck{} -> CheckOutputs
  DepositCheck{} -> CheckDeposits
  Select{} -> SelectSnapshot
  Retry{} -> RetryOrInvalidate
  Done -> Finished

data Args tx = Args
  { env :: ~RuleEnvironment
  , chainPointTime :: ~ChainPointTime
  , openState :: ~(OpenState tx)
  , ttl :: ~TTL
  , pendingDeposits :: ~(PendingDeposits tx)
  , transaction :: ~tx
  }

workflow :: forall tx. IsChainState tx => Args tx -> Workflow tx ReqTxTopology ()
workflow Args{chainPointTime = ChainPointTime{currentSlot, currentChainTime}, ..} =
  Workflow.workflow (show . statePhase) DuplicateCheck step
 where
  step :: State tx from -> Transition tx ReqTxTopology (State tx) from ()
  step = \case
    DuplicateCheck
      | SeenSnapshot snapshot _ _ <- seenSnapshot
      , Just{} <- snapshot.utxoToDecommit
      , Just known <- Map.lookup (txId transaction) allTxs
      , matchesSnapshotDecommit snapshot known ->
          continue (const Done) $ decide "transaction is retained by the candidate decommit" True
      | Map.member (txId transaction) allTxs && any ((== txId transaction) . txId) localTxs ->
          continue (const Done) $ decide "transaction is already applied locally" True
      | otherwise -> continue OutputCheck $ do
          void $ decide "transaction is already applied locally" False
          Staging.stage (Coordinated.start openState) $ Coordinated.receiveTransaction transaction
    OutputCheck runtime -> choose $ do
      result <- validateTransaction "outputs do not collide with the local UTxO" currentSlot localUTxO transaction
      pure $ case result of
        Right checked -> next $ DepositCheck runtime checked
        Left err -> next $ Retry runtime err
    DepositCheck runtime checked
      | namesNoneOf (utxoFromTx transaction) namedByDepositsInPlay -> continue Select $ do
          void $ decide "outputs do not collide with pending deposits" True
          Staging.stage runtime $ Coordinated.applyTransaction checked
      | otherwise ->
          continue (const $ Retry runtime $ ValidationError "transaction creates an output a pending deposit names") $
            decide "outputs do not collide with pending deposits" False
    Select runtime -> continue (const Done) $ do
      noSnapshot <- decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $ do
          let nextRequest =
                chainedSnapshotRequest
                  env.snapshotLimits
                  version
                  previous
                  (localTxs Seq.|> transaction)
                  pendingDeposits
                  currentDepositTxId
                  (decommitStatus version previous decommitTx)
                  settlingCommit
          case nextRequest of
            Nothing -> void $ decide "snapshot request has useful work" False
            Just request -> do
              void $ decide "snapshot request has useful work" True
              sendSnapshot runtime request
    Retry runtime err -> continue (const Done) $ do
      -- Expiry still drops the payload. A later dependency does not wake this
      -- transaction; changing that recovery policy is deferred.
      canRetry <- decide "retry budget remains" (ttl > 0)
      if canRetry
        then suspend $ WaitOnNotApplicableTx err
        else void $ Staging.stage runtime $ Coordinated.markTransactionInvalid localUTxO transaction err
    Done -> finish Done ()

  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty
  OpenState{..} = openState
  CoordinatedHeadState{..} = coordinatedHeadState
  previous = getSnapshot confirmedSnapshot
  nextSn = previous.number + 1
  settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous
