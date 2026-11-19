{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Receive and apply a transaction. Programs inside a transition keep related
-- guards and effects together; crem constrains transitions between phases.
module Hydra.HeadLogic.Rules.Network.ReqTx (Stage (..), ReqTxTopology, handle, workflow) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (filter, lookup, member)
import Data.Sequence qualified as Seq ((|>))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (WaitReason (..))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  decide,
  suspend,
 )
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.Transactions (
  ValidatedTransaction,
  validateTransaction,
 )
import Hydra.HeadLogic.Snapshot.Request (
  SettlingCommit (..),
  chainedRequest,
  contextSettlingCommit,
  currentRequestContext,
  isLeader,
 )
import Hydra.HeadLogic.Snapshot.Settlement (
  matchesSnapshotDecommit,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    localTxs,
    localUTxO,
    seenSnapshot
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
  run,
  workflow,
 )
import Hydra.Ledger (ValidationError (..))
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  DepositStatus (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, handle, state)
import Hydra.Tx (UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

data Stage = CheckDuplicate | CheckOutputs | CheckDeposits | SelectSnapshot | RetryOrInvalidate | Finished

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

workflow :: forall tx. IsChainState tx => RuleEnvironment -> ChainPointTime -> OpenState tx -> TTL -> PendingDeposits tx -> tx -> Workflow tx ReqTxTopology ()
workflow env ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx =
  Workflow.workflow (show . statePhase) DuplicateCheck step
 where
  step :: State tx from -> Transition tx ReqTxTopology (State tx) from ()
  step = \case
    DuplicateCheck
      | SeenSnapshot snapshot _ _ <- seenSnapshot
      , Just{} <- snapshot.utxoToDecommit
      , Just known <- Map.lookup (txId tx) allTxs
      , matchesSnapshotDecommit snapshot known ->
          continue (const Done) $ Program.decide "transaction is retained by the candidate decommit" True
      | Map.member (txId tx) allTxs && any ((== txId tx) . txId) localTxs ->
          continue (const Done) $ Program.decide "transaction is already applied locally" True
      | otherwise -> continue OutputCheck $ do
          void $ Program.decide "transaction is already applied locally" False
          Staging.stage (Coordinated.start st) $ Coordinated.receiveTransaction tx
    OutputCheck runtime -> choose $ do
      result <- validateTransaction "outputs do not collide with the local UTxO" currentSlot localUTxO tx
      pure $ case result of
        Right checked -> next $ DepositCheck runtime checked
        Left err -> next $ Retry runtime err
    DepositCheck runtime checked
      | namesNoneOf (utxoFromTx tx) namedByDepositsInPlay -> continue Select $ do
          void $ Program.decide "outputs do not collide with pending deposits" True
          Staging.stage runtime $ Coordinated.applyTransaction checked
      | otherwise ->
          continue (const $ Retry runtime $ ValidationError "transaction creates an output a pending deposit names") $
            Program.decide "outputs do not collide with pending deposits" False
    Select runtime -> continue (const Done) $ do
      noSnapshot <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- Program.decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $
          case chainedRequest env.snapshotLimits context of
            Nothing -> void $ Program.decide "snapshot request has useful work" False
            Just request -> do
              void $ Program.decide "snapshot request has useful work" True
              sendSnapshot runtime request
    Retry runtime err -> continue (const Done) $ do
      -- Expiry still drops the payload. A later dependency does not wake this
      -- transaction; changing that recovery policy is deferred.
      canRetry <- Program.decide "retry budget remains" (ttl > 0)
      if canRetry
        then Program.suspend $ WaitOnNotApplicableTx err
        else void $ Staging.stage runtime $ Coordinated.markTransactionInvalid localUTxO tx err
    Done -> finish Done ()

  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty
  OpenState{coordinatedHeadState, parameters} = st
  CoordinatedHeadState{localTxs, localUTxO, allTxs, confirmedSnapshot, seenSnapshot} = coordinatedHeadState
  previous = getSnapshot confirmedSnapshot
  nextSn = previous.number + 1
  context = currentRequestContext env.depositPeriod currentChainTime pendingDeposits coordinatedHeadState{localTxs = localTxs Seq.|> tx}
  settlingCommit = contextSettlingCommit context

-- | Handle one input through the inspectable workflow.
handle :: forall tx. IsChainState tx => RuleEnvironment -> ChainPointTime -> OpenState tx -> TTL -> PendingDeposits tx -> tx -> Program tx ()
handle env chainPoint st ttl pendingDeposits tx = Workflow.run $ workflow env chainPoint st ttl pendingDeposits tx
