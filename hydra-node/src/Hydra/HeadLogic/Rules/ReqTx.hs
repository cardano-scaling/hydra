{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Receive and apply a transaction. Programs inside a transition keep related
-- guards and effects together; crem constrains transitions between phases.
module Hydra.HeadLogic.Rules.ReqTx (Phase (..), ReqTxTopology, reqTxTopology, sReqTxTopology, machine) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (BaseMachineT (..), InitialState (..))
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..), WaitReason (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (..))
import Hydra.HeadLogic.Rules.SnapshotRequest (SettlingCommit (..), SnapshotRequest (..), chainedSnapshotRequest, isLeader, settlingCommitAtPeriod)
import Hydra.HeadLogic.Rules.Transactions (checkTransactionOutputs)
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
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Operations (
  namesNoneOf,
  settledUTxO,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Ledger (ValidationError (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), PendingDeposits)
import Hydra.Tx (IsTx (..), UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

$( singletons
    [d|
      data Phase = CheckDuplicate | CheckOutputs | CheckLedgerResult | CheckDeposits | SelectSnapshot | RetryOrInvalidate | Finished

      reqTxTopology :: Topology Phase
      reqTxTopology =
        Topology
          [ (CheckDuplicate, [CheckOutputs, Finished])
          , (CheckOutputs, [CheckLedgerResult, RetryOrInvalidate])
          , (CheckLedgerResult, [CheckDeposits, RetryOrInvalidate])
          , (CheckDeposits, [SelectSnapshot, RetryOrInvalidate])
          , (SelectSnapshot, [Finished])
          , (RetryOrInvalidate, [Finished])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data State tx phase where
  DuplicateCheck :: State tx 'CheckDuplicate
  OutputCheck :: State tx 'CheckOutputs
  LedgerResult :: Either ValidationError (UTxOType tx) -> State tx 'CheckLedgerResult
  DepositCheck :: ~(UTxOType tx) -> State tx 'CheckDeposits
  Select :: State tx 'SelectSnapshot
  Retry :: ValidationError -> State tx 'RetryOrInvalidate
  Done :: State tx 'Finished

machine :: forall tx signing. IsTx tx => RuleEnvironment -> ChainPointTime -> OpenState tx -> TTL -> PendingDeposits tx -> tx -> Workflow tx 'True signing ReqTxTopology
machine env ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx =
  BaseMachineT (InitialState DuplicateCheck) $ \state () -> step state
 where
  step :: State tx from -> Transition tx 'True signing ReqTxTopology (State tx) from
  step = \case
    DuplicateCheck
      | Map.member (txId tx) allTxs && any ((== txId tx) . txId) localTxs ->
          continue (const Done) $ Program.decide "transaction is already applied locally" True
      | otherwise -> continue (const OutputCheck) $ do
          void $ Program.decide "transaction is already applied locally" False
          Program.change TransactionReceived{tx}
    OutputCheck -> case checkTransactionOutputs localUTxO tx of
      Right () -> continue LedgerResult $ do
        void $ Program.decide "outputs do not collide with the local UTxO" True
        Program.applyTransaction currentSlot localUTxO tx
      Left err -> continue (const $ Retry err) $ Program.decide "outputs do not collide with the local UTxO" False
    LedgerResult result -> case result of
      Right after -> continue (const $ DepositCheck after) $ pure ()
      Left err -> continue (const $ Retry err) $ pure ()
    DepositCheck after
      | namesNoneOf (utxoFromTx tx) namedByDepositsInPlay -> continue (const Select) $ do
          void $ Program.decide "outputs do not collide with pending deposits" True
          Program.recordAppliedTransaction headId tx localUTxO after
      | otherwise ->
          continue (const $ Retry $ ValidationError "transaction creates an output a pending deposit names") $
            Program.decide "outputs do not collide with pending deposits" False
    Select -> continue (const Done) $ do
      noSnapshot <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- Program.decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $
          case chainedSnapshotRequest confirmedUTxO (localTxs Seq.|> tx) pendingDeposits currentDepositTxId decommitTx settlingCommit of
            Nothing -> void $ Program.decide "snapshot request has useful work" False
            Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} -> do
              void $ Program.decide "snapshot request has useful work" True
              Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
              Program.emit $ NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId
    Retry err -> continue (const Done) $ do
      canRetry <- Program.decide "retry budget remains" (ttl > 0)
      if canRetry
        then Program.suspend $ WaitOnNotApplicableTx err
        else Program.change TxInvalid{headId, utxo = localUTxO, transaction = tx, validationError = err}
    Done -> finish Done

  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty
  OpenState{coordinatedHeadState, headId, parameters} = st
  CoordinatedHeadState{localTxs, localUTxO, allTxs, confirmedSnapshot, seenSnapshot, decommitTx, version, currentDepositTxId} = coordinatedHeadState
  nextSn = (getSnapshot confirmedSnapshot).number + 1
  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)
  settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot)
