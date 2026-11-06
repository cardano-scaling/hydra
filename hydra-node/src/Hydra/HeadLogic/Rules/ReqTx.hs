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
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..), WaitReason (..))
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
  OpenState (OpenState, coordinatedHeadState, parameters),
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
import Hydra.Tx (UTxOType, txId, utxoFromTx)
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
  OutputCheck :: Coordinated.Runtime tx -> State tx 'CheckOutputs
  LedgerResult :: Coordinated.Runtime tx -> Either ValidationError (UTxOType tx) -> State tx 'CheckLedgerResult
  DepositCheck :: Coordinated.Runtime tx -> ~(UTxOType tx) -> State tx 'CheckDeposits
  Select :: Coordinated.Runtime tx -> State tx 'SelectSnapshot
  Retry :: Coordinated.Runtime tx -> ValidationError -> State tx 'RetryOrInvalidate
  Done :: State tx 'Finished

machine :: forall tx signing. IsChainState tx => RuleEnvironment -> ChainPointTime -> OpenState tx -> TTL -> PendingDeposits tx -> tx -> Workflow tx 'True signing ReqTxTopology
machine env ChainPointTime{currentSlot, currentChainTime} st ttl pendingDeposits tx =
  BaseMachineT (InitialState DuplicateCheck) $ \state () -> step state
 where
  step :: State tx from -> Transition tx 'True signing ReqTxTopology (State tx) from
  step = \case
    DuplicateCheck
      | Map.member (txId tx) allTxs && any ((== txId tx) . txId) localTxs ->
          continue (const Done) $ Program.decide "transaction is already applied locally" True
      | otherwise -> continue OutputCheck $ do
          void $ Program.decide "transaction is already applied locally" False
          Coordinated.stage (Coordinated.start st) $ Coordinated.receiveTransaction tx
    OutputCheck runtime -> case checkTransactionOutputs localUTxO tx of
      Right () -> continue (LedgerResult runtime) $ do
        void $ Program.decide "outputs do not collide with the local UTxO" True
        Program.applyTransaction currentSlot localUTxO tx
      Left err -> continue (const $ Retry runtime err) $ Program.decide "outputs do not collide with the local UTxO" False
    LedgerResult runtime result -> case result of
      Right after -> continue (const $ DepositCheck runtime after) $ pure ()
      Left err -> continue (const $ Retry runtime err) $ pure ()
    DepositCheck runtime after
      | namesNoneOf (utxoFromTx tx) namedByDepositsInPlay -> continue Select $ do
          void $ Program.decide "outputs do not collide with pending deposits" True
          Coordinated.stage runtime $ Coordinated.applyTransaction tx localUTxO after
      | otherwise ->
          continue (const $ Retry runtime $ ValidationError "transaction creates an output a pending deposit names") $
            Program.decide "outputs do not collide with pending deposits" False
    Select runtime -> continue (const Done) $ do
      noSnapshot <- Program.decide "no snapshot is in flight" (not $ snapshotInFlight seenSnapshot)
      when noSnapshot $ do
        leader <- Program.decide "this party leads the next snapshot" (isLeader parameters env.party nextSn)
        when leader $
          case chainedSnapshotRequest confirmedUTxO (localTxs Seq.|> tx) pendingDeposits currentDepositTxId decommitTx settlingCommit of
            Nothing -> void $ Program.decide "snapshot request has useful work" False
            Just SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} -> do
              void $ Program.decide "snapshot request has useful work" True
              void $ Coordinated.stage runtime $ Coordinated.requestSnapshot nextSn
              Program.emit $ NetworkEffect $ ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId
    Retry runtime err -> continue (const Done) $ do
      -- Expiry still drops the payload. A later dependency does not wake this
      -- transaction; changing that recovery policy is deferred.
      canRetry <- Program.decide "retry budget remains" (ttl > 0)
      if canRetry
        then Program.suspend $ WaitOnNotApplicableTx err
        else void $ Coordinated.stage runtime $ Coordinated.forgetTransaction localUTxO tx err
    Done -> finish Done

  namedByDepositsInPlay :: UTxOType tx
  namedByDepositsInPlay =
    foldMap (.deposited) (Map.filter (\Deposit{status} -> status /= Expired) pendingDeposits)
      <> case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> deposited
        _ -> mempty
  OpenState{coordinatedHeadState, parameters} = st
  CoordinatedHeadState{localTxs, localUTxO, allTxs, confirmedSnapshot, seenSnapshot, decommitTx, version, currentDepositTxId} = coordinatedHeadState
  nextSn = (getSnapshot confirmedSnapshot).number + 1
  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)
  settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot)
