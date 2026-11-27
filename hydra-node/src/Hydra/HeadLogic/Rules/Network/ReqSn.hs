{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Network.ReqSn (
  Stage (..),
  ReqSnTopology,
  Args (..),
  workflow,
  ValidationStage (..),
  ValidationTopology,
  ValidatedSnapshot,
  validatedSnapshot,
  validate,
  validationWorkflow,
) where

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map (fromList, keysSet, lookup)
import Data.Set ((\\))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Cardano (
  advanceCommitments,
  checkCommitmentCapacity,
 )
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
 )
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (Effect (..), WaitReason (..))
import Hydra.HeadLogic.Program (
  Program,
  applyTransaction,
  emit,
  reject,
  signSnapshot,
  suspend,
  traceB,
 )
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging (
  stage,
 )
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (depositPeriod, snapshotLimits),
 )
import Hydra.HeadLogic.Rules.Transactions (
  applyTransactionsProgram,
  checkTransactionOutputs,
  pruneTransactions,
 )
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Request (
  SettlingCommit (ContinueCommit),
  isLeader,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Snapshot.Settlement (
  finalizedDepositTxId,
  settledUTxO,
 )
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    finalizedCommit,
    localTxs,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  SeenSnapshot (..),
  seenSnapshotNumber,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  acceptSnapshot,
  awaitSnapshotTransactions,
  retainSnapshotDecommit,
  start,
 )
import Hydra.HeadLogic.Transaction.Outputs (absorbable)
import Hydra.HeadLogic.Validation (ValidatedSnapshot (..))
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  choose,
  continue,
  finish,
  next,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow (run, workflow)
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (
  ChainPointTime (..),
  Deposit (..),
  DepositStatus (..),
  PendingDeposits,
 )
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (
  TxIdType,
  UTxOType,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))
import Hydra.Tx.Accumulator qualified as Accumulator (
  HydraAccumulator,
 )
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
 )

data ValidationStage = CheckRequest | ResolveDecommit | ResolveDeposit | ResolveTransactions | ApplyTransactions | BuildSnapshot | Validated

type ValidationTopology =
  'Topology
    '[ '( 'CheckRequest, '[ 'ResolveDecommit])
     , '( 'ResolveDecommit, '[ 'ResolveDeposit])
     , '( 'ResolveDeposit, '[ 'ResolveTransactions])
     , '( 'ResolveTransactions, '[ 'ApplyTransactions])
     , '( 'ApplyTransactions, '[ 'BuildSnapshot])
     , '( 'BuildSnapshot, '[ 'Validated])
     ]

deriving stock instance Eq ValidationStage
deriving stock instance Show ValidationStage
deriving stock instance Enum ValidationStage
deriving stock instance Bounded ValidationStage

data ValidationState tx phase where
  Request :: ValidationState tx 'CheckRequest
  Checked :: ValidationState tx 'ResolveDecommit
  DecommitResolved :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> ValidationState tx 'ResolveDeposit
  DepositResolved :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> ValidationState tx 'ResolveTransactions
  TransactionsResolved :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> [tx] -> ValidationState tx 'ApplyTransactions
  TransactionsApplied :: ~(UTxOType tx) -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> [tx] -> ValidationState tx 'BuildSnapshot
  Built :: ValidatedSnapshot tx -> ValidationState tx 'Validated

validationPhase :: ValidationState tx phase -> ValidationStage
validationPhase = \case
  Request -> CheckRequest
  Checked -> ResolveDecommit
  DecommitResolved{} -> ResolveDeposit
  DepositResolved{} -> ResolveTransactions
  TransactionsResolved{} -> ApplyTransactions
  TransactionsApplied{} -> BuildSnapshot
  Built{} -> Validated

data Args tx = Args
  { env :: ~RuleEnvironment
  , pendingDeposits :: ~(PendingDeposits tx)
  , chainPointTime :: ~ChainPointTime
  , openState :: ~(OpenState tx)
  , ttl :: ~TTL
  , sender :: ~Party
  , snapshotVersion :: ~SnapshotVersion
  , snapshotNumber :: ~SnapshotNumber
  , transactionIds :: ~[TxIdType tx]
  , decommitTx :: ~(Maybe tx)
  , depositTxId :: ~(Maybe (TxIdType tx))
  }

validatedSnapshot :: ValidatedSnapshot tx -> Snapshot tx
validatedSnapshot (ValidatedSnapshot value) = value

validationWorkflow :: forall tx. IsChainState tx => Program tx () -> Args tx -> Workflow tx ValidationTopology (ValidatedSnapshot tx)
validationWorkflow onMissing Args{chainPointTime = ChainPointTime{currentSlot, currentChainTime}, decommitTx = mDecommitTx, depositTxId = mDepositTxId, ..} =
  Workflow.workflow (show . validationPhase) Request step
 where
  step :: ValidationState tx from -> Transition tx ValidationTopology (ValidationState tx) from (ValidatedSnapshot tx)
  step = \case
    Request -> continue (const Checked) $ do
      validNumber <- traceB "request is the next snapshot" (snapshotNumber == seenSn + 1)
      unless validNumber $ reject $ RequireFailed ReqSnNumberInvalid{requestedSn = snapshotNumber, lastSeenSn = seenSn}
      leader <- traceB "sender leads this snapshot" (isLeader parameters sender snapshotNumber)
      unless leader $ reject $ RequireFailed ReqSnNotLeader{requestedSn = snapshotNumber, leader = sender}
      confirmed <- traceB "previous snapshot is confirmed" (confSn == seenSn)
      unless confirmed $ suspend $ WaitOnSnapshotNumber seenSn
      -- A follower may still be waiting for the increment/decrement observation.
      matches <- traceB "snapshot version matches" (version == snapshotVersion)
      unless matches $ suspend $ WaitOnSnapshotVersion snapshotVersion
    Checked -> continue (uncurry DecommitResolved) resolveDecommit
    DecommitResolved active decommit -> continue (\commit -> DepositResolved active commit decommit) resolveDeposit
    DepositResolved active commit decommit -> continue (TransactionsResolved active commit decommit) $ do
      let missingTxIds = toList (fromList transactionIds \\ Map.keysSet knownTxs)
      case missingTxIds of
        [] -> pure $ mapMaybe (`Map.lookup` knownTxs) transactionIds
        unseen -> onMissing >> suspend (WaitOnTxs unseen)
    TransactionsResolved active commit decommit txs -> continue (\after -> TransactionsApplied after commit decommit txs) $ do
      case (mDepositTxId, commit) of
        (Just depositTxId, Just deposited)
          | not (absorbable active txs deposited) ->
              reject $ RequireFailed ReqSnDepositOutputsHeld{depositTxId}
        _ -> pure ()
      -- Registry entries may be unvalidated receipts. Apply every requested tx.
      -- Freshness is relative to the round's initial active set as well as
      -- the evolving ledger: spending a deposited reference cannot free its name.
      forM_ txs $ \tx -> case checkTransactionOutputs active tx of
        Left err -> reject $ RequireFailed $ SnapshotDoesNotApply snapshotNumber (txId tx) err
        Right () -> pure ()
      result <- applyTransactionsProgram currentSlot active txs
      case result of
        Left (tx, err) -> reject $ RequireFailed $ SnapshotDoesNotApply snapshotNumber (txId tx) err
        Right after -> pure after
    TransactionsApplied utxo utxoToCommit utxoToDecommit confirmed -> continue Built $ do
      let (accumulator, appliedAccumulator) = advanceCommitments previous utxo utxoToCommit utxoToDecommit
      checkSize "unsettled fanout fits accumulator capacity" accumulator
      checkSize "settled fanout fits accumulator capacity" appliedAccumulator
      pure $ ValidatedSnapshot Snapshot{number = snapshotNumber, depositTxId = mDepositTxId, ..}
    Built checked -> finish (Built checked) checked

  resolveDecommit :: Program tx (UTxOType tx, Maybe (UTxOType tx))
  resolveDecommit = case mDecommitTx of
    Nothing
      | snapshotVersion == confVersion && isJust confUTxOToDecommit -> reject $ RequireFailed ReqSnDecommitNotSettled
      | otherwise -> pure (confirmedUTxO, Nothing)
    Just tx
      | Just depositTxId <- mDepositTxId -> reject $ RequireFailed ReqSnBothCommitAndDecommit{depositTxId, decommitTxId = txId tx}
      | utxoFromTx tx == mempty -> reject $ RequireFailed ReqSnDecommitNoOutputs{decommitTxId = txId tx}
      | snapshotVersion == confVersion && isJust confUTxOToDecommit ->
          if confUTxOToDecommit == Just (utxoFromTx tx)
            then pure (confirmedUTxO, confUTxOToDecommit)
            else reject $ RequireFailed ReqSnDecommitNotSettled
      | otherwise -> do
          result <- applyTransaction currentSlot confirmedUTxO tx
          case result of
            Left err -> reject $ RequireFailed $ SnapshotDoesNotApply snapshotNumber (txId tx) err
            Right after -> let outputs = utxoFromTx tx in pure (after `withoutUTxO` outputs, Just outputs)

  -- A deposit is carried separately: it becomes spendable only after increment.
  resolveDeposit :: Program tx (Maybe (UTxOType tx))
  resolveDeposit = case mDepositTxId of
    Nothing -> do
      let settlingCommit = settlingCommitAtPeriod env.depositPeriod version currentChainTime pendingDeposits previous
      case settlingCommit of
        Just (ContinueCommit (_, Deposit{deposited})) -> suspend WaitOnUnresolvedCommit{commitUTxO = deposited}
        _ -> pure Nothing
    Just depositTxId
      | finalizedDepositTxId finalizedCommit == Just depositTxId -> reject $ RequireFailed ReqSnDepositBlockedByFinalizedCommit{depositTxId}
      | otherwise -> case Map.lookup depositTxId pendingDeposits of
          Nothing
            | ttl > 0 -> suspend WaitOnDepositObserved{depositTxId}
            | otherwise -> reject $ RequireFailed RequestedDepositNotFoundLocally{depositTxId}
          Just Deposit{status, deposited}
            | status == Inactive -> suspend WaitOnDepositActivation{depositTxId}
            -- Identity matters as well as content. An expired deposit remains
            -- admissible here while its already-signed increment can still land.
            | snapshotVersion == confVersion && isJust confUTxOToCommit ->
                if confUTxOToCommit == Just deposited && confDepositTxId == Just depositTxId
                  then pure confUTxOToCommit
                  else reject $ RequireFailed ReqSnCommitNotSettled
            | status == Expired -> reject $ RequireFailed RequestedDepositExpired{depositTxId}
            | otherwise -> pure $ Just deposited

  checkSize :: Text -> Accumulator.HydraAccumulator -> Program tx ()
  checkSize decision accumulator = do
    let result = checkCommitmentCapacity env.snapshotLimits accumulator
    void $ traceB decision (isRight result)
    case result of
      Left AccumulatorTooLarge{..} -> reject $ RequireFailed ReqSnUTxOSetTooLarge{..}
      Right () -> pure ()

  knownTxs = allTxs <> Map.fromList [(txId tx, tx) | tx <- toList localTxs]
  previous = getSnapshot confirmedSnapshot
  confSn = case confirmedSnapshot of InitialSnapshot{} -> 0; ConfirmedSnapshot{} -> previous.number
  confVersion = previous.version
  confUTxOToCommit = case confirmedSnapshot of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> previous.utxoToCommit
  confDepositTxId = case confirmedSnapshot of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> previous.depositTxId
  confUTxOToDecommit = case confirmedSnapshot of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> previous.utxoToDecommit
  seenSn = seenSnapshotNumber seenSnapshot
  confirmedUTxO = settledUTxO version previous
  CoordinatedHeadState{..} = coordinatedHeadState
  OpenState{..} = openState

-- | Validate a request without signing or changing live state. Only a complete
-- successful validation can produce the value accepted by the snapshot owner.
validate :: forall tx. IsChainState tx => Args tx -> Program tx (ValidatedSnapshot tx)
validate = Workflow.run . validationWorkflow (pure ())

data Stage = ValidateRequest | SignAndAcknowledge | PruneLocalTransactions | Finished

type ReqSnTopology =
  'Topology
    '[ '( 'ValidateRequest, '[ 'SignAndAcknowledge, 'Finished])
     , '( 'SignAndAcknowledge, '[ 'PruneLocalTransactions])
     , '( 'PruneLocalTransactions, '[ 'Finished])
     ]

deriving stock instance Eq Stage
deriving stock instance Show Stage
deriving stock instance Enum Stage
deriving stock instance Bounded Stage

data State tx phase where
  Validate :: State tx 'ValidateRequest
  Sign :: ValidatedSnapshot tx -> State tx 'SignAndAcknowledge
  Acknowledged :: ValidatedSnapshot tx -> State tx 'PruneLocalTransactions
  Done :: State tx 'Finished

statePhase :: State tx phase -> Stage
statePhase = \case
  Validate -> ValidateRequest
  Sign{} -> SignAndAcknowledge
  Acknowledged{} -> PruneLocalTransactions
  Done -> Finished

workflow :: forall tx. IsChainState tx => Args tx -> Workflow tx ReqSnTopology ()
workflow args@Args{..} = Workflow.workflow (show . statePhase) Validate step
 where
  pinRequest = void $ Staging.stage (Coordinated.start openState) $ Coordinated.awaitSnapshotTransactions snapshotNumber transactionIds

  step :: State tx from -> Transition tx ReqSnTopology (State tx) from ()
  step = \case
    Validate -> choose $ case openState.coordinatedHeadState.seenSnapshot of
      SeenSnapshot{cachedSnapshot}
        | let snapshot = Cached.snapshot cachedSnapshot
        , isLeader openState.parameters sender snapshotNumber
        , snapshot.number == snapshotNumber
        , snapshot.version == snapshotVersion
        , (txId <$> snapshot.confirmed) == transactionIds
        , snapshot.depositTxId == depositTxId
        , snapshot.utxoToDecommit == (utxoFromTx <$> decommitTx) -> do
            signed <- signSnapshot snapshot
            emit $ NetworkRetransmission [AckSn signed snapshotNumber]
            pure $ next Done
      _ -> next . Sign <$> Workflow.run (validationWorkflow pinRequest args)
    Sign checked -> continue (const $ Acknowledged checked) $ do
      signed <- signSnapshot $ validatedSnapshot checked
      emit $ NetworkEffect AckSn{..}
    Acknowledged checked -> continue (const Done) $ do
      -- Buffer the acknowledgement before pruning. SnapshotRequested reconstructs
      -- the local ledger by applying the retained transactions in order.
      let candidate = validatedSnapshot checked
      newLocalTxs <- pruneTransactions env.snapshotLimits chainPointTime.currentSlot candidate.utxoToCommit candidate.utxoToDecommit candidate.utxo openState.coordinatedHeadState.localTxs
      runtime <- maybe (pure $ Coordinated.start openState) (Staging.stage (Coordinated.start openState) . Coordinated.retainSnapshotDecommit) decommitTx
      void $ Staging.stage runtime $ Coordinated.acceptSnapshot checked newLocalTxs
    Done -> finish Done ()
