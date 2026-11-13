{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.IncrementalWorkflowSpec (spec) where

import Data.Map.Strict qualified as Map
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localUTxO,
    seenSnapshot
  ),
  Effect (NetworkEffect, OnChainEffect),
  LogicError (RequireFailed),
  OpenState (coordinatedHeadState),
  Outcome (Continue, Error, Wait),
  RequirementFailure (
    NoMatchingDeposit,
    RecoverBlockedByFinalizedCommit
  ),
  SeenSnapshot (NoSeenSnapshot, RequestedSnapshot),
  StateChanged (
    DecommitInvalid,
    DecommitRecorded,
    decommitInvalidReason
  ),
  WaitReason (WaitOnNotApplicableDecommitTx, WaitOnUnresolvedCommit),
  onClientRecover,
  onOpenNetworkReqDec,
  ruleEnvironment,
 )
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..), initialChainTime)
import Hydra.Prelude
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..))
import Test.Hydra.HeadLogic.DecommitDependency qualified as DecommitDependency
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Node.State ()
import Test.Hydra.Prelude (
  Spec,
  it,
  shouldBe,
  shouldSatisfy,
 )
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, testHeadId)

spec :: Spec
spec = do
  it "confirms the dependent decommit while preserving every reservation" $
    DecommitDependency.decommitDependencyRegression `shouldSatisfy` isRight
  it "an authorised decommit takes precedence over a stale selected deposit" $ do
    let base = Scenario.openState Scenario.initialNode
        c = base.coordinatedHeadState
        tx = Scenario.transactionOne
        utxo = utxoRef 3
        accumulator = Accumulator.buildFromUTxO @SimpleTx utxo
        snapshot = Scenario.initialSnapshot{number = 1, utxo, accumulator, appliedAccumulator = accumulator, utxoToDecommit = Just tx.txOutputs}
        signatures = aggregateInOrder (Map.fromList [(alice, sign aliceSk snapshot), (bob, sign bobSk snapshot), (carol, sign carolSk snapshot)]) Scenario.scenarioParties
        st = base{coordinatedHeadState = c{localUTxO = utxo, confirmedSnapshot = ConfirmedSnapshot snapshot signatures, decommitTx = Just tx, currentDepositTxId = Just 7}}
        pending = Map.singleton 7 (deposit Active)
        receive = onOpenNetworkReqDec env simpleLedger 1 Scenario.initialNode.chainPointTime pending st Scenario.transactionTwo
    receive `shouldBe` Wait (WaitOnNotApplicableDecommitTx $ DecommitAlreadyInFlight 1) []

  it "ReqDec with confirmed inputs agrees with bounded Agda outcomes and guard priority" $
    forM_ [(commit, existing, applies, outputs, ttl, inFlight, actor) | commit <- [False, True], existing <- [False, True], applies <- [False, True], outputs <- [False, True], ttl <- [0, 1], inFlight <- [False, True], actor <- [0, 1, 2]] $ \(commit, existing, applies, outputs, ttl, inFlight, actor) -> do
      let base = Scenario.openState Scenario.initialNode
          c = base.coordinatedHeadState
          pending = if commit then Map.singleton 7 (deposit Active) else mempty
          st = base{coordinatedHeadState = c{currentDepositTxId = if commit then Just 7 else Nothing, decommitTx = if existing then Just Scenario.transactionTwo else Nothing, seenSnapshot = if inFlight then RequestedSnapshot 0 1 else NoSeenSnapshot}}
          tx = if outputs then Scenario.transactionOne else Scenario.transactionOne{txOutputs = mempty}
          ledger = Ledger $ \slot utxo txs ->
            if slot /= 0 || utxo /= c.localUTxO || txs /= [tx]
              then error "ReqDec changes the ledger query arguments"
              else if applies then Right utxo else Left (tx, controlledFailure)
          environment = Scenario.environment actor
          program = Workflow.run $ ReqDec.machine (ruleEnvironment environment) ttl Scenario.initialNode.chainPointTime pending st tx
          (outcome, observations) = Interpreter.traceProgram (testInterpreterConfig ledger) program
          expected = Agda.reqDecOutcomeRef commit existing applies outputs (toInteger ttl) inFlight 2 1 (toInteger actor)
      projectDecommit outcome `shouldBe` expected
      outcome `shouldBe` onOpenNetworkReqDec environment ledger ttl Scenario.initialNode.chainPointTime pending st tx
      length [() | Trace.LedgerApplied{} <- observations] `shouldBe` if commit || existing then 0 else 1

  it "Recover preserves missing/blocked/post outcomes across status and slot" $
    forM_ [(registered, blocked, status, slot) | registered <- [False, True], blocked <- [False, True], status <- [Inactive, Active, Expired], slot <- [0, 100]] $ \(registered, blocked, status, slot) -> do
      let pending = if registered then Map.singleton 7 (deposit status) else mempty
          finalized = if blocked then Just 7 else Nothing
          result = onClientRecover (testInterpreterConfig simpleLedger) slot pending finalized 7
      projectRecover result `shouldBe` Agda.recoverOutcomeRef registered blocked
 where
  env = Scenario.environment 0
  deposit :: DepositStatus -> Deposit SimpleTx
  deposit = Deposit testHeadId (utxoRef 99) initialChainTime (addUTCTime 3600 initialChainTime)

controlledFailure :: ValidationError
controlledFailure = ValidationError "controlled decommit rejection"

-- Fail on extra changes/effects rather than accepting a matching first tag.
projectDecommit :: Outcome SimpleTx -> Agda.HsReqDecOutcome
projectDecommit = \case
  Wait WaitOnUnresolvedCommit{} [] -> Agda.ReqDecWaitCommit
  Wait (WaitOnNotApplicableDecommitTx DecommitAlreadyInFlight{}) [] -> Agda.ReqDecWaitExisting
  Wait (WaitOnNotApplicableDecommitTx DecommitTxInvalid{validationError}) [] | validationError == controlledFailure -> Agda.ReqDecWaitLedger
  Continue [DecommitInvalid{decommitInvalidReason = DepositInFlight{}}] [] -> Agda.ReqDecInvalidCommit
  Continue [DecommitInvalid{decommitInvalidReason = DecommitAlreadyInFlight{}}] [] -> Agda.ReqDecInvalidExisting
  Continue [DecommitInvalid{decommitInvalidReason = DecommitTxInvalid{validationError}}] []
    | validationError == controlledFailure -> Agda.ReqDecInvalidLedger
    | validationError == ValidationError "decommit transaction has no outputs" -> Agda.ReqDecEmptyOutputs
  Continue [DecommitRecorded{}] [] -> Agda.ReqDecRecorded
  Continue [DecommitRecorded{}] [NetworkEffect ReqSn{}] -> Agda.ReqDecRequested
  other -> error $ "outside bounded ReqDec model: " <> show other

projectRecover :: Outcome SimpleTx -> Agda.HsRecoverOutcome
projectRecover = \case
  Error (RequireFailed NoMatchingDeposit) -> Agda.RecoveryMissing
  Error (RequireFailed RecoverBlockedByFinalizedCommit{}) -> Agda.RecoveryBlocked
  Continue [] [OnChainEffect{}] -> Agda.RecoveryPosted
  other -> error $ "outside bounded Recover model: " <> show other
