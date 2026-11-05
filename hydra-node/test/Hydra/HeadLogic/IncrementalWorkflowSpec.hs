{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.IncrementalWorkflowSpec (spec) where

import Cardano.Binary (decodeFull', serialize')
import Data.Map.Strict qualified as Map
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic (
  CoordinatedHeadState (
    currentDepositTxId,
    decommitTx,
    localUTxO,
    seenSnapshot
  ),
  Effect (NetworkEffect, OnChainEffect),
  HeadState (Open),
  LogicError (RequireFailed),
  OpenState (coordinatedHeadState, parameters),
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
  aggregateState,
  onClientRecover,
  onOpenNetworkReqDec,
  ruleEnvironment,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..), initialChainTime, trackedFromPending)
import Hydra.Prelude
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Test.Hydra.HeadLogic.DecommitDependency qualified as DecommitDependency
import Test.Hydra.HeadLogic.LegacyIncremental qualified as Legacy
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Node.State ()
import Test.Hydra.Prelude (
  Arbitrary (arbitrary),
  Spec,
  it,
  prop,
  shouldBe,
  shouldSatisfy,
 )
import Test.Hydra.Tx.Fixture (testHeadId)
import Test.QuickCheck (counterexample, elements, forAll, resize)

spec :: Spec
spec = do
  it "characterizes the existing decommit dependency stall and reservation loss" $
    DecommitDependency.demonstration `shouldSatisfy` isRight
  prop "ReqDec preserves the frozen handler's outcomes and serialized states" $
    forAll ((,,,) <$> resize 5 arbitrary <*> elements [0, 1, 5] <*> resize 5 arbitrary <*> resize 5 arbitrary) $ \(open, ttl, pending, tx) ->
      let st = open{parameters = open.parameters{parties = Scenario.scenarioParties}} :: OpenState SimpleTx
          before = Scenario.initialNode{headState = Open st, deposits = trackedFromPending pending}
          old = Legacy.onOpenNetworkReqDec env simpleLedger ttl before.chainPointTime pending st tx
          new = onOpenNetworkReqDec env simpleLedger ttl before.chainPointTime pending st tx
          after = aggregateState before new
       in counterexample (show (old, new)) $
            old == new && after == aggregateState before old && decodeFull' (serialize' after) == Right after

  it "ReqDec agrees with bounded Agda outcomes and preserves guard priority" $
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
          (outcome, observations) = Interpreter.traceProgram ledger program
          expected = Agda.reqDecOutcomeRef commit existing applies outputs (toInteger ttl) inFlight 2 1 (toInteger actor)
      projectDecommit outcome `shouldBe` expected
      outcome `shouldBe` Legacy.onOpenNetworkReqDec environment ledger ttl Scenario.initialNode.chainPointTime pending st tx
      outcome `shouldBe` onOpenNetworkReqDec environment ledger ttl Scenario.initialNode.chainPointTime pending st tx
      length [() | Trace.LedgerApplied{} <- observations] `shouldBe` if commit || existing then 0 else 1

  it "Recover preserves missing/blocked/post outcomes across status and slot" $
    forM_ [(registered, blocked, status, slot) | registered <- [False, True], blocked <- [False, True], status <- [Inactive, Active, Expired], slot <- [0, 100]] $ \(registered, blocked, status, slot) -> do
      let pending = if registered then Map.singleton 7 (deposit status) else mempty
          finalized = if blocked then Just 7 else Nothing
          result = onClientRecover slot pending finalized 7
      result `shouldBe` Legacy.onClientRecover slot pending finalized 7
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
