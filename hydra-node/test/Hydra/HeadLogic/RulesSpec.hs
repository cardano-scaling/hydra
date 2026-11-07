{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.RulesSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude (
  Arbitrary (arbitrary),
  Spec,
  describe,
  expectationFailure,
  it,
  prop,
  shouldBe,
  shouldSatisfy,
 )

import Crem.Inspect (checkStates, inspectStates)
import Crem.StateInspection (StateCheck (NotInspected))
import Crem.StateMachine qualified as Crem
import Data.Map.Strict qualified as Map
import Hydra.HeadLogic (
  ClosedState (parameters),
  CoordinatedHeadState (confirmedSnapshot, decommitTx, seenSnapshot, version),
  HeadState (Closed, FanoutProgress, Idle, Open),
  Input (NetworkInput),
  LogicError (RequireFailed),
  OpenState (coordinatedHeadState, headId, parameters),
  Outcome (Continue, Error),
  PartialFanoutState (parameters),
  RequirementFailure (InvalidMultisignature),
  SeenSnapshot (SeenSnapshot),
  StateChanged (DepositRecovered, SnapshotConfirmed),
  aggregateState,
  ruleEnvironment,
  update,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Decommit qualified as Decommit
import Hydra.HeadLogic.Rules.Input qualified as InputRule
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Rules.ReqTx qualified as ReqTx
import Hydra.HeadLogic.State.Operations (settlingDecommit)
import Hydra.HeadLogic.Trace (TraceStep (Decided))
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network.Message (Message (ReqDec, ReqSn, decommitTx), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (ChainPointTime (currentSlot), NodeState (..), initialChainTime, trackedFromPending)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Snapshot (Snapshot (headId, utxoToDecommit), getSnapshot)
import Test.Hydra.HeadLogic.CoordinatedAudit qualified as Audit
import Test.Hydra.HeadLogic.Crem.BeforeApi.Decommit qualified as BeforeDecommit
import Test.Hydra.HeadLogic.Crem.BeforeApi.ReqDec qualified as BeforeReqDec
import Test.Hydra.HeadLogic.Crem.BeforeApi.ReqTx qualified as BeforeReqTx
import Test.Hydra.HeadLogic.Crem.BeforeApi.Workflow qualified as BeforeWorkflow
import Test.Hydra.HeadLogic.Crem.Comparison (Example (..), Reference (..), apiExamples, compareExample, comparePrograms, compareProgramsExactly, examples)
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.Input ()
import Test.Hydra.HeadLogic.InputComparison qualified as Inputs
import Test.Hydra.HeadLogic.LegacyPrograms qualified as LegacyPrograms
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Ledger.Simple ()
import Test.Hydra.Node.State ()
import Test.QuickCheck (counterexample, elements, forAll, resize, vectorOf, withMaxSuccess, (==>))

spec :: Spec
spec = describe "free programs and crem phases" $ do
  describe "local Crem API" $ do
    forM_ [(Scenario.transactionOne, "CheckDeposits"), (Scenario.transactionTwo, "RetryOrInvalidate")] $ \(tx, phase) ->
      it ("inspects the ledger-selected phase: " <> toString phase) $ do
        let before = Scenario.initialNode
            machine = Crem.Executable $ ReqTx.machine (ruleEnvironment $ Scenario.environment 0) before.chainPointTime (Scenario.openState before) 1 mempty tx
            program = do
              (_, outputCheck) <- Crem.run machine ()
              (_, selected) <- Crem.run outputCheck ()
              void $ Program.decide "selected phase" (inspectStates selected == [([], Just phase)])
            (_, steps) = Interpreter.traceProgram simpleLedger program
        steps `shouldSatisfy` elem (Decided "selected phase" True)
        checks <- checkStates [] machine
        checks `shouldSatisfy` \case
          [([], NotInspected)] -> True
          _ -> False

    forM_ apiExamples $ \(Example name before config baseline program _) ->
      it (toString name <> ": preserves every instruction and outcome") $
        case baseline of
          FrozenProgram reference -> compareProgramsExactly config before reference program `shouldSatisfy` isRight
          FrozenOutcome{} -> expectationFailure "Crem API comparison requires an instruction baseline"

    it "preserves the complete input program when dropping rendering constraints" $
      forM_ Inputs.examples $ \(name, before, input) -> do
        let env = Scenario.environment 0
            machine = InputRule.machine (ruleEnvironment env) initialChainTime before input
            result = compareProgramsExactly (Interpreter.snapshotConfig env simpleLedger) before (BeforeWorkflow.run machine) (Workflow.run machine)
        either (expectationFailure . toString . ((name <> ": ") <>)) (const $ pure ()) result

    prop "Decommit and ReqDec preserve complete outcomes and traces" $
      withMaxSuccess 300 $
        forAll ((,,,) <$> resize 5 arbitrary <*> elements [0, 1, 5] <*> resize 5 arbitrary <*> resize 5 arbitrary) $
          \(open, ttl, pending, tx) ->
            let st = open{parameters = open.parameters{parties = Scenario.scenarioParties}} :: OpenState SimpleTx
                before = Scenario.initialNode{headState = Open st, deposits = trackedFromPending pending}
                env = ruleEnvironment $ Scenario.environment 0
                compareRule = compareProgramsExactly (Interpreter.ledgerConfig simpleLedger) before
                result = do
                  void $
                    compareRule
                      (BeforeWorkflow.run $ BeforeDecommit.machine st.headId before.chainPointTime.currentSlot st.coordinatedHeadState tx)
                      (Workflow.run $ Decommit.machine st.headId before.chainPointTime.currentSlot st.coordinatedHeadState tx)
                  compareRule
                    (BeforeWorkflow.run $ BeforeReqDec.machine env ttl before.chainPointTime pending st tx)
                    (Workflow.run $ ReqDec.machine env ttl before.chainPointTime pending st tx)
             in counterexample (show result) $ isRight result

    prop "ReqTx preserves complete outcomes and traces including decommit state" $
      withMaxSuccess 1000 $
        forAll ((,,,,) <$> elements [0, 1, 2] <*> resize 5 arbitrary <*> elements [0, 1, 5] <*> resize 5 arbitrary <*> resize 5 arbitrary) $
          \(actor, open, ttl, pending, tx) ->
            let st = open{parameters = open.parameters{parties = Scenario.scenarioParties}} :: OpenState SimpleTx
                before = Scenario.initialNode{headState = Open st, deposits = trackedFromPending pending}
                env = ruleEnvironment $ Scenario.environment actor
                result =
                  compareProgramsExactly
                    (Interpreter.ledgerConfig simpleLedger)
                    before
                    (BeforeWorkflow.run $ BeforeReqTx.machine env before.chainPointTime st ttl pending tx)
                    (Workflow.run $ ReqTx.machine env before.chainPointTime st ttl pending tx)
             in counterexample (show result) $ isRight result

    prop "dependencies, expiry and duplicates preserve every prefix" $
      forAll (vectorOf 20 $ (,) <$> elements [Scenario.transactionOne, Scenario.transactionTwo] <*> elements [0, 1]) $ \inputs ->
        let result = foldlM compareApiInput Scenario.initialNode inputs
         in counterexample (show result) $ isRight result

  forM_ examples $ \fixture@(Example name _ _ _ _ _) ->
    it (toString name <> ": agrees with its frozen baseline") $
      compareExample fixture `shouldSatisfy` isRight

  describe "complete input workflow" $ do
    forM_ Inputs.examples $ \(name, before, input) ->
      it (toString name <> ": preserves outcomes, replay and state") $
        Inputs.compareInput (Scenario.environment 0) initialChainTime before input `shouldSatisfy` isRight

    prop "bounded inputs with consistent head IDs outside corrected decommit paths agree with frozen handlers" $
      withMaxSuccess 1000 $
        forAll ((,) <$> resize 5 arbitrary <*> resize 5 arbitrary) $ \(node, input) ->
          let result = Inputs.compareInput (Scenario.environment 0) initialChainTime (withParties node) input
           in compatibleWithLegacy node input ==> counterexample (show result) (isRight result)

    prop "scheduled inputs agree at every prefix" $
      forAll (vectorOf 20 $ elements [input | (_, _, input) <- Inputs.examples]) $ \inputs ->
        let result = foldlM compareFullInput Scenario.initialNode inputs
         in counterexample (show result) (isRight result)

    it "removes a previous head deposit after its recovery is observed" $ do
      let witnesses = [(before, input) | ("recover-previous-head", before, input) <- Inputs.examples]
      length witnesses `shouldBe` 1
      forM_ witnesses $ \(before, input) -> do
        let outcome = update (Scenario.environment 0) simpleLedger initialChainTime before input
        outcome `shouldSatisfy` \case
          Continue [DepositRecovered{}] [] -> True
          _ -> False
        Map.member 42 (aggregateState before outcome).pendingDeposits `shouldBe` False

  it "crem typing does not make a dishonest verification reply safe" $ do
    let selected = [fixture | fixture@(Example "ack-invalid" _ _ FrozenProgram{} _ _) <- examples]
    length selected `shouldBe` 1
    forM_ selected $ \case
      Example _ before config (FrozenProgram free) crem _ -> do
        Interpreter.runProgramWith config crem `shouldSatisfy` \case
          Error (RequireFailed InvalidMultisignature{}) -> True
          _ -> False
        let faulty = Debug.bypassVerification crem
        comparePrograms config before (Debug.bypassVerification free) faulty `shouldSatisfy` isRight
        Interpreter.runProgramWith config faulty `shouldSatisfy` \case
          Continue events _ -> any (\case SnapshotConfirmed{} -> True; _ -> False) events
          _ -> False
      _ -> expectationFailure "invalid acknowledgement example has no free-program baseline"

  prop "ReqTx without pending decommits agrees for bounded states with a consistent head ID" $
    forAll ((,,,,) <$> elements [0, 1, 2] <*> resize 5 arbitrary <*> elements [0, 1, 5] <*> resize 5 arbitrary <*> resize 5 arbitrary) $
      \(actor, open, ttl, pending, tx) ->
        let st = open{headId = (getSnapshot open.coordinatedHeadState.confirmedSnapshot).headId, parameters = open.parameters{parties = Scenario.scenarioParties}} :: OpenState SimpleTx
            before = Scenario.initialNode{headState = Open st, deposits = trackedFromPending pending}
            env = ruleEnvironment $ Scenario.environment actor
            result =
              comparePrograms
                (Interpreter.ledgerConfig simpleLedger)
                before
                (LegacyPrograms.onOpenNetworkReqTxProgram env before.chainPointTime st ttl pending tx)
                (Workflow.run $ ReqTx.machine env before.chainPointTime st ttl pending tx)
         in unchangedDecommitDomain st.coordinatedHeadState ==> counterexample (show result) (isRight result)

  prop "dependent transactions, expiry and duplicates agree at every input prefix" $
    forAll (vectorOf 20 $ (,) <$> elements [Scenario.transactionOne, Scenario.transactionTwo] <*> elements [0, 1]) $ \inputs ->
      let result = foldlM compareInput Scenario.initialNode inputs
       in counterexample (show result) $ isRight result
 where
  compareApiInput before (tx, ttl) = do
    let st = Scenario.openState before
        env = ruleEnvironment $ Scenario.environment 0
        reference = BeforeWorkflow.run $ BeforeReqTx.machine env before.chainPointTime st ttl mempty tx
        actual = Workflow.run $ ReqTx.machine env before.chainPointTime st ttl mempty tx
    (outcome, _) <- compareProgramsExactly (Interpreter.ledgerConfig simpleLedger) before reference actual
    pure $ aggregateState before outcome

  -- The refactor commit compares the full domain. These behavior changes have
  -- independent sequence/audit tests; keep exact legacy comparisons elsewhere.
  compatibleWithLegacy :: NodeState SimpleTx -> Input SimpleTx -> Bool
  compatibleWithLegacy node input = case node.headState of
    Open os ->
      unchangedDecommitDomain os.coordinatedHeadState && case input of
        NetworkInput _ (ReceivedMessage _ (ReqDec tx)) -> Audit.newDecommitReady os.coordinatedHeadState tx
        NetworkInput _ (ReceivedMessage _ ReqSn{decommitTx}) -> isNothing decommitTx
        _ -> True
    _ -> True
  unchangedDecommitDomain :: CoordinatedHeadState SimpleTx -> Bool
  unchangedDecommitDomain chs =
    isNothing chs.decommitTx
      && not (settlingDecommit chs.version chs.confirmedSnapshot)
      && case chs.seenSnapshot of
        SeenSnapshot snapshot _ _ -> isNothing snapshot.utxoToDecommit
        _ -> True
  withParties :: NodeState SimpleTx -> NodeState SimpleTx
  withParties node =
    node
      { headState = case node.headState of
          Idle st -> Idle st
          Open st -> Open st{headId = (getSnapshot st.coordinatedHeadState.confirmedSnapshot).headId, parameters = st.parameters{parties = Scenario.scenarioParties}}
          Closed st -> Closed st{parameters = st.parameters{parties = Scenario.scenarioParties}}
          FanoutProgress st -> FanoutProgress st{parameters = st.parameters{parties = Scenario.scenarioParties}}
      }
  compareFullInput before input = do
    (outcome, _) <- Inputs.compareInput (Scenario.environment 0) initialChainTime before input
    pure $ aggregateState before outcome
  compareInput before (tx, ttl) = do
    let st = Scenario.openState before
        env = ruleEnvironment $ Scenario.environment 0
        free = LegacyPrograms.onOpenNetworkReqTxProgram env before.chainPointTime st ttl mempty tx
        crem = Workflow.run $ ReqTx.machine env before.chainPointTime st ttl mempty tx
    (outcome, _) <- comparePrograms (Interpreter.ledgerConfig simpleLedger) before free crem
    pure $ aggregateState before outcome
