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

import Data.Map.Strict qualified as Map
import Hydra.HeadLogic (
  ClosedState (parameters),
  CoordinatedHeadState (confirmedSnapshot, decommitTx),
  HeadState (Closed, FanoutProgress, Idle, Open),
  Input (NetworkInput),
  LogicError (RequireFailed),
  OpenState (coordinatedHeadState, headId, parameters),
  Outcome (Continue, Error),
  PartialFanoutState (parameters),
  RequirementFailure (InvalidMultisignature),
  StateChanged (DepositRecovered, SnapshotConfirmed),
  aggregateState,
  ruleEnvironment,
  update,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.ReqTx qualified as ReqTx
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network.Message (Message (ReqDec), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (NodeState (..), initialChainTime, trackedFromPending)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Snapshot (Snapshot (headId), getSnapshot)
import Test.Hydra.HeadLogic.CoordinatedAudit qualified as Audit
import Test.Hydra.HeadLogic.Crem.Comparison (Example (..), Reference (..), compareExample, comparePrograms, examples)
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
         in isNothing st.coordinatedHeadState.decommitTx ==> counterexample (show result) (isRight result)

  prop "dependent transactions, expiry and duplicates agree at every input prefix" $
    forAll (vectorOf 20 $ (,) <$> elements [Scenario.transactionOne, Scenario.transactionTwo] <*> elements [0, 1]) $ \inputs ->
      let result = foldlM compareInput Scenario.initialNode inputs
       in counterexample (show result) $ isRight result
 where
  -- The refactor commit compares the full domain. These behavior changes have
  -- independent sequence/audit tests; keep exact legacy comparisons elsewhere.
  compatibleWithLegacy :: NodeState SimpleTx -> Input SimpleTx -> Bool
  compatibleWithLegacy node input = case node.headState of
    Open os ->
      isNothing os.coordinatedHeadState.decommitTx && case input of
        NetworkInput _ (ReceivedMessage _ (ReqDec tx)) -> Audit.newDecommitReady os.coordinatedHeadState tx
        _ -> True
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
