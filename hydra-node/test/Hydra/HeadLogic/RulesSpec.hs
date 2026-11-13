{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.RulesSpec (spec) where

import Hydra.Prelude
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.Prelude (Spec, describe, it, prop, shouldBe, shouldSatisfy)

import Crem.Inspect (checkStates, inspectStates)
import Crem.StateInspection.NoThunks (StateCheck (NotInspected))
import Crem.StateMachine qualified as Crem
import Data.Map.Strict qualified as Map
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.ReqTx qualified as ReqTx
import Hydra.HeadLogic.Trace (TraceStep (Decided))
import Hydra.Ledger.Simple (simpleLedger)
import Hydra.Network.Message (Message (ReqTx), NetworkEvent (ReceivedMessage))
import Hydra.Node.Environment (Environment (party))
import Hydra.Node.HeadLogic (Input (NetworkInput), Outcome (Continue), StateChanged (DepositRecovered), aggregateState, ruleEnvironment, update)
import Hydra.Node.State (NodeState (chainPointTime), initialChainTime)
import Test.Hydra.HeadLogic.InputFixtures qualified as Inputs
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.QuickCheck (counterexample, elements, forAll, vectorOf)

spec :: Spec
spec = describe "free programs and crem phases" $ do
  describe "workflow inspection" $ do
    forM_ [(Scenario.transactionOne, "CheckDeposits"), (Scenario.transactionTwo, "RetryOrInvalidate")] $ \(tx, phase) ->
      it ("inspects the ledger-selected phase: " <> toString phase) $ do
        let before = Scenario.initialNode
            machine = Crem.Executable $ ReqTx.machine (ruleEnvironment $ Scenario.environment 0) before.chainPointTime (Scenario.openState before) 1 mempty tx
            program = do
              (_, outputCheck) <- Crem.run machine ()
              (_, selected) <- Crem.run outputCheck ()
              void $ Program.decide "selected phase" (inspectStates selected == [([], Just phase)])
            (_, steps) = Interpreter.traceProgram (testInterpreterConfig simpleLedger) program
        steps `shouldSatisfy` elem (Decided "selected phase" True)
        checks <- checkStates [] machine
        checks `shouldSatisfy` \case
          [([], NotInspected)] -> True
          _ -> False

  describe "complete input workflow" $ do
    forM_ Inputs.examples $ \(name, before, input) ->
      it (toString name <> ": preserves execution, event replay and checkpoint consistency") $
        Inputs.checkInput (Scenario.environment 0) initialChainTime before input `shouldSatisfy` isRight

    prop "scheduled inputs preserve execution and replay consistency at every prefix" $
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

  prop "dependent transactions, expiry and duplicates preserve replay consistency" $
    forAll (vectorOf 20 $ (,) <$> elements [Scenario.transactionOne, Scenario.transactionTwo] <*> elements [0, 1]) $ \inputs ->
      let result = foldlM compareInput Scenario.initialNode inputs
       in counterexample (show result) $ isRight result
 where
  compareFullInput before input = do
    (outcome, _) <- Inputs.checkInput (Scenario.environment 0) initialChainTime before input
    pure $ aggregateState before outcome
  compareInput before (tx, ttl) =
    compareFullInput before $ NetworkInput ttl $ ReceivedMessage (Scenario.environment 0).party $ ReqTx tx
