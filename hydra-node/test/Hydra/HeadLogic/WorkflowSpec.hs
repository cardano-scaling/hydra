{-# LANGUAGE DataKinds #-}

module Hydra.HeadLogic.WorkflowSpec (spec) where

import Crem.Topology (Topology (Topology))
import Hydra.HeadLogic.Interpreter (runProgram)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.HeadLogic (
  LogicError (AssertionFailed),
  Outcome (Continue, Error, Wait),
  StateChanged (TransactionReceived),
  WaitReason (WaitOnSnapshotNumber),
 )
import Hydra.Prelude hiding (State, state)
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.Ledger.Simple (aValidTx)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe)

data Phase = Deciding | Applying | Completed

type TopologyOf =
  'Topology
    '[ '( 'Deciding, '[ 'Applying])
     , '( 'Applying, '[ 'Completed])
     ]

data State (phase :: Phase) where
  Decide :: State 'Deciding
  Apply :: State 'Applying
  Complete :: State 'Completed

spec :: Spec
spec = describe "workflow execution" $ do
  it "uses primitive replies to advance to completion" $ do
    let ledger :: Ledger SimpleTx
        ledger = Ledger $ \_ utxo _ -> Right utxo
    runProgram (testInterpreterConfig ledger) (Workflow.run $ example True) `shouldBe` Continue [TransactionReceived transaction] []
  it "suspends without evaluating a later primitive" $ do
    let ledger :: Ledger SimpleTx
        ledger = Ledger $ \_ _ _ -> error "unreachable ledger query"
    runProgram (testInterpreterConfig ledger) (Workflow.run $ example False) `shouldBe` Wait (WaitOnSnapshotNumber 1) []
  it "rejects through Program before a later workflow phase" $ do
    let ledger :: Ledger SimpleTx
        ledger = Ledger $ \_ _ _ -> Left (transaction, ValidationError "controlled rejection")
    runProgram (testInterpreterConfig ledger) (Workflow.run $ example True) `shouldBe` Error (AssertionFailed "ledger rejected")

example :: Bool -> Workflow.Workflow SimpleTx TopologyOf
example eligible = Workflow.workflow describePhase Decide step
 where
  describePhase :: State phase -> Text
  describePhase = \case
    Decide -> "deciding"
    Apply -> "applying"
    Complete -> "completed"

  step :: State phase -> Workflow.Transition SimpleTx TopologyOf State phase
  step = \case
    Decide -> Workflow.choose $ do
      proceed <- Program.decide "eligible" eligible
      if proceed
        then pure $ Workflow.next Apply
        else Program.suspend $ WaitOnSnapshotNumber 1
    Apply -> Workflow.continue (const Complete) $ do
      result <- Program.applyTransaction 0 mempty transaction
      either
        (const $ Program.reject $ AssertionFailed "ledger rejected")
        (const $ Program.change $ TransactionReceived transaction)
        result
    Complete -> Workflow.finish Complete

transaction :: SimpleTx
transaction = aValidTx 42
