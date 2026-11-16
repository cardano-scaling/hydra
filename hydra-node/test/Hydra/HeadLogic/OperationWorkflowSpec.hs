{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.OperationWorkflowSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.API.ServerOutput qualified as Server
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.Chain (ChainEvent (..), OnChainTx (..))
import Hydra.HeadLogic.Error (LogicError (AssertionFailed))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Chain.Deposits qualified as Deposits
import Hydra.HeadLogic.Rules.Network.AckSn qualified as AckSn
import Hydra.HeadLogic.Trace (TraceStep (..))
import Hydra.HeadLogicSpec (withAccumulators)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Node.Environment (Environment (depositActivation, depositPeriod))
import Hydra.Node.HeadLogic (CoordinatedHeadState (..), Effect (..), Input (..), OpenState (..), Outcome (..), StateChanged (..), aggregateState, mkSeenSnapshot, ruleEnvironment, updateProgram)
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..), initialChainTime, trackedFromPending)
import Hydra.Prelude hiding (label, state, trace)
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.DepositPeriod qualified as DepositPeriod
import Hydra.Tx.Snapshot (Snapshot (..))
import Test.Hydra.HeadLogic.InputFixtures qualified as Inputs
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)

spec :: Spec
spec = describe "operation workflow boundaries" $ do
  it "records fanout intent before posting and rejects an empty selection without intent" $ do
    forM_ ["fanout-request", "fanout-selection"] $ \name -> do
      (_, trace) <- fixture name
      signals trace `shouldBe` [Changed "head fanout initiated", Emitted "post full fanout"]
    (refused, trace) <- fixture "fanout-empty-selection"
    signals trace `shouldBe` [Emitted "client effect"]
    refused `shouldSatisfy` \case Continue [] [ClientEffect Server.CommandFailed{}] -> True; _ -> False

  it "verifies and adopts before clearing local work, reporting invalid side-loads as client refusals" $ do
    (_, trace) <- fixture "side-load-current"
    signals trace `shouldBe` [PrimitiveCalled "verify multisignature", Changed "snapshot confirmed", Changed "local state cleared"]
    (refused, invalidTrace) <- fixture "side-load-invalid-signatures"
    signals invalidTrace `shouldBe` [PrimitiveCalled "verify multisignature", Emitted "client effect"]
    refused `shouldSatisfy` \case Continue [] [ClientEffect Server.SideLoadSnapshotRejected{}] -> True; _ -> False

  it "retains close and contest observations after failed certificate verification" $
    forM_ [("close-adopt", "head closed"), ("contest-adopt", "head contested")] $ \(name, event) -> do
      (before, input) <- example name
      let original = run before input
      signals (snd original) `shouldBe` [PrimitiveCalled "verify multisignature", Changed "snapshot confirmed", Changed event]
      invalid <- case input of
        ChainInput Observation{observedTx = OnCloseTx hid sn deadline _, newChainState} -> pure $ ChainInput $ Observation (OnCloseTx hid sn deadline $ aggregate []) newChainState
        ChainInput Observation{observedTx = OnContestTx hid sn deadline _, newChainState} -> pure $ ChainInput $ Observation (OnContestTx hid sn deadline $ aggregate []) newChainState
        _ -> fail "expected a close or contest observation"
      let (outcome, trace) = run before invalid
      signals trace `shouldBe` [PrimitiveCalled "verify multisignature", Changed event]
      outcome `shouldSatisfy` \case Continue [_] [] -> True; _ -> False

  it "publishes the incremental effect after confirmation and before requesting the next round" $ do
    let tx = Scenario.transactionOne
        snapshot = withAccumulators $ Scenario.initialSnapshot{number = 1, utxoToCommit = Just (utxoRef 42), depositTxId = Just 42}
        base = Scenario.openState Scenario.initialNode
        state = base{coordinatedHeadState = base.coordinatedHeadState{seenSnapshot = mkSeenSnapshot snapshot $ Map.fromList [(alice, sign aliceSk snapshot), (bob, sign bobSk snapshot)], localUTxO = utxoRef 2, localTxs = Seq.singleton tx, allTxs = Map.singleton 1 tx, currentDepositTxId = Just 42}}
        pending = Map.singleton 42 $ Deposit base.headId (utxoRef 42) initialChainTime (addUTCTime 10000 initialChainTime) Active
        program = AckSn.handle (ruleEnvironment $ Scenario.environment 1) Scenario.initialNode.chainPointTime pending state carol (sign carolSk snapshot) 1
        (outcome, trace) = Interpreter.traceProgram (testInterpreterConfig simpleLedger) program
    outcome `shouldSatisfy` \case Continue [PartySignedSnapshot{}, SnapshotConfirmed{}, CommitApproved{}, SnapshotRequestDecided{}] [OnChainEffect{}, NetworkEffect ReqSn{}] -> True; _ -> False
    signals trace `shouldBe` [Changed "party signed snapshot", PrimitiveCalled "verify multisignature", Changed "snapshot confirmed", Changed "commit approved", Emitted "post increment", Changed "snapshot request decided", Emitted "broadcast ReqSn"]

  it "returns the same deposit statuses that replay retains, including backwards time" $
    forM_ [(old, computed) | old <- [Inactive, Active, Expired], computed <- [Inactive, Active, Expired]] $ \(old, computed) -> do
      let now = initialChainTime
          activation = DepositPeriod.toNominalDiffTime env.depositActivation
          expiry = DepositPeriod.toNominalDiffTime env.depositPeriod
          created = addUTCTime (if computed == Active then negate activation - 1 else 0) now
          deadline = addUTCTime (if computed == Expired then expiry - 1 else expiry + 100) now
          deposit = Deposit open.headId (utxoRef 7) created deadline old
          before = Scenario.initialNode{deposits = trackedFromPending $ Map.singleton 7 deposit}
          expected = Agda.effectiveDepositStatusRef (statusRef old) (statusRef computed)
          program = do
            effective <- Deposits.onChainTick (ruleEnvironment env) before.pendingDeposits now
            unless ((statusRef . (.status) <$> Map.lookup 7 effective) == Just expected) $
              Program.reject $
                AssertionFailed "tick result differs from the retained status projection"
          outcome = Interpreter.runProgram (testInterpreterConfig simpleLedger) program
      outcome `shouldSatisfy` \case Continue _ [] -> True; _ -> False
      (statusRef . (.status) <$> Map.lookup 7 (aggregateState before outcome).pendingDeposits) `shouldBe` Just expected

  it "uses a retained active status when selecting work after time moves backwards" $ do
    let deposit = Deposit open.headId (utxoRef 7) initialChainTime (addUTCTime 10000 initialChainTime) Active
        before = Scenario.initialNode{deposits = trackedFromPending $ Map.singleton 7 deposit}
        (outcome, _) = run before $ ChainInput $ Tick initialChainTime 0
    outcome `shouldSatisfy` \case
      Continue [TickObserved{}, SnapshotRequestDecided{}] [NetworkEffect (ReqSn _ _ _ Nothing (Just 7))] -> True
      _ -> False
 where
  env = Scenario.environment 0
  open = Scenario.openState Scenario.initialNode

example :: Text -> IO (NodeState SimpleTx, Input SimpleTx)
example name = case [(before, input) | (label, before, input) <- Inputs.examples, name == label] of
  [value] -> pure value
  _ -> fail $ "missing or ambiguous input fixture: " <> toString name

fixture :: Text -> IO (Outcome SimpleTx, [TraceStep])
fixture name = do
  (before, input) <- example name
  either (fail . toString) pure $ Inputs.checkInput (Scenario.environment 0) initialChainTime before input

run :: NodeState SimpleTx -> Input SimpleTx -> (Outcome SimpleTx, [TraceStep])
run before input = Interpreter.traceProgram (testInterpreterConfig simpleLedger) $ updateProgram (ruleEnvironment $ Scenario.environment 0) initialChainTime before input

signals :: [TraceStep] -> [TraceStep]
signals = filter $ \case Changed{} -> True; Emitted{} -> True; PrimitiveCalled{} -> True; _ -> False

statusRef :: DepositStatus -> Agda.HsDepositStatus
statusRef = \case Inactive -> Agda.InactiveS; Active -> Agda.ActiveS; Expired -> Agda.ExpiredS
