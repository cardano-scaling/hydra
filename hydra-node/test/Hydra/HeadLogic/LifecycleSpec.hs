{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.LifecycleSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude (Expectation, Spec, arbitrary, describe, generateWith, it, shouldBe)

import Cardano.Binary (decodeFull', serialize')
import Control.Monad (foldM)
import Crem.Inspect (Component (LeftComponent, RightComponent), inspectStates)
import Crem.StateMachine qualified as Crem
import Crem.Topology (Topology (Topology))
import Data.List qualified as List
import Hydra.API.ClientInput qualified as Client
import Hydra.Chain (ChainEvent (PostTxError, Tick), OnChainTx (OnContestTx, OnInitTx, OnPartialFanoutTx), PostChainTx (FinalPartialFanoutTx, InitTx), PostTxError (FailedToConstructFanoutTx))
import Hydra.HeadLogic.Input qualified as Input
import Hydra.HeadLogic.Outcome (Effect (OnChainEffect), Outcome (Continue), StateChanged (..))
import Hydra.HeadLogic.State (ClosedState (..), FanoutMode (..), FinalizedState (..), HeadState (..), OnChainFanoutDatum (..), OpenState (..), PartialFanoutState (..), headIdOf)
import Hydra.HeadLogic.State.Head qualified as Head
import Hydra.HeadLogic.State.Node qualified as Machine
import Hydra.HeadLogic.State.Types (DepositOutput (..), HeadOutput (..))
import Hydra.HeadLogicSpec (inClosedState', inIdleState, inOpenState, observeTx, testSnapshot)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx, simpleLedger)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.HeadLogic (Input (ClientInput), aggregateState, update)
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Tx (UTxOType)
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.HeadId (HeadId (UnsafeHeadId))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot)
import Test.Hydra.Ledger.Simple (utxoRef, utxoRefs)
import Test.Hydra.Node.Fixture (testEnvironment)
import Test.Hydra.Tx.Fixture (alice, aliceSk, testHeadId)

spec :: Spec
spec = describe "head lifecycle" $ do
  forM_ [("Open", "Open", opened), ("Closed", "Closed", closed), ("Closed with pending fanout", "Closed", pending), ("PartialFanout", "PartialFanout", partial), ("Finalized", "Finalized", finalized)] $ \(name, expected, before) ->
    it ("does not reopen or reset " <> name <> " from a retained HeadOpened") $ do
      let after = Machine.apply before openEvent
      after `shouldBe` before
      phase after `shouldBe` Just expected

  forM_ [("automatic", initiated), ("selective", selected)] $ \(name, intent) -> do
    it ("cancels a failed " <> name <> " request before any distribution is observed") $ do
      checkSteps closed [(intent, "Closed"), (reverted, "Closed")]
      let cancelled = Machine.applyEvents closed [intent, reverted]
      case cancelled.headState of
        Closed{} -> cancelled `shouldBe` closed
        _ -> fail "expected a closed head after cancelling pending fanout"

    it ("records partial distribution after a pending " <> name <> " request") $
      checkSteps closed [(intent, "Closed"), (observed AwaitingSelection, "PartialFanout"), (finished, "Finalized")]

  forM_ [("automatic", Client.Fanout), ("selective", Client.PartialFanout $ utxoRef 1)] $ \(name, command) -> do
    it ("accepts a contest and adopts its snapshot during pending " <> name <> " fanout") $ do
      let before = closedWithCandidate False
          requested = applyInput before $ ClientInput command
          signatures = aggregate [sign aliceSk candidate]
          deadline = addUTCTime 1200 initialChainTime
          contested = applyInput requested $ observeTx $ OnContestTx testHeadId 2 deadline signatures
      case contested.headState of
        Closed st -> do
          st.confirmedSnapshot `shouldBe` ConfirmedSnapshot candidate signatures
          st.unconfirmedSnapshot `shouldBe` Nothing
          st.contestationDeadline `shouldBe` deadline
        _ -> fail "a contest cancels stale local fanout work while the Head remains Closed"
      restored <- either (fail . show) pure $ decodeFull' $ serialize' requested
      applyInput restored (observeTx $ OnContestTx testHeadId 2 deadline signatures) `shouldBe` contested

    it ("ignores a pre-contest failure after a new " <> name <> " fanout request") $ do
      let before = closedWithCandidate False
          oldRequest = update testEnvironment simpleLedger initialChainTime before $ ClientInput command
          pendingFirst = aggregateState before oldRequest
          signatures = aggregate [sign aliceSk candidate]
          contested = applyInput pendingFirst $ observeTx $ OnContestTx testHeadId 2 (addUTCTime 1200 initialChainTime) signatures
          newRequest = update testEnvironment simpleLedger initialChainTime contested $ ClientInput command
          pendingSecond = aggregateState contested newRequest
      case (oldRequest, newRequest) of
        (Continue _ [OnChainEffect oldTx], Continue _ [OnChainEffect _]) ->
          applyInput pendingSecond (Input.ChainInput $ PostTxError oldTx FailedToConstructFanoutTx Nothing) `shouldBe` pendingSecond
        _ -> fail "expected fanout requests before and after the contest"

    forM_ [False, True] $ \ready ->
      it ("preserves the candidate and readiness after a failed " <> name <> " fanout, ready=" <> show ready) $ do
        let before = closedWithCandidate ready
            outcome = update testEnvironment simpleLedger initialChainTime before $ ClientInput command
            requested = aggregateState before outcome
        case outcome of
          Continue _ [OnChainEffect tx] -> do
            restored <- either (fail . show) pure $ decodeFull' $ serialize' requested
            let cancelled = applyInput restored $ Input.ChainInput $ PostTxError tx FailedToConstructFanoutTx Nothing
            cancelled `shouldBe` before
            let tick = Input.ChainInput $ Tick (addUTCTime 601 initialChainTime) 1
                notified = applyInput cancelled tick
            case notified.headState of
              Closed st -> st.readyToFanoutSent `shouldBe` True
              _ -> fail "expected Closed after the deadline tick"
          _ -> fail $ "expected a pending fanout transaction, got " <> show outcome

    it ("notifies readiness while " <> name <> " fanout is pending") $ do
      let before = applyInput (closedWithCandidate False) $ ClientInput command
          outcome = update testEnvironment simpleLedger initialChainTime before $ Input.ChainInput $ Tick (addUTCTime 601 initialChainTime) 1
          after = aggregateState before outcome
          cancelled = Machine.apply after reverted
      case outcome of
        Continue changes _ -> filter (\case HeadIsReadyToFanout{} -> True; _ -> False) changes `shouldBe` [HeadIsReadyToFanout testHeadId]
        _ -> fail "expected a readiness notification while pending"
      case cancelled.headState of
        Closed st -> st.readyToFanoutSent `shouldBe` True
        _ -> fail "expected Closed after cancelling the request"

  it "observes another party's partial fanout without a local request" $ do
    checkSteps closed [(observed AwaitingSelection, "PartialFanout"), (finished, "Finalized")]
    checkProgress partial (utxoRef 1) (utxoRefs [2, 3]) AwaitingSelection

  forM_ [("observer", closed, AwaitingSelection), ("pending request", pending, AutoDrain)] $ \(name, initial, expectedMode) ->
    it ("retains observed fanout with no locally matching outputs after " <> name) $ do
      let observation = observeTx $ OnPartialFanoutTx testHeadId (utxoRef 999)
          observedNode = applyInput initial observation
      restored <- either (fail . show) pure $ decodeFull' $ serialize' observedNode
      restored `shouldBe` observedNode
      phase restored `shouldBe` Just "PartialFanout"
      checkProgress restored mempty (utxoRefs [1, 2, 3]) expectedMode
      case restored.headState of
        FanoutProgress st -> st.fanoutDatum `shouldBe` DatumFanoutProgress
        _ -> fail "expected observed fanout progress"
      let selection = ClientInput $ Client.PartialFanout $ utxoRefs [1, 2, 3]
          outcome = update testEnvironment simpleLedger initialChainTime restored selection
          selectedNode = aggregateState restored outcome
      case outcome of
        Continue _ [OnChainEffect tx@FinalPartialFanoutTx{}] -> do
          let failed = applyInput selectedNode $ Input.ChainInput $ PostTxError tx FailedToConstructFanoutTx Nothing
          failed `shouldBe` selectedNode
          phase failed `shouldBe` Just "PartialFanout"
        _ -> fail $ "expected a final partial fanout transaction, got " <> show outcome
      checkSteps restored [(reverted, "PartialFanout"), (finished, "Finalized")]

  forM_ [("automatic", AutoDrain), ("selective", DistributingSelection $ utxoRef 2), ("waiting", AwaitingSelection)] $ \(name, mode) ->
    it ("does not revert observed partial fanout while " <> name) $ do
      let before = Machine.apply closed $ observed mode
      checkSteps before [(reverted, "PartialFanout")]
      Machine.apply before reverted `shouldBe` before

  it "preserves distributed outputs across selections, further observations and rollback" $ do
    let nextSelection = HeadPartialFanoutSelected testHeadId (utxoRefs [2, 3]) (utxoRef 2)
        nextObservation = HeadPartialFannedOut testHeadId (utxoRef 2) (utxoRef 3) (SimpleChainState 3) AwaitingSelection
        rolledBack = ChainRolledBack (SimpleChainState 2)
        events = [nextSelection, nextObservation, rolledBack, HeadFanoutInitiated testHeadId (utxoRef 3), finished]
    checkSteps partial $ zip events ["PartialFanout", "PartialFanout", "PartialFanout", "PartialFanout", "Finalized"]
    checkProgress (Machine.apply partial nextSelection) (utxoRef 1) (utxoRefs [2, 3]) (DistributingSelection $ utxoRef 2)
    checkProgress (Machine.applyEvents partial [nextSelection, nextObservation, rolledBack]) (utxoRefs [1, 2]) (utxoRef 3) AwaitingSelection

  it "can finish a full fanout without entering partial fanout" $ do
    checkSteps closed [(finished, "Finalized")]
    checkSteps pending [(finished, "Finalized")]

  it "updates the contest deadline without leaving Closed" $ do
    let deadline = addUTCTime 60 initialChainTime
        contested = HeadContested testHeadId (SimpleChainState 3) deadline 1
    checkSteps closed [(contested, "Closed"), (finished, "Finalized")]
    case (Machine.apply closed contested).headState of
      Closed st -> (st.contestationDeadline, st.chainState) `shouldBe` (deadline, SimpleChainState 3)
      _ -> fail "contest must preserve Closed"
    List.lookup (Head.Open, Head.Open) Head.transitionLabels `shouldBe` Just "Increment / Decrement"
    List.lookup (Head.Closed, Head.Closed) Head.transitionLabels `shouldBe` Just "Contest"
    let Topology transitions = Head.lifecycleTopology
    forM_ Head.transitionLabels $ \((from, to), _) ->
      maybe False (elem to) (List.lookup from transitions) `shouldBe` True

  it "retains the completed identity across checkpoints and rollback" $ do
    checkSteps partial [(finished, "Finalized"), (Checkpoint finalized, "Finalized"), (ChainRolledBack $ SimpleChainState 2, "Finalized")]
    headIdOf finalized.headState `shouldBe` Just testHeadId
    (Machine.apply finalized $ ChainRolledBack $ SimpleChainState 2).headState
      `shouldBe` Finalized FinalizedState{headId = testHeadId, chainState = SimpleChainState 2}

  it "does not reopen a finalized Head even for a different identity" $ do
    let input = DepositOutput{event = nextOpenEvent, deposits = mempty, settledDeposit = mempty}
        (result, next) = runIdentity $ Crem.run (Crem.Basic $ Head.machine finalized.headState) input
    result.headState `shouldBe` finalized.headState
    inspectStates next `shouldBe` [([], Just "Finalized")]

  it "starts a separate Head at the node boundary and preserves tracked deposits" $ do
    let before :: NodeState SimpleTx
        before = finalized{deposits = generateWith arbitrary 42}
        after = Machine.apply before nextOpenEvent
    checkSteps before [(nextOpenEvent, "Open")]
    headIdOf after.headState `shouldBe` Just nextHeadId
    after.deposits `shouldBe` before.deposits

  it "accepts Init and observes initialization of a different Head after finalization" $ do
    let env = testEnvironment
        initInput :: Input SimpleTx
        initInput = ClientInput Client.Init
        parameters = HeadParameters{parties = env.party : env.otherParties, contestationPeriod = env.contestationPeriod, depositPeriod = env.depositPeriod}
        input = observeTx $ OnInitTx nextHeadId "new-seed" parameters env.participants
        outcome = update env simpleLedger initialChainTime finalized input
    update env simpleLedger initialChainTime finalized initInput
      `shouldBe` Continue [] [OnChainEffect $ InitTx env.participants parameters]
    headIdOf (aggregateState finalized outcome).headState `shouldBe` Just nextHeadId
    phase (aggregateState finalized outcome) `shouldBe` Just "Open"

  it "opens from Idle and restores checkpoints independently of lifecycle edges" $ do
    checkSteps inIdleState [(openEvent, "Open")]
    checkSteps partial [(Checkpoint opened, "Open")]
    Machine.apply partial (Checkpoint opened) `shouldBe` opened

-- Exercise continuous execution and recovery at every boundary, and inspect
-- the actual Crem phase rather than a separate projection of stored fields.
checkSteps :: NodeState SimpleTx -> [(StateChanged SimpleTx, Text)] -> Expectation
checkSteps initial = void . foldM step (Machine.start initial)
 where
  step runtime (event, expected) = do
    restored <- either (fail . show) pure $ decodeFull' $ serialize' $ Machine.currentState runtime
    retained <- either (fail . show) pure $ decodeFull' $ serialize' event
    let next = Machine.step runtime event
        node = Machine.currentState next
    Machine.applyEvents initial [Checkpoint restored, retained] `shouldBe` node
    phase node `shouldBe` Just expected
    pure next

phase :: NodeState SimpleTx -> Maybe Text
phase = join . List.lookup [LeftComponent, RightComponent] . inspectStates . Machine.components

checkProgress :: NodeState SimpleTx -> UTxOType SimpleTx -> UTxOType SimpleTx -> FanoutMode SimpleTx -> Expectation
checkProgress node distributed remaining mode = case node.headState of
  FanoutProgress st -> (st.distributedOutputs, st.remainingOutputs, st.mode) `shouldBe` (distributed, remaining, mode)
  _ -> fail "expected partial fanout progress"

opened, closed, pending, partial, finalized :: NodeState SimpleTx
opened = Machine.apply (inOpenState [alice]) $ SnapshotRequestDecided 1
closed = inClosedState' [alice] $ ConfirmedSnapshot (testSnapshot 1 0 [] $ utxoRefs [1, 2, 3]) mempty
pending = Machine.apply closed initiated
partial = Machine.apply closed $ observed AwaitingSelection
finalized = Machine.apply partial finished

nextHeadId :: HeadId
nextHeadId = UnsafeHeadId "another-head"

nextOpenEvent :: StateChanged SimpleTx
nextOpenEvent = openEvent{headId = nextHeadId, headSeed = "new-seed"}

openEvent, initiated, selected, reverted, finished :: StateChanged SimpleTx
openEvent = case (inOpenState [alice]).headState of
  Open st -> HeadOpened st.parameters st.chainState st.headId st.headSeed [alice]
  _ -> error "fixture does not open a head"
initiated = HeadFanoutInitiated testHeadId $ utxoRefs [1, 2, 3]
selected = HeadPartialFanoutSelected testHeadId (utxoRefs [1, 2, 3]) (utxoRef 1)
reverted = HeadFanoutReverted testHeadId
finished = HeadFannedOut testHeadId (utxoRefs [1, 2, 3]) (SimpleChainState 4)

observed :: FanoutMode SimpleTx -> StateChanged SimpleTx
observed = HeadPartialFannedOut testHeadId (utxoRef 1) (utxoRefs [2, 3]) (SimpleChainState 2)

-- A fanout command can arrive before the deadline, while a candidate still
-- needs the signatures in a later contest to become confirmed.
closedWithCandidate :: Bool -> NodeState SimpleTx
closedWithCandidate ready = case closed.headState of
  Closed st -> closed{headState = Closed st{contestationDeadline = addUTCTime 600 initialChainTime, readyToFanoutSent = ready, unconfirmedSnapshot = Just candidate}}
  _ -> error "fixture does not close a head"

candidate :: Snapshot SimpleTx
candidate = testSnapshot 2 0 [] $ utxoRefs [1, 2, 3]

applyInput :: NodeState SimpleTx -> Input SimpleTx -> NodeState SimpleTx
applyInput node input = aggregateState node $ update testEnvironment simpleLedger initialChainTime node input
