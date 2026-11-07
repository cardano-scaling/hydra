{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.MachineSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude (
  Arbitrary (arbitrary),
  Spec,
  describe,
  genericArbitrary,
  it,
  prop,
  shouldBe,
 )

import Cardano.Binary (decodeFull', serialize')
import Crem.Inspect (Component (..), checkStates, inspectStates)
import Crem.StateInspection (StateCheck (NotInspected))
import Crem.StateMachine qualified as Crem
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    version
  ),
  HeadState (FanoutProgress, Open),
  OpenState (OpenState, coordinatedHeadState),
  Outcome (Continue, Error, Wait, stateChanges),
  PartialFanoutState (distributedOutputs),
  StateChanged (
    Checkpoint,
    CommitApproved,
    CommitFinalized,
    DecommitApproved,
    DecommitFinalized,
    DecommitInvalid,
    DecommitRecorded,
    DepositRecorded,
    HeadClosed,
    HeadContested,
    HeadFannedOut,
    HeadFanoutInitiated,
    HeadFanoutReverted,
    HeadIsReadyToFanout,
    HeadOpened,
    HeadPartialFannedOut,
    HeadPartialFanoutSelected,
    LocalStateCleared,
    SnapshotConfirmed,
    SnapshotRequestDecided,
    SnapshotRequested,
    TransactionAppliedToLocalUTxO,
    TxInvalid,
    distributedOutputs,
    newVersion,
    snapshot
  ),
  eventHeadId,
  headIdOf,
  nullOutputs,
 )
import Hydra.HeadLogic.Machine qualified as Machine
import Hydra.HeadLogic.Machine.Deposit qualified as Deposit
import Hydra.HeadLogic.Machine.Lifecycle qualified as Lifecycle
import Hydra.HeadLogic.Outcome qualified as Outcome
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx)
import Hydra.Node.State (Deposit (..), DepositStatus (..), NodeState (..), TrackedDeposit (..), initialChainTime)
import Hydra.Tx (HeadId)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.HeadLogic.Projection.Incremental qualified as Incremental
import Test.Hydra.HeadLogic.Projection.OffChain qualified as OffChain
import Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix)
import Test.Hydra.HeadLogic.Scenario (initialNode)
import Test.Hydra.HeadLogic.Settlement (SettlementStep (..), settlementSequence)
import Test.Hydra.Ledger.Simple ()
import Test.Hydra.Node.State ()
import Test.Hydra.Tx.Fixture (testHeadId)
import Test.QuickCheck (Property, conjoin, counterexample, forAll, property, resize, vectorOf, (===))

spec :: Spec
spec = describe "crem event application" $ do
  it "keeps owner descriptions across execution and state reconstruction" $ do
    let event = HeadClosed testHeadId 0 (SimpleChainState 1) initialChainTime
        original = Machine.components initialNode
        executable = Crem.withoutRendering original
        (node, next) = runIdentity $ Crem.run executable event
        (expected, described) = runIdentity $ Crem.run original event
        descriptions =
          [ ([LeftComponent, LeftComponent], Just "TrackedDeposits")
          , ([LeftComponent, RightComponent], Just "Closed")
          , ([RightComponent], Just "ObservedTime")
          ]
    node `shouldBe` expected
    node `shouldBe` Machine.currentState (Machine.step (Machine.start initialNode) event)
    inspectStates next `shouldBe` descriptions
    inspectStates described `shouldBe` descriptions
    inspectStates (Machine.components node) `shouldBe` descriptions
    checks <- checkStates [] next
    map (\(_, result) -> case result of NotInspected -> True; _ -> False) checks `shouldBe` [True, True, True]

  it "keeps deposit phase descriptions through consumption and rollback" $ do
    let deposit :: Deposit SimpleTx
        deposit = Deposit testHeadId mempty initialChainTime initialChainTime Active
        initial = Crem.Executable $ Deposit.machine @Identity @SimpleTx Nothing
        (_, active) = runIdentity $ Crem.run initial (Deposit.Observe 1 deposit)
        (_, consumed) = runIdentity $ Crem.run active (Deposit.Consume 2)
        (_, restored) = runIdentity $ Crem.run consumed (Deposit.Rollback 1)
    map inspectStates [initial, active, consumed, restored]
      `shouldBe` map (\name -> [([], Just name)]) ["MissingDeposit", "ActiveDeposit", "ConsumedDeposit", "ActiveDeposit"]

  prop "matches replay with the explicit reservation correction, continuous execution and CBOR recovery" $
    forAll (resize 8 arbitrary) $ \initial ->
      forAll (vectorOf 40 $ resize 8 genericArbitrary) $ \events ->
        comparePrefixes initial events

  prop "matches Agda for tracked-deposit updates, consumption and rollback" $
    forAll (resize 8 arbitrary) $ \tracked ->
      forAll arbitrary $ \payload ->
        forAll arbitrary $ \slot ->
          conjoin [depositReference tracked payload slot action | action <- [0 .. 3]]

  it "preserves all 24 settlement and rollback schedules" $
    forM_ [(confirmed, slot, recipients) | confirmed <- [False, True], slot <- [2, 3], recipients <- permutations [0, 1, 2]] $ \(confirmed, slot, recipients) -> do
      steps <- either (fail . toString) pure $ settlementSequence confirmed slot recipients
      forM_ steps $ \entry ->
        foldl' replayWithReservationFix entry.before (retained entry.outcome) `shouldBe` entry.after

  it "tracks optimistic fanout, reversion, contest and completion" $ do
    let events =
          [ HeadClosed testHeadId 0 (SimpleChainState 1) initialChainTime
          , HeadIsReadyToFanout testHeadId
          , HeadFanoutInitiated testHeadId mempty
          , HeadFanoutReverted testHeadId
          , HeadContested testHeadId (SimpleChainState 2) (addUTCTime 100 initialChainTime) 0
          , HeadFannedOut testHeadId mempty (SimpleChainState 3)
          ]
        nodes = map Machine.currentState $ scanl Machine.step (Machine.start initialNode) events
    map (Lifecycle.phase . headState) nodes
      `shouldBe` [Lifecycle.Open, Lifecycle.Closed, Lifecycle.ReadyToFanout, Lifecycle.PendingFanout, Lifecycle.ReadyToFanout, Lifecycle.ReadyToFanout, Lifecycle.Idle]
    nodes `shouldBe` scanl replayWithReservationFix initialNode events

  it "reconstructs every component after checkpoint restoration" $ do
    let waiting = Machine.step (Machine.start initialNode) (SnapshotRequestDecided 1)
        restored = Machine.step waiting (Checkpoint initialNode)
        node = Machine.currentState restored
    node `shouldBe` initialNode
    Lifecycle.phase node.headState `shouldBe` Lifecycle.Open
    Incremental.phase node.headState `shouldBe` Incremental.NoIncrement
    OffChain.phase node.headState `shouldBe` OffChain.NoRequestSeen
    Machine.currentState (Machine.step restored (SnapshotRequestDecided 1))
      `shouldBe` replayWithReservationFix initialNode (SnapshotRequestDecided 1)

-- Rebase head-scoped events onto the current Head for three out of four steps;
-- leave the rest foreign to exercise the filter. Checkpoints remain arbitrary.
-- Generic generation includes every StateChanged constructor, unlike the
-- environment-constrained generator used by node hydration tests.
comparePrefixes :: NodeState SimpleTx -> [StateChanged SimpleTx] -> Property
comparePrefixes initial = go 0 initial (Machine.start initial)
 where
  go :: Int -> NodeState SimpleTx -> Machine.Runtime SimpleTx -> [StateChanged SimpleTx] -> Property
  go _ _ _ [] = property True
  go n old runtime (raw : rest) =
    let event = case (headIdOf old.headState, eventHeadId raw) of
          (Just headId, Just _) | n `mod` 4 /= 0 -> scope headId raw
          _ -> raw
        expected = replayWithReservationFix old event
        next = Machine.step runtime event
        actual = Machine.currentState next
     in counterexample ("prefix " <> show n <> ": " <> show event <> "\nbefore: " <> show old) $
          conjoin
            [ actual === expected
            , Machine.apply old event === expected
            , decodeFull' (serialize' actual) === Right expected
            , incrementalReference actual
            , if ignored old event then property True else lifecycleReference old event actual
            , if ignored old event then property True else coordinationReference old event actual
            , go (n + 1) expected next rest
            ]

retained :: Outcome tx -> [StateChanged tx]
retained = \case
  Continue{stateChanges} -> stateChanges
  Wait{stateChanges} -> stateChanges
  Error{} -> []

-- The Agda pending-action reference projects stored fields; it does not model
-- settlement payloads or admission guards. Compare it at checkpoint prefixes too.
incrementalReference :: NodeState SimpleTx -> Property
incrementalReference node =
  project (Incremental.phase node.headState) === case node.headState of
    Open OpenState{coordinatedHeadState = chs} ->
      Agda.pendingIncrementPhaseRef True (isJust chs.currentDepositTxId) (isJust chs.decommitTx)
    _ -> Agda.pendingIncrementPhaseRef False False False
 where
  project = \case
    Incremental.OutsideHead -> 0
    Incremental.NoIncrement -> 1
    Incremental.DepositSelected -> 2
    Incremental.DecommitPending -> 3
    Incremental.ConflictingSelection -> 4

lifecycleReference :: NodeState SimpleTx -> StateChanged SimpleTx -> NodeState SimpleTx -> Property
lifecycleReference before event after =
  project (Lifecycle.phase after.headState) === Agda.lifecyclePhaseRef (project $ Lifecycle.phase before.headState) tag distributed
 where
  project = \case
    Lifecycle.Idle -> Agda.IdleHead
    Lifecycle.Open -> Agda.OpenHead
    Lifecycle.Closed -> Agda.ClosedHead
    Lifecycle.ReadyToFanout -> Agda.ReadyHead
    Lifecycle.PendingFanout -> Agda.PendingFanout
    Lifecycle.FanoutProgress -> Agda.FanoutProgress
  tag = case event of
    HeadOpened{} -> Agda.Opened
    HeadClosed{} -> Agda.Closed
    HeadIsReadyToFanout{} -> Agda.Ready
    HeadFanoutInitiated{} -> Agda.FanoutStarted
    HeadPartialFanoutSelected{} -> Agda.FanoutStarted
    HeadPartialFannedOut{} -> Agda.PartialObserved
    HeadFanoutReverted{} -> Agda.FanoutReverted
    HeadFannedOut{} -> Agda.FannedOut
    _ -> Agda.Unchanged
  distributed = case event of
    HeadPartialFannedOut{distributedOutputs = added} ->
      not $
        nullOutputs $
          added <> case before.headState of
            FanoutProgress st -> st.distributedOutputs
            _ -> mempty
    _ -> False

coordinationReference :: NodeState SimpleTx -> StateChanged SimpleTx -> NodeState SimpleTx -> Property
coordinationReference before event after =
  project (OffChain.phase after.headState) === Agda.coordinationPhaseRef (project $ OffChain.phase before.headState) tag initial explicit fresh
 where
  project = \case
    OffChain.OutsideHead -> Agda.OutsideHead
    OffChain.NoRequestSeen -> Agda.NoRequestSeen
    OffChain.SnapshotIdle -> Agda.SnapshotIdle
    OffChain.RequestSent -> Agda.RequestSent
    OffChain.CollectingSignatures -> Agda.Collecting
  tag = case event of
    HeadOpened{} -> Agda.HeadOpened
    HeadClosed{} -> Agda.HeadClosed
    SnapshotRequestDecided{} -> Agda.RequestDecided
    SnapshotRequested{} -> Agda.SnapshotRequested
    SnapshotConfirmed{} -> Agda.SnapshotConfirmed
    LocalStateCleared{} -> Agda.LocalCleared
    CommitFinalized{} -> Agda.SettlementObserved
    DecommitFinalized{} -> Agda.SettlementObserved
    _ -> Agda.OtherEvent
  initial = case before.headState of
    Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot = InitialSnapshot{}}} -> True
    _ -> False
  explicit = case event of
    SnapshotConfirmed{snapshot} -> isJust snapshot
    _ -> False
  fresh = case before.headState of
    Open OpenState{coordinatedHeadState = chs} -> case event of
      CommitFinalized{newVersion} -> newVersion > chs.version
      DecommitFinalized{newVersion} -> newVersion > chs.version
      _ -> False
    _ -> False

depositReference :: Maybe (TrackedDeposit SimpleTx) -> Deposit SimpleTx -> ChainSlot -> Integer -> Property
depositReference tracked payload slot action =
  project (Deposit.phase $ Deposit.transition change tracked) === Agda.trackedDepositPhaseRef (project $ Deposit.phase tracked) action status recordedLater consumedLater
 where
  change = case action of
    0 -> Deposit.Observe slot payload
    1 -> Deposit.Update payload
    2 -> Deposit.Consume slot
    _ -> Deposit.Rollback slot
  retainedStatus = maybe Inactive (\t -> t.deposit.status) tracked
  status = case if action < 2 then payload.status else retainedStatus of
    Inactive -> Agda.InactiveS
    Active -> Agda.ActiveS
    Expired -> Agda.ExpiredS
  recordedLater = maybe False (\t -> t.recordedAt > slot) tracked
  consumedLater = maybe False (\t -> maybe False (> slot) t.consumedAt) tracked
  project = \case
    Deposit.MissingDeposit -> Agda.MissingDeposit
    Deposit.InactiveDeposit -> Agda.InactiveDeposit
    Deposit.ActiveDeposit -> Agda.ActiveDeposit
    Deposit.ExpiredDeposit -> Agda.ExpiredDeposit
    Deposit.ConsumedDeposit -> Agda.ConsumedDeposit

ignored :: NodeState SimpleTx -> StateChanged SimpleTx -> Bool
ignored node event = case event of
  Checkpoint{} -> True
  _ -> case (headIdOf node.headState, eventHeadId event) of
    (Just ours, Just theirs) -> ours /= theirs
    _ -> False

-- Only event fields used by the production filter are rebased.
scope :: HeadId -> StateChanged tx -> StateChanged tx
scope headId = \case
  event@HeadOpened{} -> event{Outcome.headId = headId}
  event@TransactionAppliedToLocalUTxO{} -> event{Outcome.headId = headId}
  event@SnapshotConfirmed{} -> event{Outcome.headId = headId}
  event@LocalStateCleared{} -> event{Outcome.headId = headId}
  event@DepositRecorded{} -> event{Outcome.headId = headId}
  event@CommitApproved{} -> event{Outcome.headId = headId}
  event@CommitFinalized{} -> event{Outcome.headId = headId}
  event@DecommitRecorded{} -> event{Outcome.headId = headId}
  event@DecommitApproved{} -> event{Outcome.headId = headId}
  event@DecommitInvalid{} -> event{Outcome.headId = headId}
  event@DecommitFinalized{} -> event{Outcome.headId = headId}
  event@HeadIsReadyToFanout{} -> event{Outcome.headId = headId}
  event@HeadClosed{} -> event{Outcome.headId = headId}
  event@HeadContested{} -> event{Outcome.headId = headId}
  event@HeadFannedOut{} -> event{Outcome.headId = headId}
  event@TxInvalid{} -> event{Outcome.headId = headId}
  event@HeadPartialFannedOut{} -> event{Outcome.headId = headId}
  event@HeadFanoutInitiated{} -> event{Outcome.headId = headId}
  event@HeadPartialFanoutSelected{} -> event{Outcome.headId = headId}
  event@HeadFanoutReverted{} -> event{Outcome.headId = headId}
  event -> event
