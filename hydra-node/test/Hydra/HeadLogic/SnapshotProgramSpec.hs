{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.SnapshotProgramSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.Agda.OffChainReference (HsReqTxOutcome (..), HsReqTxTerminal (..), HsSnapshotChange (..), HsSnapshotEffect (..), HsSnapshotOutcome (..), HsSnapshotPhase (..), HsSnapshotRejection (..), HsSnapshotWait (..), ackSnOutcomeRef, reqSnOutcomeRef, reqTxKnownOutcomeRef)
import Hydra.Chain (PostChainTx (DecrementTx, IncrementTx))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.AckSn qualified as CremAckSn
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx, SimpleTxOut, simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node (defaultTxTTL)
import Hydra.Node.Environment (Environment)
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (
    allTxs,
    confirmedSnapshot,
    decommitTx,
    localTxs,
    localUTxO,
    seenSnapshot
  ),
  Effect (NetworkEffect, OnChainEffect),
  HeadState (Open),
  LogicError (RequireFailed),
  OpenState (coordinatedHeadState),
  Outcome (Continue, Error, Wait, effects),
  RequirementFailure (
    AckSnNumberInvalid,
    InvalidMultisignature,
    ReqSnNotLeader,
    ReqSnNumberInvalid,
    SnapshotAlreadySigned,
    SnapshotDoesNotApply
  ),
  RuleEnvironment,
  SeenSnapshot (
    LastSeenSnapshot,
    NoSeenSnapshot,
    RequestedSnapshot,
    SeenSnapshot,
    signableBytes,
    snapshot
  ),
  StateChanged (
    CommitApproved,
    DecommitApproved,
    PartySignedSnapshot,
    SnapshotConfirmed,
    SnapshotRequestDecided,
    SnapshotRequested,
    TransactionReceived
  ),
  WaitReason (
    WaitOnNotApplicableTx,
    WaitOnSeenSnapshot,
    WaitOnSnapshotNumber,
    WaitOnSnapshotVersion,
    WaitOnTxs
  ),
  aggregateState,
  mkSeenSnapshot,
  onOpenNetworkReqSn,
  onOpenNetworkReqTx,
  onOpenNetworkReqTxProgram,
  ruleEnvironment,
 )
import Hydra.Node.HeadLogic qualified as HeadLogic
import Hydra.Node.State (ChainPointTime, Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (Signature, Verified (FailedKeys), getVerificationKey, sign)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.Explorer qualified as Explorer
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.HeadLogic.Registry qualified as Registry
import Test.Hydra.HeadLogic.Replay qualified as Replay
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.HeadLogic.Settlement qualified as Settlement
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Node.Fixture (
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
  testHeadId,
 )
import Test.Hydra.Prelude (
  Spec,
  describe,
  failure,
  it,
  prop,
  shouldBe,
  shouldContain,
  shouldSatisfy,
 )
import Test.QuickCheck (chooseInt, counterexample, forAll, shuffle, vectorOf)

spec :: Spec
spec = do
  reqSnSpec
  describe "crem phases with program actions" $ specWith $ \environment time pending st sender signature sn ->
    Workflow.run $ CremAckSn.machine environment time pending st sender signature sn
  integrationSpec

reqSnSpec :: Spec
reqSnSpec = do
  describe "bounded ReqSn Agda agreement" $ do
    it "compares independent ReqSn ordered outcomes with declared exclusions" $ do
      forM_ [0 .. 2] $ \seen ->
        forM_ [0 .. seen] $ \confirmed ->
          forM_ [0 .. 4] $ \sn ->
            forM_ [0 .. 2] $ \sender ->
              forM_ [0, 1] $ \sv ->
                forM_ [False, True] $ \missing ->
                  forM_ [False, True] $ \applicable -> do
                    let predecessor = Scenario.initialSnapshot{number = fromIntegral confirmed}
                        c = os.coordinatedHeadState{seenSnapshot = LastSeenSnapshot $ fromIntegral seen, confirmedSnapshot = ConfirmedSnapshot predecessor mempty}
                        ledger = if applicable then simpleLedger else Ledger $ \_ _ _ -> Left (Scenario.transactionOne, ValidationError "controlled failure")
                        outcome = onOpenNetworkReqSn env ledger mempty initial.chainPointTime os{coordinatedHeadState = c} 1 (Scenario.scenarioParties !! sender) sv (fromIntegral sn) [if missing then 99 else 1] Nothing Nothing
                    projectSnapshotOutcome outcome `shouldBe` reqSnOutcomeRef seen confirmed 0 (fromIntegral sv) sn 2 (fromIntegral sender) (if missing then 1 else 0) applicable

type AckRule = RuleEnvironment -> ChainPointTime -> PendingDeposits SimpleTx -> OpenState SimpleTx -> Party -> Signature (Snapshot SimpleTx) -> SnapshotNumber -> Program.Program SimpleTx ()

specWith :: AckRule -> Spec
specWith onOpenNetworkAckSnProgram = do
  describe "AckSn program" $ do
    it "preserves increment and decrement approval ordering at confirmation" $ do
      forM_ [False, True] $ \decommitting -> do
        let pending = if decommitting then mempty else Map.singleton 7 (depositFixture Active)
            base = os{coordinatedHeadState = os.coordinatedHeadState{decommitTx = if decommitting then Just Scenario.transactionOne else Nothing}}
            ids = [1 | not decommitting]
            req = onOpenNetworkReqSn env simpleLedger pending initial.chainPointTime base 1 alice 0 1 ids (if decommitting then Just Scenario.transactionOne else Nothing) (if decommitting then Nothing else Just 7)
            afterRequest = aggregateState initial{headState = Open base} req
            afterOpen = Scenario.openState afterRequest
        case afterOpen.coordinatedHeadState.seenSnapshot of
          SeenSnapshot{snapshot} -> do
            let seen = mkSeenSnapshot snapshot (Map.fromList [(alice, sign aliceSk snapshot), (bob, sign bobSk snapshot)])
                ready = afterOpen{coordinatedHeadState = afterOpen.coordinatedHeadState{seenSnapshot = seen}}
                new = onOpenNetworkAckSn (Scenario.environment 1) initial.chainPointTime pending ready carol (sign carolSk snapshot) 1
            new `shouldSatisfy` \case
              Continue (PartySignedSnapshot{} : SnapshotConfirmed{} : DecommitApproved{} : _) (OnChainEffect DecrementTx{} : _) -> decommitting
              Continue (PartySignedSnapshot{} : SnapshotConfirmed{} : CommitApproved{} : _) (OnChainEffect IncrementTx{} : _) -> not decommitting
              _ -> False
          _ -> failure "incremental request did not produce a snapshot"

    it "rejecting the last invalid signature discards its staged change" $ do
      let state = ackState CollectingSignatures [0, 1]
          program = onOpenNetworkAckSnProgram (ruleEnvironment env) initial.chainPointTime mempty (Scenario.openState state) carol (sign aliceSk snapshotFixture) 1
          (outcome, trace) = Interpreter.traceProgram (testInterpreterConfig simpleLedger) program
      trace `shouldContain` [Trace.Changed "party signed snapshot"]
      viaNonEmpty last trace `shouldBe` Just Trace.Rejected
      outcome `shouldSatisfy` \case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False
      aggregateState state outcome `shouldBe` state

    it "uses the configured verifier in production and debugger recording" $ do
      let state = ackState CollectingSignatures [0, 1]
          program = onOpenNetworkAckSnProgram (ruleEnvironment env) initial.chainPointTime mempty (Scenario.openState state) carol (sign carolSk snapshotFixture) 1
          config :: Interpreter.InterpreterConfig SimpleTx
          config = (testInterpreterConfig simpleLedger){Interpreter.snapshotVerifier = \_ _ _ -> FailedKeys [getVerificationKey carolSk]}
          outcome = Interpreter.runProgram config program
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program `shouldSatisfy` \case Continue{} -> True; _ -> False
      outcome `shouldSatisfy` \case
        Error (RequireFailed (InvalidMultisignature _ failedKeys)) -> failedKeys == [getVerificationKey carolSk]
        _ -> False
      aggregateState state outcome `shouldBe` state
      recorded <- either (fail . toString) pure $ Debug.prepare Debug.RecordReplies config program
      Interpreter.runProgram config recorded.lowered `shouldBe` outcome
      -- Replay uses the retained reply, even when the current verifier accepts.
      replayed <- either (fail . toString) pure $ Debug.prepare (Debug.RecordedReplies recorded.queries) (testInterpreterConfig simpleLedger) program
      Interpreter.runProgram (testInterpreterConfig simpleLedger) replayed.lowered `shouldBe` outcome

  describe "bounded AckSn Agda agreement" $ do
    it "compares independent AckSn outcomes including rejection after staging" $ do
      forM_ [minBound .. maxBound] $ \phase ->
        forM_ (subsequences [0, 1, 2]) $ \signers ->
          forM_ [0 .. 2] $ \sender ->
            forM_ [0 .. 3] $ \sn ->
              forM_ [False, True] $ \valid -> do
                let state = ackState phase signers
                    signature = sign ([aliceSk, bobSk, carolSk] !! sender) (if valid then snapshotFixture else Scenario.initialSnapshot)
                    outcome = onOpenNetworkAckSn (Scenario.environment 1) initial.chainPointTime mempty (Scenario.openState state) (Scenario.scenarioParties !! sender) signature sn
                    seen = if phase == NeverSeen then 0 else 1
                projectSnapshotOutcome outcome `shouldBe` ackSnOutcomeRef phase seen (fromIntegral sn) (fromIntegral <$> signers) (fromIntegral sender) 2 valid True 1
 where
  onOpenNetworkAckSn env' time pending st sender signature sn =
    Interpreter.runProgram (testInterpreterConfig simpleLedger) $ onOpenNetworkAckSnProgram (ruleEnvironment env') time pending st sender signature sn

integrationSpec :: Spec
integrationSpec = do
  describe "registry regressions and bounded recovery policies" $ do
    prop "preserves registry payloads and reconciles local UTxO at every scheduled prefix" $
      forAll (vectorOf 40 $ chooseInt (0, 10000)) $ \choices ->
        let worlds = scanl scheduleChoice Scenario.initialWorld choices
         in counterexample (show $ fmap Explorer.safetyFailure worlds) $ all (isNothing . Explorer.safetyFailure) worlds

    prop "confirms dependent transactions after fair delivery without retry exhaustion" $
      forAll (shuffle Scenario.initialWorld.pending) $ \deliveries ->
        let final = drainWorld 100 Scenario.initialWorld{Scenario.pending = deliveries}
         in counterexample (show final) $
              isNothing (Explorer.invariantFailure final)
                && null final.pending
                && null final.retries
                && all ((== utxoRef 3) . (.utxo) . getSnapshot . (.confirmedSnapshot) . (.coordinatedHeadState) . Scenario.openState) final.nodes

    it "preserves the validated payload and ignores every retry of an applied transaction" $ do
      world <- requireRight $ Explorer.runPath Scenario.VerifySignatures Scenario.initialWorld registryRegressionRoute
      Explorer.invariantFailure world `shouldBe` Nothing
      let node = world.nodes !! 1
          c = (Scenario.openState node).coordinatedHeadState
          request = onOpenNetworkReqSn (Scenario.environment 1) simpleLedger mempty node.chainPointTime (Scenario.openState node) 2 alice 0 1 [1] Nothing Nothing
      c.localTxs `shouldBe` Seq.singleton Scenario.transactionOne
      c.localUTxO `shouldBe` utxoRef 2
      Map.lookup 1 c.allTxs `shouldBe` Just Scenario.transactionOne
      request `shouldSatisfy` \case Continue{} -> True; _ -> False
      forM_ [0 .. defaultTxTTL] $ \ttl -> do
        let program = onOpenNetworkReqTxProgram (ruleEnvironment env) node.chainPointTime (Scenario.openState node) ttl mempty Scenario.transactionOne
            (outcome, trace) = Interpreter.traceProgram (testInterpreterConfig simpleLedger) program
        outcome `shouldBe` Continue [] []
        trace `shouldSatisfy` all (\case Trace.LedgerApplied{} -> False; _ -> True)
        reqTxKnownOutcomeRef True False (toInteger ttl) False 2 1 1 `shouldBe` HsReqTxOutcome ReqTxFinished [] []
    it "characterizes an expired dependency leaving an honest head quiescent with pending work" $ do
      -- B receives tx2 before tx1 and exhausts its retry budget. Delivering all
      -- remaining messages confirms snapshot 1, but B has lost the work for 2.
      world <- requireRight $ Explorer.runPath Scenario.VerifySignatures Scenario.initialWorld expiredDependencyRoute
      world.pending `shouldBe` []
      world.retries `shouldBe` []
      world.delayed `shouldBe` []
      Explorer.invariantFailure world `shouldBe` Just "quiescent scenario has unconfirmed transactions"
      let states = (\node -> (Scenario.openState node).coordinatedHeadState) <$> world.nodes
      fmap (\c -> (getSnapshot c.confirmedSnapshot).number) states `shouldBe` [1, 1, 1]
      fmap (toList . (.localTxs)) states `shouldBe` [[Scenario.transactionTwo], [], [Scenario.transactionTwo]]
      let bobNode = world.nodes !! 1
          request = onOpenNetworkReqSn (Scenario.environment 1) simpleLedger mempty bobNode.chainPointTime (Scenario.openState bobNode) 2 bob 0 2 [2] Nothing Nothing
      request `shouldBe` Wait (WaitOnTxs [2]) []
      -- Re-submission supplies the lost payload and wakes the next leader.
      let retryTx = onOpenNetworkReqTx (Scenario.environment 1) simpleLedger bobNode.chainPointTime (Scenario.openState bobNode) 2 mempty Scenario.transactionTwo
      retryTx `shouldSatisfy` \case
        Continue{effects} -> NetworkEffect (ReqSn 0 2 [2] Nothing Nothing) `elem` effects
        _ -> False

    it "characterizes invalid early signatures blocking later valid replacements" $ do
      let start = ackState CollectingSignatures []
          applyAck node sender signature = HeadLogic.onOpenNetworkAckSn (testInterpreterConfig simpleLedger) env node.chainPointTime mempty (Scenario.openState node) sender signature 1
          bad = sign aliceSk Scenario.initialSnapshot
          accepted = applyAck start alice bad
          poisoned = aggregateState start accepted
          afterBob = aggregateState poisoned $ applyAck poisoned bob (sign bobSk snapshotFixture)
          final = applyAck afterBob carol (sign carolSk snapshotFixture)
          corrected = applyAck afterBob alice (sign aliceSk snapshotFixture)
      accepted `shouldSatisfy` \case Continue{} -> True; _ -> False
      final `shouldSatisfy` \case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False
      aggregateState afterBob final `shouldBe` afterBob
      corrected `shouldSatisfy` \case Error (RequireFailed SnapshotAlreadySigned{}) -> True; _ -> False

  describe "bounded explorer and replay debugger" $ do
    it "preserves the validated Cardano transaction after an invalid duplicate" $ do
      Registry.registryRegression `shouldSatisfy` isRight

    it "detects omitted receipt and a dishonest ledger reply with independent state invariants" $ do
      let world = Scenario.initialWorld
          d = Scenario.Delivery 0 1 2 (ReqTx Scenario.transactionOne)
          withoutReceipt :: Program.Program tx a -> Program.Program tx a
          withoutReceipt = Program.foldProgram $ \case
            Program.Change TransactionReceived{} -> pure ()
            instruction -> Program.instruction instruction
          missingReceipt = case Explorer.programFor world d of
            Interpreter.Execution originalConfig program -> Interpreter.runProgram originalConfig $ withoutReceipt program
          missingDependency = Scenario.Delivery 0 1 2 (ReqTx Scenario.transactionTwo)
          config = (testInterpreterConfig simpleLedger){Interpreter.transactionApplication = \_ _ _ -> Right (utxoRef 1 <> utxoRef 3)}
          acceptedWithoutInput = Interpreter.runProgram config $ onOpenNetworkReqTxProgram (ruleEnvironment $ Scenario.environment 1) Scenario.initialNode.chainPointTime (Scenario.openState Scenario.initialNode) 2 mempty Scenario.transactionTwo
      Explorer.safetyFailure (Explorer.installOutcome world d missingReceipt)
        `shouldBe` Just "local pending transaction differs from the registry payload"
      Explorer.safetyFailure (Explorer.installOutcome world missingDependency acceptedWithoutInput)
        `shouldBe` Just "local UTxO does not reconcile with snapshot and pending transactions"

    it "finds asymmetric retry expiry within the default budget and replays its minimized schedule" $ do
      let settings = Scenario.defaultExploration{Scenario.family = Scenario.DelayedDependency, Scenario.searchOrder = Scenario.RetryWindows}
          start = Scenario.initialFor settings.family
          report = Explorer.explore Scenario.defaultLimits settings
      report.failure `shouldBe` Just "quiescent scenario has unconfirmed transactions"
      report.exploredStates `shouldSatisfy` (< Scenario.defaultLimits.maxStates)
      let route = Explorer.minimize settings.executionMode start report.path
      (scenario, _) <- requireRight $ Replay.recordScenario Scenario.defaultLimits settings start route
      (world, _) <- requireRight $ Replay.replayScenario scenario
      Explorer.safetyFailure world `shouldBe` Nothing
      Explorer.progressFailure world `shouldBe` report.failure
      Explorer.explore (Scenario.Limits 1 1) settings `shouldSatisfy` Scenario.truncated

    it "preserves settlement and spent outputs across 24 rollback schedules and serialized replay" $ do
      forM_ [False, True] $ \confirmed ->
        forM_ [2, 3] $ \slot ->
          forM_ (permutations [0, 1, 2]) $ \recipients ->
            void $ requireRight $ Settlement.settlementSequence confirmed slot recipients

    it "finds and minimizes invalid confirmation, while the normal interpreter rejects that sequence" $ do
      let report = Explorer.explore (Scenario.Limits 24 5000) faultExploration
      report.failure `shouldBe` Just "confirmed snapshot has invalid signatures"
      let route = Explorer.minimize Scenario.BypassVerification Scenario.initialWorld report.path
      normal <- requireRight $ Explorer.runPath Scenario.VerifySignatures Scenario.initialWorld route
      Explorer.invariantFailure normal `shouldBe` Nothing
      Explorer.minimize Scenario.VerifySignatures Scenario.initialWorld route `shouldBe` route
      outcomes <- requireRight $ Explorer.pathOutcomes Scenario.VerifySignatures Scenario.initialWorld route
      outcomes `shouldSatisfy` any (\case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False)
      (scenario, views) <- requireRight $ Replay.recordScenario report.limits faultExploration Scenario.initialWorld route
      (replayed, replayViews) <- requireRight $ Replay.replayScenario scenario
      Explorer.invariantFailure replayed `shouldBe` report.failure
      replayViews `shouldBe` views
      let (_, stopped) = Debug.breakAt "TEST ONLY: bypass multisignature verification" views
      stopped `shouldSatisfy` (not . null)
      Debug.stepOnce stopped `shouldSatisfy` isJust
      forM_ [0 .. length route - 1] $ \i ->
        case Explorer.runPath Scenario.BypassVerification Scenario.initialWorld (take i route <> drop (i + 1) route) of
          Left _ -> pure ()
          Right world -> Explorer.invariantFailure world `shouldBe` Nothing

    it "reports truncation and deterministic counts at the state limit" $ do
      let report = Explorer.explore (Scenario.Limits 24 40) Scenario.defaultExploration
      report.truncated `shouldBe` True
      report.exploredStates `shouldBe` 40
      Explorer.explore (Scenario.Limits 24 40) Scenario.defaultExploration `shouldBe` report

    it "round trips an explicit starting world" $ do
      start <- requireRight $ Explorer.runPath Scenario.VerifySignatures Scenario.initialWorld [Scenario.Deliver $ Scenario.Delivery 0 1 2 (ReqTx Scenario.transactionOne)]
      (scenario, _) <- requireRight $ Replay.recordScenario Scenario.defaultLimits Scenario.defaultExploration start []
      decoded <- either failure pure $ eitherDecode $ encode scenario
      decoded `shouldBe` scenario
      (world, _) <- requireRight $ Replay.replayScenario decoded
      world `shouldBe` start
    it "runs each declared family and search order deterministically within bounds" $ do
      forM_ [Scenario.DependentTransactions, Scenario.DuplicateTransaction, Scenario.DelayedDependency] $ \family ->
        forM_ [Scenario.DeliveriesFirst, Scenario.RetriesFirst, Scenario.BreadthFirst, Scenario.RetryWindows] $ \order -> do
          let settings = Scenario.defaultExploration{Scenario.family = family, Scenario.searchOrder = order}
              report = Explorer.explore (Scenario.Limits 24 40) settings
          report.exploredStates `shouldSatisfy` (<= 40)
          Explorer.explore report.limits settings `shouldBe` report

    it "rejects wrong query identity, arguments, missing and extra replies" $ do
      let program = onOpenNetworkReqTxProgram (ruleEnvironment env) initial.chainPointTime (Scenario.openState Scenario.initialNode) 1 mempty Scenario.transactionOne
          config = testInterpreterConfig simpleLedger
      prepared <- requireRight $ Debug.prepare Debug.RecordReplies config program
      let tape = prepared.queries
      case tape of
        [] -> failure "expected a primitive query"
        q : qs -> do
          forM_ [[], q{Debug.primitive = "wrong query"} : qs, q{Debug.arguments = toJSON ()} : qs, tape <> tape] $ \wrong ->
            isLeft (Debug.prepare (Debug.RecordedReplies wrong) config program) `shouldBe` True
          let controlled = q{Debug.reply = toJSON (Left (ValidationError "controlled") :: Either ValidationError (Set SimpleTxOut))} : qs
          changed <- requireRight $ Debug.prepare (Debug.RecordedReplies controlled) config program
          Interpreter.runProgram (testInterpreterConfig simpleLedger) changed.lowered `shouldBe` Wait (WaitOnNotApplicableTx $ ValidationError "controlled") [TransactionReceived Scenario.transactionOne]
initial :: NodeState SimpleTx
initial = Scenario.initialNode{headState = Open os}

os :: OpenState SimpleTx
os = base{coordinatedHeadState = c{allTxs = Map.fromList [(txId tx, tx) | tx <- [Scenario.transactionOne, Scenario.transactionTwo]], localTxs = Seq.fromList [Scenario.transactionOne, Scenario.transactionTwo], localUTxO = utxoRef 3}}
 where
  base = Scenario.openState Scenario.initialNode
  c = base.coordinatedHeadState

env :: Environment
env = Scenario.environment 0

snapshotFixture :: Snapshot SimpleTx
snapshotFixture = case (Scenario.openState $ aggregateState initial $ onOpenNetworkReqSn env simpleLedger mempty initial.chainPointTime os 1 alice 0 1 [1] Nothing Nothing).coordinatedHeadState.seenSnapshot of
  SeenSnapshot{snapshot} -> snapshot
  _ -> error "snapshot fixture did not request"

ackState :: HsSnapshotPhase -> [Int] -> NodeState SimpleTx
ackState phase signers = initial{headState = Open os{coordinatedHeadState = c{localTxs = Seq.singleton Scenario.transactionTwo, seenSnapshot = seen}}}
 where
  c = os.coordinatedHeadState
  seen = case phase of
    NeverSeen -> NoSeenSnapshot
    RequestPending -> RequestedSnapshot 1 2
    CollectingSignatures -> case mkSeenSnapshot snapshotFixture mempty of
      SeenSnapshot{signableBytes} -> SeenSnapshot snapshotFixture (Map.fromList [(Scenario.scenarioParties !! i, sign ([aliceSk, bobSk, carolSk] !! i) snapshotFixture) | i <- signers]) signableBytes
      _ -> error "mkSeenSnapshot did not construct a seen snapshot"
    LastConfirmed -> LastSeenSnapshot 1

depositFixture :: DepositStatus -> Deposit SimpleTx
depositFixture = Deposit testHeadId (utxoRef 100) initialChainTime (addUTCTime 3600 initialChainTime)

projectSnapshotOutcome :: Outcome SimpleTx -> HsSnapshotOutcome
projectSnapshotOutcome = \case
  Continue events effects -> SnapshotCompleted (fmap projectChange events) (fmap projectEffect effects)
  Wait reason _ -> SnapshotWaiting $ case reason of
    WaitOnSnapshotNumber{} -> PreviousUnconfirmed
    WaitOnSnapshotVersion{} -> VersionMismatch
    WaitOnTxs{} -> MissingTransactions
    WaitOnSeenSnapshot -> SnapshotNotSeen
    _ -> error "wait outside bounded snapshot model"
  Error (RequireFailed reason) -> SnapshotRejected $ case reason of
    ReqSnNumberInvalid{} -> InvalidNumber
    AckSnNumberInvalid{} -> InvalidNumber
    ReqSnNotLeader{} -> NotLeader
    SnapshotDoesNotApply{} -> TransactionDoesNotApply
    SnapshotAlreadySigned{} -> AlreadySigned
    InvalidMultisignature{} -> InvalidSignatureSet
    _ -> error "rejection outside bounded snapshot model"
  _ -> error "outcome outside bounded snapshot model"
 where
  projectChange :: StateChanged SimpleTx -> HsSnapshotChange
  projectChange = \case
    PartySignedSnapshot{} -> RecordSignature
    SnapshotConfirmed{} -> ConfirmSnapshot
    SnapshotRequestDecided{} -> DecideNextSnapshot
    SnapshotRequested{} -> RecordSnapshot
    _ -> error "change outside bounded snapshot model"
  projectEffect :: Effect SimpleTx -> HsSnapshotEffect
  projectEffect = \case
    NetworkEffect AckSn{} -> SendAcknowledgement
    NetworkEffect ReqSn{} -> SendSnapshotRequest
    _ -> error "effect outside bounded snapshot model"

requireRight :: Either Text a -> IO a
requireRight = either (failure . toString) pure

-- | Known-bug witnesses use the normal interpreter and enabled scheduler actions.
registryRegressionRoute :: [Scenario.Action]
registryRegressionRoute = [Scenario.Duplicate d, Scenario.Deliver d, Scenario.Deliver d]
 where
  d = Scenario.Delivery 0 1 2 (ReqTx Scenario.transactionOne)

expiredDependencyRoute :: [Scenario.Action]
expiredDependencyRoute = prefix <> drain 40 (either error id $ Explorer.runPath Scenario.VerifySignatures Scenario.initialWorld prefix)
 where
  d = Scenario.Delivery 0 1 2 (ReqTx Scenario.transactionTwo)
  prefix = [Scenario.Deliver d{Scenario.recipientIndex = 0}, Scenario.Deliver d{Scenario.recipientIndex = 2}, Scenario.Deliver d, Scenario.Retry d{Scenario.remainingTTL = 1}, Scenario.Retry d{Scenario.remainingTTL = 0}]
  drain :: Int -> Scenario.World -> [Scenario.Action]
  drain remaining world
    | remaining <= 0 = []
    | otherwise = case fmap Scenario.Deliver world.pending <> fmap Scenario.Retry world.retries of
        [] -> []
        action : _ -> action : drain (remaining - 1) (either error id $ Explorer.advance Scenario.VerifySignatures world action)

faultExploration :: Scenario.Exploration
faultExploration = Scenario.defaultExploration{Scenario.executionMode = Scenario.BypassVerification, Scenario.disturbances = Scenario.AllowInvalidAcknowledgement}

-- | Choose only enabled actions; safety must survive delay, duplicates and retries.
scheduleChoice :: Scenario.World -> Int -> Scenario.World
scheduleChoice world choice = case Explorer.actions Scenario.ValidMessagesOnly world of
  [] -> world
  enabled -> either error id $ Explorer.advance Scenario.VerifySignatures world (enabled !! (choice `mod` length enabled))

-- | Deliver all pending messages before retrying, so prerequisites can arrive.
drainWorld :: Int -> Scenario.World -> Scenario.World
drainWorld remaining world
  | remaining <= 0 = world
  | otherwise = case fmap Scenario.Deliver world.pending <> fmap Scenario.Retry world.retries of
      [] -> world
      action : _ -> drainWorld (remaining - 1) $ either error id $ Explorer.advance Scenario.VerifySignatures world action
