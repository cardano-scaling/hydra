{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.SnapshotProgramSpec (spec) where

import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Hydra.HeadLogic hiding (error)
import Hydra.HeadLogic qualified as HeadLogic
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.HeadLogic.Version qualified as Version
import Hydra.Ledger (Ledger)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node (defaultTxTTL)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx (IsTx, TxIdType, txId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (Signature, aggregateInOrder, sign)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)
import Test.Hydra.HeadLogic.LegacyReqTx (legacyOnOpenNetworkReqTx)
import Test.Hydra.HeadLogic.LegacySnapshot
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Node.Fixture
import Test.Hydra.Prelude hiding (pending)
import Test.QuickCheck (checkCoverage, chooseInteger, cover, forAllShrink, ioProperty, oneof, shrinkIntegral)

spec :: Spec
spec = do
  describe "version evidence" $ do
    versionCases "only establishes a request version when the head and request agree" $ \headVersion ->
      forM_ (requestVersions headVersion) $ \requestedVersion ->
        case Version.matchRequestVersion headVersion requestedVersion of
          Nothing -> headVersion `shouldNotBe` requestedVersion
          Just (Version.SomeRequestVersion checked) -> do
            headVersion `shouldBe` requestedVersion
            Version.requestVersion checked `shouldBe` headVersion

    versionCases "signs exactly the snapshot constructed at the validated request version" $ \headVersion ->
      case Version.matchRequestVersion headVersion headVersion of
        Nothing -> failure "equal versions did not match"
        Just (Version.SomeRequestVersion checked) -> do
          let expectedSnapshot :: Snapshot SimpleTx
              expectedSnapshot = initialSnapshot{version = headVersion}
              program = do
                signed <- Version.signSnapshot $ Version.snapshotForRequest checked initialSnapshot
                Program.change SnapshotRequested{requestedSnapshot = Version.signedSnapshot signed, newLocalTxs = mempty, newCurrentDepositTxId = Nothing}
                Program.emit $ NetworkEffect $ AckSn (Version.snapshotSignature signed) expectedSnapshot.number
              expected =
                Continue
                  [SnapshotRequested{requestedSnapshot = expectedSnapshot, newLocalTxs = mempty, newCurrentDepositTxId = Nothing}]
                  [NetworkEffect $ AckSn (sign env.signingKey expectedSnapshot) expectedSnapshot.number]
          Interpreter.runProgramWith (Interpreter.snapshotConfig env simpleLedger) program `shouldBe` expected

    versionCases "matches settlement by snapshot number and exact successor, preserving the snapshot" $ \snapshotVersion ->
      forM_ ([0, snapshotVersion, snapshotVersion + 1, snapshotVersion + 2] <> [snapshotVersion - 1 | snapshotVersion > 0]) $ \observedVersion ->
        forM_ [0, 1, 2] $ \observedNumber -> do
          let snapshot :: Snapshot SimpleTx
              snapshot = snapshotFixture{version = snapshotVersion}
              matches = observedNumber == snapshot.number && observedVersion == snapshotVersion + 1
          case Version.matchSettlement observedNumber observedVersion snapshot of
            Nothing -> matches `shouldBe` False
            Just settled -> do
              matches `shouldBe` True
              Version.settlementSnapshot settled `shouldBe` snapshot
              Version.settlementVersion settled `shouldBe` observedVersion

    versionCases "preserves request validation order and outcomes across past, current and future versions" $ \headVersion -> do
      let coordinated = os.coordinatedHeadState
          previous :: Snapshot SimpleTx
          previous = (getSnapshot coordinated.confirmedSnapshot){version = headVersion}
          confirmed = ConfirmedSnapshot previous (aggregateInOrder (Map.fromList [(p, sign sk previous) | (p, sk) <- zip scenarioParties [aliceSk, bobSk, carolSk]]) scenarioParties)
          opened = os{coordinatedHeadState = coordinated{version = headVersion, confirmedSnapshot = confirmed}}
          initialAtVersion = initial{headState = Open opened}
      forM_ (requestVersions headVersion) $ \requestedVersion ->
        forM_ [0, 1, 2] $ \requestedNumber ->
          forM_ [alice, bob] $ \sender -> do
            let old = legacyOnOpenNetworkReqSn env simpleLedger mempty initialAtVersion.chainPointTime opened 1 sender requestedVersion requestedNumber [1] Nothing Nothing
                new = runReqSn env simpleLedger mempty initialAtVersion.chainPointTime opened 1 sender requestedVersion requestedNumber [1] Nothing Nothing
            new `shouldBe` old
            aggregateState initialAtVersion new `shouldBe` aggregateState initialAtVersion old
  describe "snapshot program migration" $ do
    it "preserves complete ReqSn outcomes and states across waits, deposits, decommits and pruning" $ do
      forM_ [0, 1] $ \ttl ->
        forM_ [alice, bob] $ \sender ->
          forM_ [0, 1, 2] $ \sn ->
            forM_ [0, 1] $ \sv ->
              forM_ [[], [1], [1, 2], [2], [99]] $ \ids ->
                forM_ [Nothing, Just transactionOne] $ \decommit ->
                  forM_ [Nothing, Just 7] $ \deposit ->
                    forM_ [mempty, Map.singleton 7 (depositFixture Active), Map.singleton 7 (depositFixture Inactive), Map.singleton 7 (depositFixture Expired)] $ \pending -> do
                      let old = legacyOnOpenNetworkReqSn env simpleLedger pending initial.chainPointTime os ttl sender sv sn ids decommit deposit
                          new = runReqSn env simpleLedger pending initial.chainPointTime os ttl sender sv sn ids decommit deposit
                      new `shouldBe` old
                      aggregateState initial new `shouldBe` aggregateState initial old

    it "preserves complete AckSn outcomes, repeated records, early/duplicate acks and next requests" $ do
      forM_ [minBound .. maxBound] $ \phase ->
        forM_ (subsequences [0, 1, 2]) $ \signers ->
          forM_ [0 .. 2] $ \sender ->
            forM_ [0 .. 3] $ \sn ->
              forM_ [False, True] $ \valid -> do
                let state = ackState phase signers
                    signature = sign ([aliceSk, bobSk, carolSk] !! sender) (if valid then snapshotFixture else initialSnapshot)
                    old = legacyOnOpenNetworkAckSn (environment 1) initial.chainPointTime mempty (openState state) (scenarioParties !! sender) signature sn
                    new = runAckSn (environment 1) initial.chainPointTime mempty (openState state) (scenarioParties !! sender) signature sn
                new `shouldBe` old
                aggregateState state new `shouldBe` aggregateState state old

    it "preserves increment and decrement approval ordering at confirmation" $ do
      forM_ [False, True] $ \decommitting -> do
        let pending = if decommitting then mempty else Map.singleton 7 (depositFixture Active)
            base = os{coordinatedHeadState = os.coordinatedHeadState{decommitTx = if decommitting then Just transactionOne else Nothing}}
            ids = [1 | not decommitting]
            req = runReqSn env simpleLedger pending initial.chainPointTime base 1 alice 0 1 ids (if decommitting then Just transactionOne else Nothing) (if decommitting then Nothing else Just 7)
            afterRequest = aggregateState initial{headState = Open base} req
            afterOpen = openState afterRequest
        case afterOpen.coordinatedHeadState.seenSnapshot of
          SeenSnapshot{snapshot} -> do
            let seen = mkSeenSnapshot snapshot (Map.fromList [(alice, sign aliceSk snapshot), (bob, sign bobSk snapshot)])
                ready = afterOpen{coordinatedHeadState = afterOpen.coordinatedHeadState{seenSnapshot = seen}}
                old = legacyOnOpenNetworkAckSn (environment 1) initial.chainPointTime pending ready carol (sign carolSk snapshot) 1
                new = runAckSn (environment 1) initial.chainPointTime pending ready carol (sign carolSk snapshot) 1
                beforeAck = afterRequest{headState = Open ready}
            new `shouldBe` old
            aggregateState beforeAck new `shouldBe` aggregateState beforeAck old
            new `shouldSatisfy` \case
              Continue{stateChanges, effects} -> length stateChanges >= 3 && any (\case OnChainEffect{} -> True; _ -> False) effects
              _ -> False
          _ -> failure "incremental request did not produce a snapshot"

    it "rejecting the last invalid signature discards its staged change" $ do
      let state = ackState CollectingSignatures [0, 1]
          program = onOpenNetworkAckSnProgram (ruleEnvironment env) initial.chainPointTime mempty (openState state) carol (sign aliceSk snapshotFixture) 1
          (outcome, trace) = Interpreter.traceProgramWith Interpreter.acknowledgementConfig program
      trace `shouldContain` [Trace.Changed "record snapshot signature"]
      viaNonEmpty last trace `shouldBe` Just Trace.Rejected
      outcome `shouldSatisfy` \case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False
      aggregateState state outcome `shouldBe` state

    it "continues after a narrow or promoted rule finishes a partial or redundant acknowledgement" $ do
      forM_ [CollectingSignatures, LastConfirmed] $ \phase -> do
        let signature = sign aliceSk snapshotFixture
            event = PartySignedSnapshot 1 alice signature
            afterHandler = NetworkEffect $ ReqTx transactionTwo
            program = do
              HeadLogic.onOpenNetworkAckSn env initial.chainPointTime mempty (openState $ ackState phase []) alice signature 1
              Program.emit afterHandler
            narrowProgram = do
              onOpenNetworkAckSnProgram (ruleEnvironment env) initial.chainPointTime mempty (openState $ ackState phase []) alice signature 1
              Program.emit afterHandler
            expectedChanges = case phase of
              CollectingSignatures -> [event, event]
              _ -> []
        Interpreter.runProgramWith (Interpreter.snapshotConfig env simpleLedger) program
          `shouldBe` Continue expectedChanges [afterHandler]
        Interpreter.runProgramWith Interpreter.acknowledgementConfig narrowProgram
          `shouldBe` Continue expectedChanges [afterHandler]

    it "pins two ordered records on a partial acknowledgement" $ do
      let signature = sign aliceSk snapshotFixture
          outcome = runAckSn env initial.chainPointTime mempty (openState $ ackState CollectingSignatures []) alice signature 1
          event = PartySignedSnapshot 1 alice signature
      outcome `shouldBe` Continue [event, event] []

  describe "registry and acknowledgement regressions" $ do
    it "preserves the validated payload and ignores every retry of an applied transaction" $ do
      let node = aggregateState initialNode $ runReqTx (environment 1) simpleLedger initialNode.chainPointTime (openState initialNode) 2 mempty transactionOne
          c = (openState node).coordinatedHeadState
          request = runReqSn (environment 1) simpleLedger mempty node.chainPointTime (openState node) 2 alice 0 1 [1] Nothing Nothing
      c.localTxs `shouldBe` Seq.singleton transactionOne
      c.localUTxO `shouldBe` utxoRef 2
      Map.lookup 1 c.allTxs `shouldBe` Just transactionOne
      request `shouldSatisfy` \case Continue{} -> True; _ -> False
      forM_ [0 .. defaultTxTTL] $ \ttl -> do
        let program = onOpenNetworkReqTxProgram (ruleEnvironment env) node.chainPointTime (openState node) ttl mempty transactionOne
            (outcome, trace) = Interpreter.traceProgram simpleLedger program
        outcome `shouldBe` Continue [] []
        trace `shouldSatisfy` all (\case Trace.LedgerApplied{} -> False; _ -> True)
        aggregateState node outcome `shouldBe` node
      let old = legacyOnOpenNetworkReqTx (environment 1) simpleLedger node.chainPointTime (openState node) 0 mempty transactionOne
      aggregateState node old `shouldNotBe` node

    it "characterizes invalid early signatures blocking later valid replacements" $ do
      let start = ackState CollectingSignatures []
          applyAck node sender signature = runAckSn env node.chainPointTime mempty (openState node) sender signature 1
          bad = sign aliceSk initialSnapshot
          accepted = applyAck start alice bad
          poisoned = aggregateState start accepted
          afterBob = aggregateState poisoned $ applyAck poisoned bob (sign bobSk snapshotFixture)
          final = applyAck afterBob carol (sign carolSk snapshotFixture)
          corrected = applyAck afterBob alice (sign aliceSk snapshotFixture)
      accepted `shouldSatisfy` \case Continue{} -> True; _ -> False
      final `shouldSatisfy` \case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False
      aggregateState afterBob final `shouldBe` afterBob
      corrected `shouldSatisfy` \case Error (RequireFailed SnapshotAlreadySigned{}) -> True; _ -> False
      legacyOnOpenNetworkAckSn env initial.chainPointTime mempty (openState afterBob) alice (sign aliceSk snapshotFixture) 1 `shouldBe` corrected

initial :: NodeState SimpleTx
initial = initialNode{headState = Open os}

os :: OpenState SimpleTx
os = base{coordinatedHeadState = c{allTxs = Map.fromList [(txId tx, tx) | tx <- [transactionOne, transactionTwo]], localTxs = Seq.fromList [transactionOne, transactionTwo], localUTxO = utxoRef 3}}
 where
  base = openState initialNode
  c = base.coordinatedHeadState

-- | Reuse the same assertions for deterministic boundaries and shrinking
-- properties. Generate both numeric ranges explicitly so large values are not
-- lost to QuickCheck's usual size limits.
versionCases :: String -> (SnapshotVersion -> Expectation) -> Spec
versionCases description assertVersion = describe description $ do
  it "covers boundary versions" $
    forM_ [0, 1, word64Boundary - 1, word64Boundary, word64Boundary + 1, 2 ^ (80 :: Int)] assertVersion
  prop "covers generated versions" $
    checkCoverage $
      forAllShrink genVersion shrinkVersion $ \version ->
        cover 30 (version < word64Boundary) "within 64 bits" $
          cover 30 (version >= word64Boundary) "above 64 bits" $
            ioProperty $
              assertVersion version $> True

word64Boundary :: SnapshotVersion
word64Boundary = 2 ^ (64 :: Int)

genVersion :: Gen SnapshotVersion
genVersion =
  fromInteger
    <$> oneof
      [ chooseInteger (0, toInteger word64Boundary - 1)
      , chooseInteger (toInteger word64Boundary, 2 ^ (128 :: Int) - 1)
      ]

shrinkVersion :: SnapshotVersion -> [SnapshotVersion]
shrinkVersion = fmap fromInteger . filter (>= 0) . shrinkIntegral . toInteger

-- | Derive relationships after shrinking the base version. Natural versions
-- have no predecessor at zero.
requestVersions :: SnapshotVersion -> [SnapshotVersion]
requestVersions version = [0, version, version + 1] <> [version - 1 | version > 0]

env :: Environment
env = environment 0

snapshotFixture :: Snapshot SimpleTx
snapshotFixture = case (openState $ aggregateState initial $ runReqSn env simpleLedger mempty initial.chainPointTime os 1 alice 0 1 [1] Nothing Nothing).coordinatedHeadState.seenSnapshot of
  SeenSnapshot{snapshot} -> snapshot
  _ -> error "snapshot fixture did not request"

ackState :: SnapshotPhase -> [Int] -> NodeState SimpleTx
ackState phase signers = initial{headState = Open os{coordinatedHeadState = c{localTxs = Seq.singleton transactionTwo, seenSnapshot = seen}}}
 where
  c = os.coordinatedHeadState
  seen = case phase of
    NeverSeen -> NoSeenSnapshot
    RequestPending -> RequestedSnapshot 1 2
    CollectingSignatures -> case mkSeenSnapshot snapshotFixture mempty of
      SeenSnapshot{signableBytes} -> SeenSnapshot snapshotFixture (Map.fromList [(scenarioParties !! i, sign ([aliceSk, bobSk, carolSk] !! i) snapshotFixture) | i <- signers]) signableBytes
      _ -> error "mkSeenSnapshot did not construct a seen snapshot"
    LastConfirmed -> LastSeenSnapshot 1

depositFixture :: DepositStatus -> Deposit SimpleTx
depositFixture = Deposit testHeadId (utxoRef 100) initialChainTime (addUTCTime 3600 initialChainTime)

data SnapshotPhase = NeverSeen | RequestPending | CollectingSignatures | LastConfirmed
  deriving stock (Enum, Bounded)

scenarioParties :: [Party]
scenarioParties = [alice, bob, carol]

environment :: Int -> Environment
environment n = testEnvironment{party = scenarioParties !! n, signingKey = [aliceSk, bobSk, carolSk] !! n}

transactionOne, transactionTwo :: SimpleTx
transactionOne = SimpleTx 1 (Set.singleton 1) (Set.singleton 2)
transactionTwo = SimpleTx 2 (Set.singleton 2) (Set.singleton 3)

initialSnapshot :: Snapshot SimpleTx
initialSnapshot =
  Snapshot
    { headId = testHeadId
    , version = 0
    , number = 0
    , confirmed = []
    , utxo = Set.singleton 1
    , utxoToCommit = Nothing
    , depositTxId = Nothing
    , utxoToDecommit = Nothing
    , accumulator = Accumulator.buildFromUTxO @SimpleTx (Set.singleton 1)
    , appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (Set.singleton 1)
    }

initialNode :: NodeState SimpleTx
initialNode =
  NodeInSync
    { headState =
        Open
          OpenState
            { parameters = HeadParameters cperiod dperiod scenarioParties
            , chainState = 0
            , headId = testHeadId
            , headSeed = testHeadSeed
            , coordinatedHeadState =
                CoordinatedHeadState
                  { localUTxO = Set.singleton 1
                  , localTxs = mempty
                  , allTxs = mempty
                  , confirmedSnapshot = ConfirmedSnapshot initialSnapshot (aggregateInOrder (Map.fromList [(p, sign sk initialSnapshot) | (p, sk) <- zip scenarioParties [aliceSk, bobSk, carolSk]]) scenarioParties)
                  , seenSnapshot = NoSeenSnapshot
                  , currentDepositTxId = Nothing
                  , decommitTx = Nothing
                  , version = 0
                  , finalizedCommit = Nothing
                  , finalizedDecommit = Nothing
                  }
            }
    , deposits = mempty
    , chainPointTime = ChainPointTime 0 initialChainTime 0
    }

openState :: NodeState SimpleTx -> OpenState SimpleTx
openState st = case st.headState of
  Open opened -> opened
  _ -> error "snapshot fixture leaves the open phase"

runReqTx ::
  forall tx.
  IsTx tx =>
  Environment ->
  Ledger tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
runReqTx handlerEnv ledger chainTime st ttl pending tx =
  Interpreter.runProgramWith (Interpreter.snapshotConfig handlerEnv ledger) $
    HeadLogic.onOpenNetworkReqTx handlerEnv chainTime st ttl pending tx

runReqSn ::
  forall tx.
  IsTx tx =>
  Environment ->
  Ledger tx ->
  PendingDeposits tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot version.
  SnapshotVersion ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  -- | Optional commit of additional funds into the head.
  Maybe (TxIdType tx) ->
  Outcome tx
runReqSn handlerEnv ledger pending chainTime st ttl sender sv sn txs decommit deposit =
  Interpreter.runProgramWith (Interpreter.snapshotConfig handlerEnv ledger) $
    HeadLogic.onOpenNetworkReqSn handlerEnv pending chainTime st ttl sender sv sn txs decommit deposit

runAckSn ::
  Environment ->
  ChainPointTime ->
  PendingDeposits SimpleTx ->
  OpenState SimpleTx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot SimpleTx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome SimpleTx
runAckSn handlerEnv chainTime pending st sender signature sn =
  Interpreter.runProgramWith (Interpreter.snapshotConfig handlerEnv simpleLedger) $
    HeadLogic.onOpenNetworkAckSn handlerEnv chainTime pending st sender signature sn
