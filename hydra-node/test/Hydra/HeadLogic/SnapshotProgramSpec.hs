{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.SnapshotProgramSpec (spec) where

import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Hydra.HeadLogic hiding (error)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Node (defaultTxTTL)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (..), NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state, trace)
import Hydra.Tx (txId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.Hydra.HeadLogic.LegacyReqTx (legacyOnOpenNetworkReqTx)
import Test.Hydra.HeadLogic.LegacySnapshot
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Node.Fixture
import Test.Hydra.Prelude hiding (pending)

spec :: Spec
spec = do
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
                          new = onOpenNetworkReqSn env simpleLedger pending initial.chainPointTime os ttl sender sv sn ids decommit deposit
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
                    new = onOpenNetworkAckSn (environment 1) initial.chainPointTime mempty (openState state) (scenarioParties !! sender) signature sn
                new `shouldBe` old
                aggregateState state new `shouldBe` aggregateState state old

    it "preserves increment and decrement approval ordering at confirmation" $ do
      forM_ [False, True] $ \decommitting -> do
        let pending = if decommitting then mempty else Map.singleton 7 (depositFixture Active)
            base = os{coordinatedHeadState = os.coordinatedHeadState{decommitTx = if decommitting then Just transactionOne else Nothing}}
            ids = [1 | not decommitting]
            req = onOpenNetworkReqSn env simpleLedger pending initial.chainPointTime base 1 alice 0 1 ids (if decommitting then Just transactionOne else Nothing) (if decommitting then Nothing else Just 7)
            afterRequest = aggregateState initial{headState = Open base} req
            afterOpen = openState afterRequest
        case afterOpen.coordinatedHeadState.seenSnapshot of
          SeenSnapshot{snapshot} -> do
            let seen = mkSeenSnapshot snapshot (Map.fromList [(alice, sign aliceSk snapshot), (bob, sign bobSk snapshot)])
                ready = afterOpen{coordinatedHeadState = afterOpen.coordinatedHeadState{seenSnapshot = seen}}
                old = legacyOnOpenNetworkAckSn (environment 1) initial.chainPointTime pending ready carol (sign carolSk snapshot) 1
                new = onOpenNetworkAckSn (environment 1) initial.chainPointTime pending ready carol (sign carolSk snapshot) 1
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

    it "pins two ordered records on a partial acknowledgement" $ do
      let signature = sign aliceSk snapshotFixture
          outcome = onOpenNetworkAckSn env initial.chainPointTime mempty (openState $ ackState CollectingSignatures []) alice signature 1
          event = PartySignedSnapshot 1 alice signature
      outcome `shouldBe` Continue [event, event] []

  describe "registry and acknowledgement regressions" $ do
    it "preserves the validated payload and ignores every retry of an applied transaction" $ do
      let node = aggregateState initialNode $ onOpenNetworkReqTx (environment 1) simpleLedger initialNode.chainPointTime (openState initialNode) 2 mempty transactionOne
          c = (openState node).coordinatedHeadState
          request = onOpenNetworkReqSn (environment 1) simpleLedger mempty node.chainPointTime (openState node) 2 alice 0 1 [1] Nothing Nothing
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
          applyAck node sender signature = onOpenNetworkAckSn env node.chainPointTime mempty (openState node) sender signature 1
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

env :: Environment
env = environment 0

snapshotFixture :: Snapshot SimpleTx
snapshotFixture = case (openState $ aggregateState initial $ onOpenNetworkReqSn env simpleLedger mempty initial.chainPointTime os 1 alice 0 1 [1] Nothing Nothing).coordinatedHeadState.seenSnapshot of
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
