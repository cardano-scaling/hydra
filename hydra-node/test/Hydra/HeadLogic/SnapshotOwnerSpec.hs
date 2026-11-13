{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.SnapshotOwnerSpec (spec) where

import Hydra.Prelude
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)

import Crem.Topology (Topology (Topology))
import Data.Map.Strict qualified as Map
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic.Error (LogicError (AssertionFailed, RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Machine.Coordinated qualified as Owner
import Hydra.HeadLogic.Machine.Coordinated.Topology (SnapshotPhase, snapshotTopology)
import Hydra.HeadLogic.Outcome (Outcome (Continue, Error), StateChanged (DecommitFinalized, PartySignedSnapshot, SnapshotConfirmed, SnapshotRequestDecided, SnapshotRequested))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotVerification (verifyCandidate, verifySeenSnapshot)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (RequestedSnapshot, SeenSnapshot), mkSeenSnapshot)
import Hydra.Ledger.Simple (SimpleChainState (SimpleChainState), SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (AckSn), NetworkEvent (ReceivedMessage))
import Hydra.Node.HeadLogic qualified as Head
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature, aggregateInOrder, getSignableRepresentation, sign)
import Hydra.Tx.HeadId (HeadId (UnsafeHeadId))
import Hydra.Tx.HeadParameters (HeadParameters (parties))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..))
import Hydra.Tx.Snapshot qualified as Snapshot
import Test.Hydra.HeadLogic.DecommitSequences qualified as Sequences
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)

spec :: Spec
spec = describe "live snapshot ownership" $ do
  it "checks 960 decommit, acknowledgement and rollback schedules at all three parties" $ do
    length Sequences.schedules `shouldBe` 960
    forM_ Sequences.schedules $ \schedule ->
      either (fail . toString) (const $ pure ()) $ Sequences.runSchedule schedule

  it "admits the next follower candidate and refuses replacement while collecting" $ do
    let command = Owner.acceptSnapshot snapshot mempty Nothing
    run (void $ Owner.stage initial command) `shouldBe` Continue [SnapshotRequested snapshot mempty Nothing] []
    forM_ [snapshot, different, snapshot{number = 2}] $ \replacement ->
      run (void $ Owner.stage collecting $ Owner.acceptSnapshot replacement mempty Nothing) `shouldSatisfy` refused

  it "refuses a foreign Head, wrong version, skipped round or conflicting request marker" $ do
    forM_ ([snapshot{Snapshot.headId = UnsafeHeadId "other"}, snapshot{Snapshot.version = 1}, snapshot{number = 2}] :: [Snapshot SimpleTx]) $ \invalid ->
      run (void $ Owner.stage initial $ Owner.acceptSnapshot invalid mempty Nothing) `shouldSatisfy` refused
    let requested = Owner.replayEvent mempty (SnapshotRequestDecided 2) initial
    run (void $ Owner.stage requested $ Owner.acceptSnapshot snapshot mempty Nothing) `shouldSatisfy` refused

  it "binds signature recording to candidate content, version and number" $
    forM_ ([different, snapshot{Snapshot.version = 1}, snapshot{number = 2}] :: [Snapshot SimpleTx]) $ \other -> do
      let stale = owned $ owner other
          program = do
            staged <- Owner.stage collecting $ Owner.recordSignature (owned collecting) alice (sign aliceSk snapshot)
            void $ Owner.stage staged $ Owner.recordSignature stale bob (sign bobSk other)
      run program `shouldSatisfy` refused

  it "refuses a replacement signature and discards the attempt's staged prefix" $ do
    let program :: Program.Program SimpleTx ()
        program = do
          staged <- Owner.stage collecting $ Owner.recordSignature (owned collecting) alice (sign aliceSk snapshot)
          void $ Owner.stage staged $ Owner.recordSignature (owned staged) alice (sign aliceSk different)
    run program `shouldSatisfy` refused

  it "refuses stale confirmation evidence even after staging a current signature" $ do
    let program :: Program.Program SimpleTx ()
        program = do
          evidence <- verifyCandidate (owned $ owner different) (signatures different) >>= either Program.reject pure
          staged <- Owner.stage collecting $ Owner.recordSignature (owned collecting) alice (sign aliceSk snapshot)
          void $ Owner.stage staged $ Owner.confirmSnapshot evidence
    run program `shouldSatisfy` refused

  it "keeps a candidate handle valid when settlement precedes its final acknowledgement" $ do
    let ownedCandidate = owned collecting
        program :: Program.Program SimpleTx ()
        program = do
          settled <- Owner.stage collecting $ Owner.settleDecommit (SimpleChainState 2) 1 mempty
          evidence <- verifyCandidate ownedCandidate (signatures snapshot) >>= either Program.reject pure
          void $ Owner.stage settled $ Owner.confirmSnapshot evidence
    run program `shouldSatisfy` \case
      Continue [DecommitFinalized{}, SnapshotConfirmed{}] [] -> True
      _ -> False

  it "retires a confirmed candidate and exposes the next round's candidate" $ do
    let nextSnapshot = snapshot{number = 2}
        program :: Program.Program SimpleTx ()
        program = do
          firstEvidence <- verifyCandidate (owned collecting) (signatures snapshot) >>= either Program.reject pure
          confirmed <- Owner.stage collecting $ Owner.confirmSnapshot firstEvidence
          when (isJust $ Owner.candidate confirmed) $
            Program.reject $
              AssertionFailed "confirmed owner still exposes a candidate"
          next <- Owner.stage confirmed $ Owner.acceptSnapshot nextSnapshot mempty Nothing
          nextEvidence <- verifyCandidate (owned next) (signatures nextSnapshot) >>= either Program.reject pure
          finished <- Owner.stage next $ Owner.confirmSnapshot nextEvidence
          when (isJust $ Owner.candidate finished) $
            Program.reject $
              AssertionFailed "second confirmation retains a candidate"
    run program `shouldSatisfy` \case
      Continue [SnapshotConfirmed{}, SnapshotRequested{}, SnapshotConfirmed{}] [] -> True
      _ -> False

  it "imports historical request markers without adding an edge to the live graph" $ do
    let restored = Owner.replayEvent mempty (SnapshotRequestDecided 2) collecting
    (Owner.currentState restored).seenSnapshot `shouldBe` RequestedSnapshot 1 2
    let imported = Owner.replayEvent mempty (SnapshotRequested different mempty Nothing) collecting
    (Owner.currentState imported).seenSnapshot `shouldBe` mkSeenSnapshot different mempty

  it "does not confirm one snapshot using another snapshot's cached signing bytes" $ do
    let before = withCache different different
        input = Head.NetworkInput 0 $ ReceivedMessage carol $ AckSn (sign carolSk different) 1
        outcome = Head.update (Scenario.environment 0) simpleLedger initialChainTime before input
    outcome `shouldSatisfy` invalidSignatures
    Head.aggregateState before outcome `shouldBe` before

  it "verifies the owned contents even when the stored signing-byte cache is wrong" $ do
    let before = withCache different snapshot
        outcome =
          Head.update (Scenario.environment 0) simpleLedger initialChainTime before $
            Head.NetworkInput 0 $
              ReceivedMessage carol $
                AckSn (sign carolSk snapshot) 1
    outcome `shouldSatisfy` \case
      Continue [PartySignedSnapshot{}, SnapshotConfirmed{}] _ -> True
      _ -> False
    (Scenario.openState $ Head.aggregateState before outcome).coordinatedHeadState.confirmedSnapshot
      `shouldBe` ConfirmedSnapshot snapshot (signatures snapshot)

  it "chain adoption also verifies contents rather than a supplied byte cache" $ do
    let supplied = SeenSnapshot snapshot mempty (getSignableRepresentation different)
        program :: Program.Program SimpleTx ()
        program = verifySeenSnapshot (vkey <$> Scenario.scenarioParties) supplied (signatures different) >>= either Program.reject (const $ pure ())
    run program `shouldSatisfy` invalidSignatures

  it "matches the Agda live graph including Crem's implicit self transitions" $ do
    let Topology edges = snapshotTopology
    forM_ ([minBound .. maxBound] :: [SnapshotPhase]) $ \from ->
      forM_ [minBound .. maxBound] $ \to ->
        (from == to || any (\(source, destinations) -> source == from && to `elem` destinations) edges)
          `shouldBe` Agda.liveSnapshotEdgeRef (fromIntegral $ fromEnum from) (fromIntegral $ fromEnum to)

  it "matches Agda on 96 ownership and party ordering combinations with real signatures" $
    forM_ [False, True] $ \collects ->
      forM_ [False, True] $ \foreignHead ->
        forM_ [0, 1] $ \version ->
          forM_ [1, 2] $ \number ->
            forM_ [False, True] $ \otherContents ->
              forM_ [[0, 1, 2], [2, 1, 0], [0, 1]] $ \members -> do
                let base = if otherContents then different else snapshot
                    supplied :: Snapshot SimpleTx
                    supplied = base{Snapshot.headId = if foreignHead then UnsafeHeadId "other" else snapshot.headId, Snapshot.version = version, number}
                    signers = [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]
                    selected = [entry | i <- members, (j, entry) <- zip [0 :: Integer ..] signers, i == j]
                    open = Scenario.openState $ node supplied
                    parameters :: HeadParameters
                    parameters = open.parameters{parties = fst <$> selected}
                    issuer = Owner.start open{headId = supplied.headId, parameters}
                    proof = aggregateInOrder (Map.fromList [(p, sign sk supplied) | (p, sk) <- selected]) (fst <$> selected)
                    program = do
                      verified <- verifyCandidate (owned issuer) proof >>= either Program.reject pure
                      void $ Owner.stage (if collects then collecting else initial) $ Owner.confirmSnapshot verified
                    accepted = case run program of Continue [SnapshotConfirmed{}] [] -> True; _ -> False
                    suppliedIdentity = [if foreignHead then 1 else 0, fromIntegral version, fromIntegral number, if otherContents then 1 else 0]
                accepted `shouldBe` Agda.candidateConfirmationRef collects [0, 0, 1, 0] suppliedIdentity [0, 1, 2] members

run :: Program.Program SimpleTx () -> Outcome SimpleTx
run = Interpreter.runProgram (testInterpreterConfig simpleLedger)

refused :: Outcome SimpleTx -> Bool
refused = \case Error AssertionFailed{} -> True; _ -> False

invalidSignatures :: Outcome SimpleTx -> Bool
invalidSignatures = \case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False

initial :: Owner.Runtime SimpleTx
initial = Owner.start $ Scenario.openState Scenario.initialNode

collecting :: Owner.Runtime SimpleTx
collecting = owner snapshot

owned :: Owner.Runtime SimpleTx -> Owner.Candidate SimpleTx
owned = fromMaybe (error "fixture has no owned candidate") . Owner.candidate

owner :: Snapshot SimpleTx -> Owner.Runtime SimpleTx
owner candidate = Owner.start $ Scenario.openState $ node candidate

node :: Snapshot SimpleTx -> NodeState SimpleTx
node candidate =
  let open = Scenario.openState Scenario.initialNode
      coordinated = open.coordinatedHeadState{localUTxO = candidate.utxo, seenSnapshot = mkSeenSnapshot candidate mempty}
   in Scenario.initialNode{headState = Open open{coordinatedHeadState = coordinated}}

snapshot, different :: Snapshot SimpleTx
snapshot = snapshotFor $ SimpleTx 1 (utxoRef 1) (utxoRef 2)
different = snapshotFor $ SimpleTx 2 (utxoRef 1) (utxoRef 3)

snapshotFor :: SimpleTx -> Snapshot SimpleTx
snapshotFor tx = Scenario.initialSnapshot{number = 1, confirmed = [tx], utxo = tx.txOutputs, accumulator = Accumulator.buildFromUTxO @SimpleTx tx.txOutputs, appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx tx.txOutputs}

signatures :: Snapshot SimpleTx -> MultiSignature (Snapshot SimpleTx)
signatures candidate = aggregateInOrder (Map.fromList [(alice, sign aliceSk candidate), (bob, sign bobSk candidate), (carol, sign carolSk candidate)]) Scenario.scenarioParties

withCache :: Snapshot SimpleTx -> Snapshot SimpleTx -> NodeState SimpleTx
withCache cached signed =
  let open = Scenario.openState $ node snapshot
      seen = SeenSnapshot snapshot (Map.fromList [(alice, sign aliceSk signed), (bob, sign bobSk signed)]) (getSignableRepresentation cached)
   in Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = seen}}}
