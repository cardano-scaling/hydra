{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.SnapshotOwnerSpec (spec) where

import Hydra.HeadLogic.Program (reject)
import Hydra.HeadLogic.Rules.Coordinated qualified as Staging
import Hydra.HeadLogic.Rules.Network.AckSn qualified as AckSn
import Hydra.Prelude
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.HeadLogic.Validated (checkedSnapshot)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Binary (decodeFull', serialize')
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (foldM)
import Crem.Topology (Topology (Topology))
import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.Map.Strict qualified as Map
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic.Error (LogicError (AssertionFailed, RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Outcome (Outcome (Continue, Error), StateChanged (DecommitFinalized, PartySignedSnapshot, SnapshotConfirmed, SnapshotRequestDecided, SnapshotRequested))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Verification (verifyCandidate, verifySeenSnapshot)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (RequestedSnapshot, SeenSnapshot, cachedSnapshot, signatories), mkSeenSnapshot)
import Hydra.HeadLogic.State.Coordinated qualified as Owner
import Hydra.HeadLogic.State.Coordinated.Snapshots.Topology (SnapshotPhase, snapshotTopology)
import Hydra.HeadLogic.Workflow qualified as Workflow
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
import System.Mem.StableName (makeStableName)
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
    let command = Owner.acceptSnapshot (checkedSnapshot snapshot) mempty
    run (void $ Staging.stage initial command) `shouldBe` Continue [SnapshotRequested snapshot mempty Nothing] []
    forM_ [snapshot, different, snapshot{number = 2}] $ \replacement ->
      run (void $ Staging.stage collecting $ Owner.acceptSnapshot (checkedSnapshot replacement) mempty) `shouldSatisfy` refused

  it "refuses a foreign Head, wrong version, skipped round or conflicting request marker" $ do
    forM_ ([snapshot{Snapshot.headId = UnsafeHeadId "other"}, snapshot{Snapshot.version = 1}, snapshot{number = 2}] :: [Snapshot SimpleTx]) $ \invalid ->
      run (void $ Staging.stage initial $ Owner.acceptSnapshot (checkedSnapshot invalid) mempty) `shouldSatisfy` refused
    let requested = Owner.replayEvent mempty (SnapshotRequestDecided 2) initial
    run (void $ Staging.stage requested $ Owner.acceptSnapshot (checkedSnapshot snapshot) mempty) `shouldSatisfy` refused

  it "binds signature recording to candidate content, version and number" $
    forM_ ([different, snapshot{Snapshot.version = 1}, snapshot{number = 2}] :: [Snapshot SimpleTx]) $ \other -> do
      let stale = owned $ owner other
          program = do
            staged <- Staging.stage collecting $ Owner.recordSignature (owned collecting) alice (sign aliceSk snapshot)
            void $ Staging.stage staged $ Owner.recordSignature stale bob (sign bobSk other)
      run program `shouldSatisfy` refused

  it "refuses a replacement signature and discards the attempt's staged prefix" $ do
    let program :: Program.Program SimpleTx ()
        program = do
          staged <- Staging.stage collecting $ Owner.recordSignature (owned collecting) alice (sign aliceSk snapshot)
          void $ Staging.stage staged $ Owner.recordSignature (owned staged) alice (sign aliceSk different)
    run program `shouldSatisfy` refused

  it "refuses stale confirmation evidence even after staging a current signature" $ do
    let program :: Program.Program SimpleTx ()
        program = do
          evidence <- verifyCandidate (owned $ owner different) (signatures different) >>= either reject pure
          staged <- Staging.stage collecting $ Owner.recordSignature (owned collecting) alice (sign aliceSk snapshot)
          void $ Staging.stage staged $ Owner.confirmSnapshot evidence
    run program `shouldSatisfy` refused

  it "keeps a candidate handle valid when settlement precedes its final acknowledgement" $ do
    let ownedCandidate = owned collecting
        program :: Program.Program SimpleTx ()
        program = do
          settled <- Staging.stage collecting $ Owner.recordDecommitFinalization (SimpleChainState 2) 1 mempty
          evidence <- verifyCandidate ownedCandidate (signatures snapshot) >>= either reject pure
          void $ Staging.stage settled $ Owner.confirmSnapshot evidence
    run program `shouldSatisfy` \case
      Continue [DecommitFinalized{}, SnapshotConfirmed{}] [] -> True
      _ -> False

  it "retires a confirmed candidate and exposes the next round's candidate" $ do
    let nextSnapshot = snapshot{number = 2}
        program :: Program.Program SimpleTx ()
        program = do
          firstEvidence <- verifyCandidate (owned collecting) (signatures snapshot) >>= either reject pure
          confirmed <- Staging.stage collecting $ Owner.confirmSnapshot firstEvidence
          when (isJust $ Owner.candidate confirmed) $
            reject $
              AssertionFailed "confirmed owner still exposes a candidate"
          next <- Staging.stage confirmed $ Owner.acceptSnapshot (checkedSnapshot nextSnapshot) mempty
          nextEvidence <- verifyCandidate (owned next) (signatures nextSnapshot) >>= either reject pure
          finished <- Staging.stage next $ Owner.confirmSnapshot nextEvidence
          when (isJust $ Owner.candidate finished) $
            reject $
              AssertionFailed "second confirmation retains a candidate"
    run program `shouldSatisfy` \case
      Continue [SnapshotConfirmed{}, SnapshotRequested{}, SnapshotConfirmed{}] [] -> True
      _ -> False

  it "imports historical request markers without adding an edge to the live graph" $ do
    let restored = Owner.replayEvent mempty (SnapshotRequestDecided 2) collecting
    (Owner.currentState restored).seenSnapshot `shouldBe` RequestedSnapshot 1 2
    let imported = Owner.replayEvent mempty (SnapshotRequested different mempty Nothing) collecting
    (Owner.currentState imported).seenSnapshot `shouldBe` mkSeenSnapshot different mempty

  it "does not confirm one snapshot using another snapshot's signatures" $ do
    let before = withSignatures different
        input = Head.NetworkInput 0 $ ReceivedMessage carol $ AckSn (sign carolSk different) 1
        outcome = Head.update (Scenario.environment 0) simpleLedger initialChainTime before input
    outcome `shouldSatisfy` invalidSignatures
    Head.aggregateState before outcome `shouldBe` before

  it "shares the cached snapshot through restoration, partial acknowledgements and aggregation" $ do
    let before = node snapshot
        cache st = case (Scenario.openState st).coordinatedHeadState.seenSnapshot of
          SeenSnapshot{cachedSnapshot} -> cachedSnapshot
          _ -> error "expected a collecting snapshot"
    cached <- evaluate (cache before) >>= makeStableName
    bytes <- evaluate (Cached.signingBytes $ cache before) >>= makeStableName
    let checkCache retained = do
          retainedName <- evaluate retained >>= makeStableName
          (retainedName == cached) `shouldBe` True
          representation <- evaluate (Cached.signingBytes retained) >>= makeStableName
          (representation == bytes) `shouldBe` True
    after <-
      foldM
        ( \st (party, sk) -> do
            let runtime = Owner.start $ Scenario.openState st
                (events, staged) = Owner.step runtime $ Owner.recordSignature (owned runtime) party (sign sk snapshot)
            events `shouldBe` [PartySignedSnapshot 1 party (sign sk snapshot)]
            checkCache (Owner.currentState staged).seenSnapshot.cachedSnapshot
            let outcome = Head.update (Scenario.environment 0) simpleLedger initialChainTime st $ Head.NetworkInput 0 $ ReceivedMessage party $ AckSn (sign sk snapshot) 1
            outcome `shouldBe` Continue [PartySignedSnapshot 1 party (sign sk snapshot)] []
            let next = Head.aggregateState st outcome
            checkCache $ cache next
            pure next
        )
        before
        [(alice, aliceSk), (bob, bobSk)]
    let outcome =
          Head.update (Scenario.environment 0) simpleLedger initialChainTime after $
            Head.NetworkInput 0 $
              ReceivedMessage carol $
                AckSn (sign carolSk snapshot) 1
    outcome `shouldSatisfy` \case
      Continue [PartySignedSnapshot{}, SnapshotConfirmed{}] _ -> True
      _ -> False
    (Scenario.openState $ Head.aggregateState after outcome).coordinatedHeadState.confirmedSnapshot
      `shouldBe` ConfirmedSnapshot snapshot (signatures snapshot)

  it "preserves JSON and CBOR layouts and reconstructs canonical signing bytes" $ do
    let signatories = Map.singleton alice (sign aliceSk snapshot)
        seen = mkSeenSnapshot snapshot signatories
        json = object ["tag" .= ("SeenSnapshot" :: Text), "snapshot" .= snapshot, "signatories" .= signatories]
        cbor = toStrictByteString $ toCBOR ("SeenSnapshot" :: Text) <> toCBOR snapshot <> toCBOR signatories
    toJSON seen `shouldBe` json
    serialize' seen `shouldBe` cbor
    forM_ [eitherDecode (encode json), first show $ decodeFull' cbor] $ \case
      Right SeenSnapshot{cachedSnapshot, signatories = retained} -> do
        Cached.snapshot cachedSnapshot `shouldBe` snapshot
        Cached.signingBytes cachedSnapshot `shouldBe` getSignableRepresentation snapshot
        retained `shouldBe` signatories
      other -> fail $ "failed to decode a collecting snapshot: " <> show other

  it "does not demand signing bytes when persisting a snapshot to CBOR" $ do
    let poisoned = snapshot{accumulator = error "CBOR forces signing bytes", appliedAccumulator = error "CBOR forces signing bytes"}
    decodeFull' (serialize' $ mkSeenSnapshot poisoned mempty) `shouldBe` Right (mkSeenSnapshot snapshot mempty)

  it "confirms the owned contents using their cached signing bytes" $ do
    let before = withSignatures snapshot
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

  it "does not select a chained request when this party is not the next leader" $ do
    let before = withSignatures snapshot
        program =
          Workflow.run $
            AckSn.workflow
              AckSn.Args
                { env = Head.ruleEnvironment $ Scenario.environment 0
                , chainPointTime = before.chainPointTime
                , pendingDeposits = error "a follower must not inspect deposits to select a request"
                , openState = Scenario.openState before
                , sender = carol
                , signed = sign carolSk snapshot
                , snapshotNumber = 1
                }
    run program `shouldSatisfy` \case
      Continue [PartySignedSnapshot{}, SnapshotConfirmed{}] [] -> True
      _ -> False

  it "chain adoption rejects another snapshot's signatures" $ do
    let supplied = mkSeenSnapshot snapshot mempty
        program :: Program.Program SimpleTx ()
        program = verifySeenSnapshot (vkey <$> Scenario.scenarioParties) supplied (signatures different) >>= either reject (const $ pure ())
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
                      verified <- verifyCandidate (owned issuer) proof >>= either reject pure
                      void $ Staging.stage (if collects then collecting else initial) $ Owner.confirmSnapshot verified
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

withSignatures :: Snapshot SimpleTx -> NodeState SimpleTx
withSignatures signed =
  let open = Scenario.openState $ node snapshot
      seen = mkSeenSnapshot snapshot (Map.fromList [(alice, sign aliceSk signed), (bob, sign bobSk signed)])
   in Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = seen}}}
