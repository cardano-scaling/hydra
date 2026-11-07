{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.CoordinatedSpec (spec) where

import Hydra.Prelude hiding (keys)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Binary (decodeFull', serialize')
import Crem.Inspect (Component (..), checkStates, inspectStates)
import Crem.StateInspection (StateCheck (NotInspected))
import Crem.StateMachine qualified as Crem
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.API.ClientInput qualified as Client
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic (aggregateNodeState)
import Hydra.HeadLogic qualified as Head
import Hydra.HeadLogic.Error (LogicError (AssertionFailed, RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Outcome (Continue, Error, Wait), StateChanged (..), WaitReason (WaitOnSeenSnapshot))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotRequest (DecommitStatus (..), chainedSnapshotRequest, decommitStatus, nextSnapshotRequest)
import Hydra.HeadLogic.Rules.SnapshotVerification (verifyCandidate)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), OpenState (..), SeenSnapshot (SeenSnapshot), mkSeenSnapshot)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (ReqDec), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (Deposit (..), DepositStatus (Active), NodeState (..), initialChainTime, trackedFromPending)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.HeadId (HeadId (UnsafeHeadId))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.ConflictingDecommit qualified as Conflict
import Test.Hydra.HeadLogic.CoordinatedAudit qualified as Audit
import Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, testHeadId)

spec :: Spec
spec = describe "coordinated state owners" $ do
  it "inspects retained owner phases without rebuilding the local UTxO" $ do
    let initial = Crem.withoutRendering $ Coordinated.components open
        (_, applied) = runIdentity $ Crem.run initial $ Coordinated.applyTransaction Scenario.transactionOne (error "inspection demands before UTxO") (error "inspection demands after UTxO")
        (_, collecting) = runIdentity $ Crem.run applied $ Coordinated.acceptSnapshot candidate (Seq.singleton $ error "inspection rebuilds transactions") Nothing
    inspectStates applied `shouldBe` [([LeftComponent], Just "AwaitingRequest"), ([RightComponent], Just "NoDecommit")]
    inspectStates collecting `shouldBe` [([LeftComponent], Just "CollectingSignatures"), ([RightComponent], Just "NoDecommit")]
    checks <- checkStates [] collecting
    checks `shouldSatisfy` \case
      [(_, NotInspected), (_, NotInspected)] -> True
      _ -> False

  it "inspects reservation and certificate phases after reconstruction" $ do
    let pending = open{coordinatedHeadState = open.coordinatedHeadState{decommitTx = Just Scenario.transactionOne}}
        certified = open{coordinatedHeadState = open.coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot candidate{utxoToDecommit = Just $ utxoRef 2} signatures}}
        descriptions = map (inspectStates . Crem.withoutRendering . Coordinated.components) [open, pending, certified]
    descriptions
      `shouldBe` [ [([LeftComponent], Just "AwaitingRequest"), ([RightComponent], Just phaseName)]
                 | phaseName <- ["NoDecommit", "DecommitPending", "AwaitingSettlement"]
                 ]

  it "compares the independent ownership checks with Agda over bounded UTxOs" $
    forM_ [(reserved, available, confirmed, carried) | reserved <- subsequences [1, 2, 3], available <- subsequences [1, 2, 3], confirmed <- subsequences [1, 2, 3], carried <- [False, True]] $ \(reserved, available, confirmed, carried) -> do
      let tx = SimpleTx 3 (fromList $ fromInteger <$> reserved) (utxoRef 4)
          base :: Snapshot SimpleTx
          base = Scenario.initialSnapshot{utxo = fromList $ fromInteger <$> confirmed, utxoToDecommit = if carried then Just tx.txOutputs else Nothing}
          chs = open.coordinatedHeadState{decommitTx = Just tx, localUTxO = fromList $ fromInteger <$> available, confirmedSnapshot = ConfirmedSnapshot base mempty}
      Audit.reservationHeld chs `shouldBe` Agda.reservationHeldRef reserved available
      Audit.decommitReady chs tx `shouldBe` Agda.decommitReadyRef carried reserved confirmed
      let eligible = case decommitStatus 0 base (Just tx) of
            ReadyToDecommit{} -> True
            CarryDecommit{} -> True
            _ -> False
      eligible `shouldBe` Agda.decommitReadyRef carried reserved confirmed
      isJust (chainedSnapshotRequest 0 base mempty mempty Nothing (decommitStatus 0 base $ Just tx) Nothing)
        `shouldBe` Agda.newDecommitReadyRef carried reserved confirmed
      let rebased :: Snapshot SimpleTx
          rebased = base{utxo = fromList $ fromInteger <$> available}
          (_, next) = Coordinated.step (Coordinated.start open{coordinatedHeadState = chs}) $ Coordinated.acceptSnapshot rebased mempty Nothing
      (Coordinated.currentState next).localUTxO `shouldBe` fromList (fromInteger <$> Agda.reserveRebaseRef reserved available)

  it "agrees between continuous replay and checkpoint recovery" $ do
    let events =
          [ TransactionReceived Scenario.transactionOne
          , TransactionAppliedToLocalUTxO testHeadId Scenario.transactionOne
          , TransactionReceived Scenario.transactionTwo
          , TransactionAppliedToLocalUTxO testHeadId Scenario.transactionTwo
          , DecommitRecorded testHeadId (SimpleTx 3 (utxoRef 3) (utxoRef 4))
          , SnapshotRequestDecided 1
          , SnapshotRequested candidate (Seq.singleton Scenario.transactionTwo) Nothing
          , PartySignedSnapshot 1 alice (sign aliceSk candidate)
          , PartySignedSnapshot 1 bob (sign bobSk candidate)
          , PartySignedSnapshot 1 carol (sign carolSk candidate)
          , SnapshotConfirmed testHeadId Nothing signatures
          ]
    void $ foldlM check (Scenario.initialNode, Coordinated.start open) events

  it "does not reconstruct the local UTxO merely to stage a snapshot event" $ do
    let command = Coordinated.acceptSnapshot candidate (Seq.singleton $ error "staging reconstructs the local UTxO") Nothing
    case Interpreter.runProgramWith Interpreter.basicConfig (void $ Coordinated.stage (Coordinated.start open) command) of
      Continue events [] -> length events `shouldBe` 1
      _ -> fail "snapshot staging does not complete"

  it "carries a live continuation without forcing the validated UTxO projection" $ do
    request <- maybe (fail "no request") pure $ nextSnapshotRequest 0 Scenario.initialSnapshot mempty mempty Nothing NoDecommit Nothing
    let program :: Program.Program SimpleTx 'False 'False ()
        program = do
          next <- Coordinated.stage (Coordinated.start open) $ Coordinated.applyTransaction Scenario.transactionOne (error "before demanded") (error "after demanded")
          void $ Coordinated.stage next $ Coordinated.requestSnapshot request
    Interpreter.runProgramWith Interpreter.basicConfig program
      `shouldBe` Continue [TransactionAppliedToLocalUTxO testHeadId Scenario.transactionOne, SnapshotRequestDecided 1] []

  it "owns settlement, reobservation, rollback, adoption and clearing across recovery" $ do
    let depositId = 7
        deposit = Deposit testHeadId (utxoRef 8) initialChainTime (addUTCTime 100 initialChainTime) Active
        initial = Scenario.initialNode{deposits = trackedFromPending $ Map.singleton depositId deposit}
        adopted :: Snapshot SimpleTx
        adopted = candidate{utxoToCommit = Just (utxoRef 8), depositTxId = Just depositId, appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 2 <> utxoRef 8)}
        events =
          [ DepositActivated depositId initialChainTime deposit
          , CommitFinalized (SimpleChainState 3) testHeadId 1 depositId
          , SnapshotConfirmed testHeadId (Just adopted) mempty
          , CommitFinalized (SimpleChainState 4) testHeadId 1 depositId
          , ChainRolledBack (SimpleChainState 2)
          , DepositActivated depositId initialChainTime deposit
          , DecommitRecorded testHeadId (SimpleTx 3 (utxoRef 2) (utxoRef 4))
          , DecommitFinalized (SimpleChainState 5) testHeadId (utxoRef 4) 2
          , DecommitRecorded testHeadId (SimpleTx 4 (utxoRef 8) (utxoRef 9))
          , DecommitFinalized (SimpleChainState 6) testHeadId (utxoRef 4) 2
          , LocalStateCleared testHeadId 1
          , DepositRecovered (SimpleChainState 7) testHeadId depositId (utxoRef 8)
          ]
    void $ foldlM check (initial, Coordinated.start $ Scenario.openState initial) events

  it "matches the Agda settlement projection, including repeated observations" $
    forM_ [(old, new, pending) | old <- [0 .. 3], new <- [0 .. 3], pending <- [False, True]] $ \(old, new, pending) -> do
      let tx = SimpleTx 3 (utxoRef 1) (utxoRef 4)
          runtime = Coordinated.start open{coordinatedHeadState = open.coordinatedHeadState{version = fromInteger old, decommitTx = tx <$ guard pending}}
          (_, next) = Coordinated.step runtime $ Coordinated.settleDecommit (SimpleChainState 3) (fromInteger new) (utxoRef 4)
          actual = Coordinated.currentState next
      toInteger actual.version `shouldBe` Agda.settlementVersionRef old new
      isJust actual.decommitTx `shouldBe` Agda.settlementReservationRef pending old new

  it "does not demand signing bytes when replaying a retained confirmation" $ do
    let before = open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = SeenSnapshot candidate mempty (error "replay computes signing bytes")}}
        event = SnapshotConfirmed testHeadId Nothing signatures
    let next = Coordinated.replayEvent mempty event (Coordinated.start before)
    (Coordinated.currentState next).confirmedSnapshot `shouldBe` ConfirmedSnapshot candidate signatures

  it "retains a staged signature on wait and discards it on failed verification" $ do
    let runtime = Coordinated.start open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = seen}}
        signed = Coordinated.recordSignature 1 alice (sign aliceSk candidate)
        stage :: Program.Program SimpleTx 'False 'False (Coordinated.Runtime SimpleTx)
        stage = Coordinated.stage runtime signed
        event = PartySignedSnapshot 1 alice (sign aliceSk candidate)
    Interpreter.runProgramWith Interpreter.basicConfig (stage >> Program.suspend WaitOnSeenSnapshot)
      `shouldBe` Wait WaitOnSeenSnapshot [event]
    Interpreter.runProgramWith
      Interpreter.basicConfig
      ( do
          next <- stage
          result <- verifyCandidate keys (Coordinated.currentState next).seenSnapshot (aggregateInOrder (Map.singleton alice (sign aliceSk candidate)) Scenario.scenarioParties)
          either Program.reject (void . Coordinated.stage next . Coordinated.confirmSnapshot) result
      )
      `shouldSatisfy` \case Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False

  it "binds live confirmation evidence to the candidate's signed representation" $ do
    let runtime snapshot = Coordinated.start open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = mkSeenSnapshot snapshot mempty}}
        confirm :: Coordinated.Runtime SimpleTx -> Program.AcknowledgementProgram SimpleTx ()
        confirm target = do
          verified <- verifyCandidate keys seen signatures >>= either Program.reject pure
          void $ Coordinated.stage target $ Coordinated.confirmSnapshot verified
    Interpreter.runProgramWith Interpreter.basicConfig (confirm $ runtime candidate)
      `shouldBe` Continue [SnapshotConfirmed testHeadId Nothing signatures] []
    Interpreter.runProgramWith Interpreter.basicConfig (confirm $ runtime candidate{number = 2})
      `shouldSatisfy` \case Error AssertionFailed{} -> True; _ -> False
    Interpreter.runProgramWith Interpreter.basicConfig (confirm $ Coordinated.start open)
      `shouldSatisfy` \case Error AssertionFailed{} -> True; _ -> False
  it "binds confirmation and adoption to the Head and ordered committee" $
    forM_ [[], [alice], [bob, alice, carol]] $ \committee ->
      forM_ [Coordinated.confirmSnapshot, Coordinated.adoptSnapshot] $ \command -> do
        let signed = aggregateInOrder (Map.fromList [(p, sign sk candidate) | (p, sk) <- [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]]) committee
            program :: Program.AcknowledgementProgram SimpleTx ()
            program = do
              staged <- Coordinated.stage (Coordinated.start open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = seen}}) $ Coordinated.recordSignature 1 alice (sign aliceSk candidate)
              evidence <- verifyCandidate (vkey <$> committee) seen signed >>= either Program.reject pure
              void $ Coordinated.stage staged $ command evidence
        Interpreter.runProgramWith Interpreter.basicConfig program
          `shouldSatisfy` \case Error AssertionFailed{} -> True; _ -> False

  it "rejects correctly signed evidence from another Head, including adoption" $
    forM_ [Coordinated.confirmSnapshot, Coordinated.adoptSnapshot] $ \command -> do
      let foreignSnapshot :: Snapshot SimpleTx
          foreignSnapshot = candidate{headId = UnsafeHeadId "another head"}
          signed = aggregateInOrder (Map.fromList [(p, sign sk foreignSnapshot) | (p, sk) <- [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]]) Scenario.scenarioParties
          program :: Program.AcknowledgementProgram SimpleTx ()
          program = do
            evidence <- verifyCandidate keys (mkSeenSnapshot foreignSnapshot mempty) signed >>= either Program.reject pure
            void $ Coordinated.stage (Coordinated.start open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = seen}}) $ command evidence
      Interpreter.runProgramWith Interpreter.basicConfig program
        `shouldSatisfy` \case Error AssertionFailed{} -> True; _ -> False

  it "releases a conflicting reservation in all 24 delivery orders and admits the next decommit" $
    forM_ Conflict.schedules $ \schedule -> do
      (world, _) <- either (fail . toString) pure schedule
      world.pending `shouldBe` []
      world.retries `shouldBe` []
      forM_ (zip [0 ..] world.nodes) $ \(partyIndex, node) -> do
        let current = (Scenario.openState node).coordinatedHeadState
        current.decommitTx `shouldBe` Nothing
        (getSnapshot current.confirmedSnapshot).utxo `shouldBe` utxoRef 2
        let outcome = Head.update (Scenario.environment partyIndex) simpleLedger initialChainTime node $ Head.NetworkInput 0 $ ReceivedMessage alice $ ReqDec $ SimpleTx 11 (utxoRef 2) (utxoRef 11)
        outcome `shouldSatisfy` \case Continue (DecommitRecorded{} : _) _ -> True; _ -> False

  it "restores other reserved inputs on conflict, but keeps a retained dependency" $
    forM_ [False, True] $ \keepDependency -> do
      let tx = SimpleTx 10 (utxoRef 1 <> utxoRef 9) (utxoRef 10)
          base :: Snapshot SimpleTx
          base = candidate{utxo = utxoRef 2 <> utxoRef 9, accumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 2 <> utxoRef 9), appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 2 <> utxoRef 9)}
          pending = if keepDependency then Seq.singleton (SimpleTx 11 (utxoRef 2) (utxoRef 1)) else mempty
          before = open{coordinatedHeadState = open.coordinatedHeadState{decommitTx = Just tx, localTxs = pending, localUTxO = if keepDependency then mempty else utxoRef 2, seenSnapshot = mkSeenSnapshot base mempty}}
          event :: StateChanged SimpleTx
          event = SnapshotConfirmed testHeadId Nothing mempty
          runtime = Coordinated.replayEvent mempty event (Coordinated.start before)
          current = Coordinated.currentState runtime
      current.decommitTx `shouldBe` if keepDependency then Just tx else Nothing
      current.localUTxO `shouldBe` if keepDependency then mempty else utxoRef 2 <> utxoRef 9
      let node = Scenario.initialNode{headState = Head.Open before}
          expected = aggregateNodeState node event
      current `shouldBe` (Scenario.openState expected).coordinatedHeadState
      decodeFull' (serialize' expected) `shouldBe` Right expected

  it "protects an authorised decommit even when no ReqDec was received" $ do
    let authorised :: Snapshot SimpleTx
        authorised = candidate{utxoToDecommit = Just (utxoRef 10)}
        before = open{coordinatedHeadState = open.coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot authorised mempty, localUTxO = utxoRef 2}}
        runtime = Coordinated.start before
        command = Coordinated.reserveDecommit (SimpleTx 11 (utxoRef 2) (utxoRef 11))
        stage :: Coordinated.Runtime SimpleTx -> Outcome SimpleTx
        stage target = Interpreter.runProgramWith Interpreter.basicConfig (void $ Coordinated.stage target command)
    stage runtime `shouldSatisfy` \case Error AssertionFailed{} -> True; _ -> False
    let (_, settled) = Coordinated.step runtime $ Coordinated.settleDecommit (SimpleChainState 3) 1 (utxoRef 10)
    stage settled `shouldSatisfy` \case Continue [DecommitRecorded{}] [] -> True; _ -> False

  it "compares confirmation disposition with Agda over bounded input sets" $
    forM_ [(inputs, available, carried, other) | inputs <- subsequences [1, 2, 3], available <- subsequences [1, 2, 3], (carried, other) <- [(False, False), (True, False), (False, True)]] $ \(inputs, available, carried, other) -> do
      let tx = SimpleTx 10 (fromList $ fromInteger <$> inputs) (utxoRef 4)
          snapshot :: Snapshot SimpleTx
          snapshot = candidate{utxo = fromList $ fromInteger <$> available, utxoToDecommit = if carried then Just (utxoRef 4) else if other then Just (utxoRef 5) else Nothing}
          before = open{coordinatedHeadState = open.coordinatedHeadState{decommitTx = Just tx, seenSnapshot = mkSeenSnapshot snapshot mempty}}
          next = Coordinated.replayEvent mempty (SnapshotConfirmed testHeadId Nothing mempty) (Coordinated.start before)
      isJust (Coordinated.currentState next).decommitTx `shouldBe` Agda.retainReservationRef carried other inputs available

  it "compares client and network admission with Agda, including certificates without ReqDec" $
    forM_ [(pending, authorised) | pending <- [False, True], authorised <- [False, True]] $ \(pending, authorised) -> do
      let snapshot :: Snapshot SimpleTx
          snapshot = candidate{utxoToDecommit = utxoRef 10 <$ guard authorised}
          chs = open.coordinatedHeadState{confirmedSnapshot = ConfirmedSnapshot snapshot mempty, decommitTx = SimpleTx 10 (utxoRef 1) (utxoRef 10) <$ guard pending, localUTxO = utxoRef 2}
          node = Scenario.initialNode{headState = Head.Open open{coordinatedHeadState = chs}}
          tx = SimpleTx 11 (utxoRef 2) (utxoRef 11)
          run = Head.update (Scenario.environment 0) simpleLedger initialChainTime node
          accepts :: Outcome SimpleTx -> Bool
          accepts = \case Continue (DecommitInvalid{} : _) _ -> False; Continue{} -> True; _ -> False
      accepts (run $ Head.NetworkInput 0 $ ReceivedMessage alice $ ReqDec tx) `shouldBe` Agda.decommitAdmissionRef pending authorised
      accepts (run $ Head.ClientInput $ Client.Decommit tx) `shouldBe` Agda.decommitAdmissionRef pending authorised

  it "compares certificate context with Agda using real signatures" $
    forM_ [(sameHead, committee) | sameHead <- [False, True], committee <- [] : [[(1, alice)]] <> permutations [(1, alice), (2, bob), (3, carol)]] $ \(sameHead, committee) -> do
      let snapshot :: Snapshot SimpleTx
          snapshot = candidate{headId = if sameHead then testHeadId else UnsafeHeadId "another head"}
          signed = aggregateInOrder (Map.fromList [(p, sign sk snapshot) | (p, sk) <- [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]]) (snd <$> committee)
          program :: Program.AcknowledgementProgram SimpleTx ()
          program = do
            evidence <- verifyCandidate (vkey . snd <$> committee) (mkSeenSnapshot snapshot mempty) signed >>= either Program.reject pure
            void $ Coordinated.stage (Coordinated.start open{coordinatedHeadState = open.coordinatedHeadState{seenSnapshot = mkSeenSnapshot snapshot mempty}}) $ Coordinated.confirmSnapshot evidence
          accepted = case Interpreter.runProgramWith Interpreter.basicConfig program of Continue [SnapshotConfirmed{}] [] -> True; _ -> False
      accepted `shouldBe` Agda.verificationContextRef 0 (if sameHead then 0 else 1) [1, 2, 3] (fst <$> committee)
 where
  open = Scenario.openState Scenario.initialNode
  candidate :: Snapshot SimpleTx
  candidate = Scenario.initialSnapshot{number = 1, confirmed = [Scenario.transactionOne], utxo = utxoRef 2, accumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 2), appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (utxoRef 2)}
  seen = mkSeenSnapshot candidate mempty
  keys = vkey <$> Scenario.scenarioParties
  signatures = aggregateInOrder (Map.fromList [(p, sign sk candidate) | (p, sk) <- [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]]) Scenario.scenarioParties

  check :: (NodeState SimpleTx, Coordinated.Runtime SimpleTx) -> StateChanged SimpleTx -> IO (NodeState SimpleTx, Coordinated.Runtime SimpleTx)
  check (node, runtime) event = do
    let before = Scenario.openState node
        deposited = case event of
          CommitFinalized{depositTxId} -> maybe mempty (.deposited) $ Map.lookup depositTxId node.pendingDeposits
          _ -> mempty
    let next = Coordinated.replayEvent deposited event runtime
        restoredNext = Coordinated.replayEvent deposited event (Coordinated.start before)
        expected = replayWithReservationFix node event
        after = aggregateNodeState node event
    after `shouldBe` expected
    Coordinated.currentState next `shouldBe` (Scenario.openState after).coordinatedHeadState
    Coordinated.currentState restoredNext `shouldBe` Coordinated.currentState next
    decodeFull' (serialize' after) `shouldBe` Right expected
    pure (after, next)
