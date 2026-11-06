{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.CoordinatedSpec (spec) where

import Hydra.Prelude hiding (keys)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Binary (decodeFull', serialize')
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic (aggregateNodeState)
import Hydra.HeadLogic.Error (LogicError (RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Outcome (Continue, Error, Wait), StateChanged (..), WaitReason (WaitOnSeenSnapshot))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotRequest (DecommitStatus (..), chainedSnapshotRequest, decommitStatus)
import Hydra.HeadLogic.Rules.SnapshotVerification (verifyCandidate)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), OpenState (..), SeenSnapshot (SeenSnapshot), mkSeenSnapshot)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..))
import Hydra.Node.State (Deposit (..), DepositStatus (Active), NodeState (..), initialChainTime, trackedFromPending)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..))
import Test.Hydra.HeadLogic.CoordinatedAudit qualified as Audit
import Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, testHeadId)

spec :: Spec
spec = describe "coordinated state owners" $ do
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
      isJust (chainedSnapshotRequest base.utxo mempty mempty Nothing (decommitStatus 0 base $ Just tx) Nothing)
        `shouldBe` Agda.newDecommitReadyRef carried reserved confirmed
      let rebased :: Snapshot SimpleTx
          rebased = base{utxo = fromList $ fromInteger <$> available}
          (_, next) = Coordinated.step (Coordinated.start open{coordinatedHeadState = chs}) $ Coordinated.acceptSnapshot rebased mempty Nothing
      (Coordinated.currentState next).localUTxO `shouldBe` fromList (fromInteger <$> Agda.reserveRebaseRef reserved available)

  it "uses one transition definition for staging, continuous replay and checkpoint recovery" $ do
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
    let program :: Program.Program SimpleTx 'False 'False ()
        program = do
          next <- Coordinated.stage (Coordinated.start open) $ Coordinated.applyTransaction Scenario.transactionOne (error "before demanded") (error "after demanded")
          void $ Coordinated.stage next $ Coordinated.requestSnapshot 1
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
    command <- maybe (fail "confirmation has no replay command") pure $ Coordinated.fromEvent mempty event
    let (_, next) = Coordinated.step (Coordinated.start before) command
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
      `shouldBe` Continue [] []
    Interpreter.runProgramWith Interpreter.basicConfig (confirm $ Coordinated.start open)
      `shouldBe` Continue [] []
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
        emitted = case event of ChainRolledBack{} -> []; _ -> [event]
    command <- maybe (fail "event has no coordinated command") pure $ Coordinated.fromEvent deposited event
    let (produced, next) = Coordinated.step runtime command
        (_, restoredNext) = Coordinated.step (Coordinated.start before) command
        expected = replayWithReservationFix node event
        after = aggregateNodeState node event
    produced `shouldBe` emitted
    Interpreter.runProgramWith Interpreter.basicConfig (void $ Coordinated.stage runtime command) `shouldBe` Continue emitted []
    after `shouldBe` expected
    Coordinated.currentState next `shouldBe` (Scenario.openState after).coordinatedHeadState
    Coordinated.currentState restoredNext `shouldBe` Coordinated.currentState next
    decodeFull' (serialize' after) `shouldBe` Right expected
    pure (after, next)
