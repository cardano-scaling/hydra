{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.InterpreterSpec (spec) where

import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node.HeadLogic (Effect (NetworkEffect), LogicError (AssertionFailed), Outcome (..), StateChanged (TransactionAppliedToLocalUTxO, TransactionReceived), WaitReason (WaitOnSnapshotNumber))
import Hydra.Prelude hiding (trace)
import Hydra.Prelude qualified as Prelude
import Hydra.Tx.Crypto (Verified (..), aggregate, getSignableRepresentation, getVerificationKey, sign)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (InitialSnapshot), Snapshot, getSnapshot)
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.Ledger.Simple (aValidTx, utxoRef)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe)
import Test.Hydra.Tx.Fixture (aliceSk, testHeadId)

spec :: Spec
spec = do
  describe "program termination" $ do
    it "preserves change and effect order on completion" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            Program.change received
            Program.emit firstEffect
            Program.change applied
            Program.emit secondEffect
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program
        `shouldBe` Continue [received, applied] [firstEffect, secondEffect]

    it "commits the change prefix and discards effects when suspended" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            Program.change received
            Program.emit firstEffect
            Program.change applied
            _ <- Program.suspend waitReason
            Prelude.error "suspension evaluates its continuation"
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program
        `shouldBe` Wait waitReason [received, applied]

    it "discards all changes and effects when rejected" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            Program.change received
            Program.emit firstEffect
            Program.change applied
            _ <- Program.reject rejection
            Prelude.error "rejection evaluates its continuation"
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program `shouldBe` Error rejection

    it "does not evaluate a rejection after suspension" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            Program.change received
            _ <- Program.suspend waitReason
            Program.reject rejection
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program `shouldBe` Wait waitReason [received]

    it "uses the result of a named decision to select its continuation" $ do
      forM_ [False, True] $ \predicate -> do
        let program :: Program.Program SimpleTx ()
            program = do
              accepted <- Program.decide "example guard" predicate
              Program.change $ if accepted then applied else received
        Interpreter.runProgram (testInterpreterConfig simpleLedger) program
          `shouldBe` Continue [if predicate then applied else received] []

    it "passes the requested slot, UTxO, and transaction to the ledger" $ do
      let oldUTxO = utxoRef 10
          returnedUTxO = utxoRef 20
          ledger = Ledger $ \slot utxo txs ->
            if slot == 7 && utxo == oldUTxO && txs == [sampleTx]
              then Right returnedUTxO
              else Prelude.error "ledger query arguments differ from the instruction"
          program :: Program.Program SimpleTx ()
          program = do
            result <- Program.applyTransaction 7 oldUTxO sampleTx
            if result == Right returnedUTxO
              then Program.change applied
              else Program.reject rejection
      Interpreter.runProgram (testInterpreterConfig ledger) program `shouldBe` Continue [applied] []

    it "returns ledger failures to the rule without rejecting the attempt" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            result <- Program.applyTransaction 0 mempty sampleTx
            if result == Left ledgerFailure
              then Program.change received
              else Program.reject rejection
      Interpreter.runProgram (testInterpreterConfig rejectingLedger) program `shouldBe` Continue [received] []

    it "traces tentative work and the disposition that discards it" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            Program.change received
            Program.emit firstEffect
            Program.reject rejection
          (outcome, trace) = Interpreter.traceProgram (testInterpreterConfig simpleLedger) program
      outcome `shouldBe` Error rejection
      trace `shouldBe` [Trace.Changed "transaction received", Trace.Emitted "broadcast ReqTx", Trace.Rejected]

    it "uses the configured signer and verifier" $ do
      let snapshot = getSnapshot (InitialSnapshot testHeadId) :: Snapshot SimpleTx
          key = getVerificationKey aliceSk
          signature = sign aliceSk snapshot
          bytes = getSignableRepresentation snapshot
          config =
            Interpreter.InterpreterConfig
              { Interpreter.transactionApplication = \_ _ _ -> Prelude.error "unexpected ledger query"
              , Interpreter.snapshotSigner = Interpreter.SnapshotSigner (sign aliceSk) key
              , Interpreter.snapshotVerifier = \vkeys signatures contents ->
                  if vkeys == [key] && signatures == aggregate [signature] && contents == bytes
                    then Verified
                    else FailedKeys [key]
              }
          program :: Program.Program SimpleTx ()
          program = do
            signed <- Program.signSnapshot snapshot
            result <- Program.verifySnapshot [key] (aggregate [signed]) bytes
            case result of
              Verified -> Program.change received
              _ -> Program.reject rejection
      Interpreter.runExecution (Interpreter.Execution config program) `shouldBe` Continue [received] []
      Interpreter.signingIdentity config.snapshotSigner `shouldBe` key

    it "uses the configured verifier" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            result <- Program.verifySnapshot [] (aggregate []) "contents"
            case result of
              FailedKeys [] -> Program.change received
              _ -> Program.reject rejection
          config :: Interpreter.InterpreterConfig SimpleTx
          config = (testInterpreterConfig simpleLedger){Interpreter.snapshotVerifier = \_ _ _ -> FailedKeys []}
      Interpreter.runProgram config program `shouldBe` Continue [received] []

    it "can replace a ledger reply while preserving the program continuation" $ do
      let program :: Program.Program SimpleTx ()
          program = do
            result <- Program.applyTransaction 0 mempty sampleTx
            either (const $ Program.change received) (const $ Program.reject rejection) result
          replace :: Program.Instruction SimpleTx a -> Program.Program SimpleTx a
          replace = \case
            Program.ApplyTransaction{} -> pure $ Left ledgerFailure
            op -> Program.instruction op
      Interpreter.runProgram (testInterpreterConfig simpleLedger) (Program.foldProgram replace program) `shouldBe` Continue [received] []

sampleTx :: SimpleTx
sampleTx = aValidTx 42

received, applied :: StateChanged SimpleTx
received = TransactionReceived sampleTx
applied = TransactionAppliedToLocalUTxO testHeadId sampleTx

firstEffect, secondEffect :: Effect SimpleTx
firstEffect = NetworkEffect $ ReqTx sampleTx
secondEffect = NetworkEffect $ ReqTx (aValidTx 43)

waitReason :: WaitReason SimpleTx
waitReason = WaitOnSnapshotNumber 1

rejection :: LogicError SimpleTx
rejection = AssertionFailed "test rejection"

ledgerFailure :: ValidationError
ledgerFailure = ValidationError "controlled ledger rejection"

rejectingLedger :: Ledger SimpleTx
rejectingLedger = Ledger $ \_ _ txs -> case txs of
  [tx] -> Left (tx, ledgerFailure)
  _ -> Prelude.error "expected one ledger transaction"
