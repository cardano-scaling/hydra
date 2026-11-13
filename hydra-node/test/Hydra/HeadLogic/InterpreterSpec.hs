{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.InterpreterSpec (spec) where

import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program (
  Instruction (ApplyTransaction),
  Program,
  applyTransaction,
  change,
  decide,
  emit,
  foldProgram,
  instruction,
  reject,
  signSnapshot,
  suspend,
  verifySnapshot,
 )
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network.Message (Message (ReqTx))
import Hydra.Node.HeadLogic (
  Effect (NetworkEffect),
  LogicError (AssertionFailed),
  Outcome (..),
  StateChanged (TransactionAppliedToLocalUTxO, TransactionReceived),
  WaitReason (WaitOnSnapshotNumber),
 )
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
      let program :: Program SimpleTx ()
          program = do
            change received
            emit firstEffect
            change applied
            emit secondEffect
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program
        `shouldBe` Continue [received, applied] [firstEffect, secondEffect]

    it "commits the change prefix and discards effects when suspended" $ do
      let program :: Program SimpleTx ()
          program = do
            change received
            emit firstEffect
            change applied
            _ <- suspend waitReason
            Prelude.error "suspension evaluates its continuation"
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program
        `shouldBe` Wait waitReason [received, applied]

    it "discards all changes and effects when rejected" $ do
      let program :: Program SimpleTx ()
          program = do
            change received
            emit firstEffect
            change applied
            _ <- reject rejection
            Prelude.error "rejection evaluates its continuation"
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program `shouldBe` Error rejection

    it "does not evaluate a rejection after suspension" $ do
      let program :: Program SimpleTx ()
          program = do
            change received
            _ <- suspend waitReason
            reject rejection
      Interpreter.runProgram (testInterpreterConfig simpleLedger) program `shouldBe` Wait waitReason [received]

    it "uses the result of a named decision to select its continuation" $ do
      forM_ [False, True] $ \predicate -> do
        let program :: Program SimpleTx ()
            program = do
              accepted <- decide "example guard" predicate
              change $ if accepted then applied else received
        Interpreter.runProgram (testInterpreterConfig simpleLedger) program
          `shouldBe` Continue [if predicate then applied else received] []

    it "passes the requested slot, UTxO, and transaction to the ledger" $ do
      let oldUTxO = utxoRef 10
          returnedUTxO = utxoRef 20
          ledger = Ledger $ \slot utxo txs ->
            if slot == 7 && utxo == oldUTxO && txs == [sampleTx]
              then Right returnedUTxO
              else Prelude.error "ledger query arguments differ from the instruction"
          program :: Program SimpleTx ()
          program = do
            result <- applyTransaction 7 oldUTxO sampleTx
            if result == Right returnedUTxO
              then change applied
              else reject rejection
      Interpreter.runProgram (testInterpreterConfig ledger) program `shouldBe` Continue [applied] []

    it "returns ledger failures to the rule without rejecting the attempt" $ do
      let program :: Program SimpleTx ()
          program = do
            result <- applyTransaction 0 mempty sampleTx
            if result == Left ledgerFailure
              then change received
              else reject rejection
      Interpreter.runProgram (testInterpreterConfig rejectingLedger) program `shouldBe` Continue [received] []

    it "traces tentative work and the disposition that discards it" $ do
      let program :: Program SimpleTx ()
          program = do
            change received
            emit firstEffect
            reject rejection
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
          program :: Program SimpleTx ()
          program = do
            signed <- signSnapshot snapshot
            result <- verifySnapshot [key] (aggregate [signed]) bytes
            case result of
              Verified -> change received
              _ -> reject rejection
      Interpreter.runExecution (Interpreter.Execution config program) `shouldBe` Continue [received] []
      Interpreter.signingIdentity config.snapshotSigner `shouldBe` key

    it "uses the configured verifier" $ do
      let program :: Program SimpleTx ()
          program = do
            result <- verifySnapshot [] (aggregate []) "contents"
            case result of
              FailedKeys [] -> change received
              _ -> reject rejection
          config :: Interpreter.InterpreterConfig SimpleTx
          config = (testInterpreterConfig simpleLedger){Interpreter.snapshotVerifier = \_ _ _ -> FailedKeys []}
      Interpreter.runProgram config program `shouldBe` Continue [received] []

    it "can replace a ledger reply while preserving the program continuation" $ do
      let program :: Program SimpleTx ()
          program = do
            result <- applyTransaction 0 mempty sampleTx
            either (const $ change received) (const $ reject rejection) result
          replace :: Instruction SimpleTx a -> Program SimpleTx a
          replace = \case
            ApplyTransaction{} -> pure $ Left ledgerFailure
            op -> instruction op

      Interpreter.runProgram
        (testInterpreterConfig simpleLedger)
        (foldProgram replace program)
        `shouldBe` Continue [received] []

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
