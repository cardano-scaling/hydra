-- | The evaluator of `Program`s.
module Hydra.HeadLogic.Interpreter where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Outcome (Effect, Outcome (..), StateChanged)
import Hydra.HeadLogic.Program (
  Instruction (..),
  Program (Program),
 )
import Hydra.HeadLogic.Trace (
  TraceStep (..),
  changeLabel,
  effectLabel,
 )
import Hydra.Ledger (Ledger (..), ValidationError)
import Hydra.Prelude hiding (keys, label, trace)
import Hydra.Tx (UTxOType)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, Signature, VerificationKey, Verified, verifyMultiSignatureBytes)
import Hydra.Tx.Snapshot (Snapshot)

data SnapshotSigner tx = SnapshotSigner
  { signOne :: Snapshot tx -> Signature (Snapshot tx)
  , signingIdentity :: VerificationKey HydraKey
  }

-- | An interpreter config controls how transactions are applied, how
-- snapshots are signed, and a process for verifying snapshots.
data InterpreterConfig tx = InterpreterConfig
  { transactionApplication ::
      ChainSlot ->
      UTxOType tx ->
      tx ->
      Either ValidationError (UTxOType tx)
  , snapshotSigner ::
      SnapshotSigner tx
  , snapshotVerifier ::
      [VerificationKey HydraKey] ->
      MultiSignature (Snapshot tx) ->
      ByteString ->
      Verified
  }

mkInterpreterConfig :: Ledger tx -> SnapshotSigner tx -> InterpreterConfig tx
mkInterpreterConfig Ledger{applyTransactions} signer =
  InterpreterConfig
    { transactionApplication = \slot utxo tx -> first snd $ applyTransactions slot utxo [tx]
    , snapshotSigner = signer
    , snapshotVerifier = verifyMultiSignatureBytes
    }

-- | A program paired with its primitive implementations for development tools.
data Execution tx = Execution (InterpreterConfig tx) (Program tx ())

runExecution :: Execution tx -> Outcome tx
runExecution (Execution config program) = runProgram config program

-- | Run the program, ignore the traces (don't even force them.)
runProgram :: InterpreterConfig tx -> Program tx () -> Outcome tx
runProgram config = fst . evaluateProgram (\() _ -> ()) () config
{-# INLINE runProgram #-}

traceProgram :: InterpreterConfig tx -> Program tx () -> (Outcome tx, [TraceStep])
traceProgram config program =
  let (outcome, reversedTrace) = evaluateProgram (flip (:)) [] config program
   in (outcome, reverse reversedTrace)

evaluateProgram ::
  forall trace tx.
  (trace -> TraceStep -> trace) ->
  trace ->
  InterpreterConfig tx ->
  Program tx () ->
  (Outcome tx, trace)
evaluateProgram observe initialTrace InterpreterConfig{transactionApplication, snapshotSigner, snapshotVerifier} (Program program) =
  program (const done) step [] [] initialTrace
 where
  done ::
    [StateChanged tx] ->
    [Effect tx] ->
    trace ->
    (Outcome tx, trace)
  done changes effects trace = (Continue (reverse changes) (reverse effects), observe trace Finished)

  step ::
    Instruction tx x ->
    (x -> [StateChanged tx] -> [Effect tx] -> trace -> (Outcome tx, trace)) ->
    [StateChanged tx] ->
    [Effect tx] ->
    trace ->
    (Outcome tx, trace)
  step op next changes effects trace = case op of
    ApplyTransaction slot utxo tx ->
      let result = transactionApplication slot utxo tx
       in next result changes effects (observe trace $ LedgerApplied $ isRight result)
    SignSnapshot snapshot ->
      next (signOne snapshotSigner snapshot) changes effects (observe trace $ PrimitiveCalled "sign snapshot")
    VerifySnapshot keys signatures bytes ->
      next
        (snapshotVerifier keys signatures bytes)
        changes
        effects
        (observe trace $ PrimitiveCalled "verify multisignature")
    Change event ->
      next () (event : changes) effects (observe trace $ Changed $ changeLabel event)
    Emit effect ->
      next () changes (effect : effects) (observe trace $ Emitted $ effectLabel effect)
    Decide label answer ->
      next answer changes effects (observe trace $ Decided label answer)
    Suspend reason ->
      (Wait reason (reverse changes), observe trace Suspended)
    Reject err ->
      (Error err, observe trace Rejected)
{-# INLINE evaluateProgram #-}
