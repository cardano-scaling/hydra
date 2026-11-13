-- | The evaluator of `Program`s.
module Hydra.HeadLogic.Interpreter where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Outcome (Effect, Outcome (..), StateChanged)
import Hydra.HeadLogic.Program (
  Instruction (..),
  Program (Program),
  TransactionProgram,
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

-- | The two indices say which primitive queries a program may issue. Disabled
-- capabilities have no function to call; incompatible runners fail to typecheck.
data Capability (enabled :: Bool) a where
  Available :: a -> Capability 'True a
  Unavailable :: Capability 'False a

available :: Capability 'True a -> a
available (Available value) = value

data SnapshotSigner tx = SnapshotSigner
  { signOne :: Snapshot tx -> Signature (Snapshot tx)
  , signingIdentity :: VerificationKey HydraKey
  }

-- | An interpreter config controls how transactions are applied, how
-- snapshots are signed, and a process for verifying snapshots.
--
-- The bools let us control at which point certain capabilities; for example, creating
-- a snapshot requires the ledger and signing; but acknowledging a snapshot doesn't require
data InterpreterConfig tx (ledger :: Bool) (signing :: Bool) = InterpreterConfig
  { transactionApplication ::
      Capability ledger (ChainSlot -> UTxOType tx -> tx -> Either ValidationError (UTxOType tx))
  , snapshotSigner ::
      Capability signing (SnapshotSigner tx)
  , snapshotVerifier ::
      [VerificationKey HydraKey] ->
      MultiSignature (Snapshot tx) ->
      ByteString ->
      Verified
  }

ledgerConfig :: Ledger tx -> InterpreterConfig tx 'True 'False
ledgerConfig Ledger{applyTransactions} =
  InterpreterConfig
    { transactionApplication = Available $ \slot utxo tx -> first snd $ applyTransactions slot utxo [tx]
    , snapshotSigner = Unavailable
    , snapshotVerifier = verifyMultiSignatureBytes
    }

basicConfig :: InterpreterConfig tx 'False 'False
basicConfig = InterpreterConfig Unavailable Unavailable verifyMultiSignatureBytes

-- | Pair existential capabilities with their matching interpreter once, at a
-- development dispatch boundary. The production wrappers use concrete types.
data Execution tx where
  Execution :: InterpreterConfig tx ledger signing -> Program tx ledger signing () -> Execution tx

runExecution :: Execution tx -> Outcome tx
runExecution (Execution config program) = runProgramWith config program

-- | Production interpretation. The ignored observer never forces trace values.
runProgram :: Ledger tx -> TransactionProgram tx () -> Outcome tx
runProgram ledger = runProgramWith (ledgerConfig ledger)
{-# INLINE runProgram #-}

runProgramWith :: InterpreterConfig tx ledger signing -> Program tx ledger signing () -> Outcome tx
runProgramWith config = fst . evaluateProgram (\() _ -> ()) () config
{-# INLINE runProgramWith #-}

traceProgram :: Ledger tx -> TransactionProgram tx () -> (Outcome tx, [TraceStep])
traceProgram ledger = traceProgramWith (ledgerConfig ledger)

traceProgramWith :: InterpreterConfig tx ledger signing -> Program tx ledger signing () -> (Outcome tx, [TraceStep])
traceProgramWith config program =
  let (outcome, reversedTrace) = evaluateProgram (flip (:)) [] config program
   in (outcome, reverse reversedTrace)

-- | Completion commits changes and effects; waiting commits only the prefix;
-- rejection discards both. All interpreters use this definition.
evaluateProgram ::
  forall trace tx ledger signing.
  (trace -> TraceStep -> trace) ->
  trace ->
  InterpreterConfig tx ledger signing ->
  Program tx ledger signing () ->
  (Outcome tx, trace)
evaluateProgram observe initialTrace InterpreterConfig{transactionApplication, snapshotSigner, snapshotVerifier} (Program program) =
  program done step [] [] initialTrace
 where
  done :: () -> [StateChanged tx] -> [Effect tx] -> trace -> (Outcome tx, trace)
  done () changes effects trace = (Continue (reverse changes) (reverse effects), observe trace Finished)
  step :: Instruction tx ledger signing x -> (x -> [StateChanged tx] -> [Effect tx] -> trace -> (Outcome tx, trace)) -> [StateChanged tx] -> [Effect tx] -> trace -> (Outcome tx, trace)
  step op next changes effects trace = case op of
    ApplyTransaction slot utxo tx ->
      let result = available transactionApplication slot utxo tx
       in next result changes effects (observe trace $ LedgerApplied $ isRight result)
    SignSnapshot snapshot -> next (signOne (available snapshotSigner) snapshot) changes effects (observe trace $ PrimitiveCalled "sign snapshot")
    VerifySnapshot keys signatures bytes -> next (snapshotVerifier keys signatures bytes) changes effects (observe trace $ PrimitiveCalled "verify multisignature")
    Change event -> next () (event : changes) effects (observe trace $ Changed $ changeLabel event)
    Emit effect -> next () changes (effect : effects) (observe trace $ Emitted $ effectLabel effect)
    Decide label answer -> next answer changes effects (observe trace $ Decided label answer)
    Suspend reason -> (Wait reason (reverse changes), observe trace Suspended)
    Reject err -> (Error err, observe trace Rejected)
{-# INLINE evaluateProgram #-}
