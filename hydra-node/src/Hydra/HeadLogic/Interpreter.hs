-- | Primitive implementations and the single evaluator for completion, waiting
-- and rejection. Presentation belongs to development tools.
module Hydra.HeadLogic.Interpreter where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.Outcome (Effect, Outcome (..), StateChanged (..), WaitReason)
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
import Hydra.Node.Environment (Environment (..))
import Hydra.Prelude hiding (keys, label, trace)
import Hydra.Tx (HeadId, IsTx, UTxOType)
import Hydra.Tx.Crypto (HydraKey, Signature, VerificationKey, getVerificationKey, sign, verifyMultiSignatureBytes)
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

data InterpreterConfig tx (ledger :: Bool) (signing :: Bool) = InterpreterConfig
  { transactionApplication :: Capability ledger (ChainSlot -> UTxOType tx -> tx -> Either ValidationError (UTxOType tx))
  , snapshotSigner :: Capability signing (SnapshotSigner tx)
  }

ledgerConfig :: Ledger tx -> InterpreterConfig tx 'True 'False
ledgerConfig Ledger{applyTransactions} =
  InterpreterConfig
    { transactionApplication = Available $ \slot utxo tx -> first snd $ applyTransactions slot utxo [tx]
    , snapshotSigner = Unavailable
    }

snapshotConfig :: IsTx tx => Environment -> Ledger tx -> InterpreterConfig tx 'True 'True
snapshotConfig Environment{signingKey} ledger =
  InterpreterConfig
    { transactionApplication = transactionApplication $ ledgerConfig ledger
    , snapshotSigner = Available $ SnapshotSigner (sign signingKey) (getVerificationKey signingKey)
    }

basicConfig :: InterpreterConfig tx 'False 'False
basicConfig = InterpreterConfig Unavailable Unavailable

-- | Pair existential capabilities with their matching interpreter once, at a
-- development dispatch boundary. The production wrappers use concrete types.
data Execution tx where
  Execution :: InterpreterConfig tx ledger signing -> Program tx ledger signing () -> Execution tx

runExecution :: Execution tx -> Outcome tx
runExecution (Execution config program) = runProgramWith config program

-- | A rule supplies the validated endpoints; persistence interpreters choose the
-- representation. This type is not part of the production event encoding.
data StagedChange tx
  = ExistingChange (StateChanged tx)
  | AppliedTransaction HeadId tx ~(UTxOType tx) ~(UTxOType tx)

compatibleChange :: StagedChange tx -> StateChanged tx
compatibleChange = \case
  ExistingChange event -> event
  AppliedTransaction headId tx _ _ -> TransactionAppliedToLocalUTxO{headId, tx}

-- | Shared terminal semantics for compatibility and experimental interpreters.
data Result tx event
  = Completed [event] [Effect tx]
  | Waiting (WaitReason tx) [event]
  | RejectedWith (LogicError tx)

compatibleOutcome :: Result tx (StateChanged tx) -> Outcome tx
compatibleOutcome = \case
  Completed events effects -> Continue events effects
  Waiting reason events -> Wait reason events
  RejectedWith err -> Error err

-- | Production interpretation. The ignored observer never forces trace values.
runProgram :: Ledger tx -> TransactionProgram tx () -> Outcome tx
runProgram ledger = runProgramWith (ledgerConfig ledger)
{-# INLINE runProgram #-}

runProgramWith :: InterpreterConfig tx ledger signing -> Program tx ledger signing () -> Outcome tx
runProgramWith config = compatibleOutcome . fst . evaluateChanges compatibleChange (\() _ -> ()) () config
{-# INLINE runProgramWith #-}

traceProgram :: Ledger tx -> TransactionProgram tx () -> (Outcome tx, [TraceStep])
traceProgram ledger = traceProgramWith (ledgerConfig ledger)

traceProgramWith :: InterpreterConfig tx ledger signing -> Program tx ledger signing () -> (Outcome tx, [TraceStep])
traceProgramWith config program =
  let (outcome, reversedTrace) = evaluateChanges compatibleChange (flip (:)) [] config program
   in (compatibleOutcome outcome, reverse reversedTrace)

-- | Completion commits changes and effects; waiting commits only the prefix;
-- rejection discards both. All interpreters use this definition.
evaluateChanges :: forall event trace tx ledger signing. (StagedChange tx -> event) -> (trace -> TraceStep -> trace) -> trace -> InterpreterConfig tx ledger signing -> Program tx ledger signing () -> (Result tx event, trace)
evaluateChanges resolve observe initialTrace InterpreterConfig{transactionApplication, snapshotSigner} (Program program) =
  program done step [] [] initialTrace
 where
  done :: () -> [event] -> [Effect tx] -> trace -> (Result tx event, trace)
  done () changes effects trace = (Completed (reverse changes) (reverse effects), observe trace Finished)
  step :: Instruction tx ledger signing x -> (x -> [event] -> [Effect tx] -> trace -> (Result tx event, trace)) -> [event] -> [Effect tx] -> trace -> (Result tx event, trace)
  step op next changes effects trace = case op of
    ApplyTransaction slot utxo tx ->
      let result = available transactionApplication slot utxo tx
       in next result changes effects (observe trace $ LedgerApplied $ isRight result)
    SignSnapshot snapshot -> next (signOne (available snapshotSigner) snapshot) changes effects (observe trace $ PrimitiveCalled "sign snapshot")
    VerifySnapshot keys signatures bytes -> next (verifyMultiSignatureBytes keys signatures bytes) changes effects (observe trace $ PrimitiveCalled "verify multisignature")
    RecordAppliedTransaction hid tx before after ->
      next () (resolve (AppliedTransaction hid tx before after) : changes) effects (observe trace $ Changed "apply transaction to local state")
    Change event -> next () (resolve (ExistingChange event) : changes) effects (observe trace $ Changed $ changeLabel event)
    Emit effect -> next () changes (effect : effects) (observe trace $ Emitted $ effectLabel effect)
    Decide label answer -> next answer changes effects (observe trace $ Decided label answer)
    Suspend reason -> (Waiting reason (reverse changes), observe trace Suspended)
    Reject err -> (RejectedWith err, observe trace Rejected)
{-# INLINE evaluateChanges #-}
