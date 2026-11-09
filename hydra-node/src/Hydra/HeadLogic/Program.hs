{-# LANGUAGE RankNTypes #-}

-- | The typed instruction language and composition of HeadLogic rules.
-- It contains no evaluator, key configuration, tracing policy or rendering.
module Hydra.HeadLogic.Program (
  Program (..),
  Instruction (..),
  TransactionProgram,
  SnapshotProgram,
  AcknowledgementProgram,
  HeadProgram,
  promoteProgram,
  handler,
  instruction,
  change,
  emit,
  decide,
  require,
  applyTransaction,
  signSnapshot,
  verifySnapshot,
  recordAppliedTransaction,
  finish,
  suspend,
  reject,
  foldProgram,
) where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.Outcome (Effect, StateChanged, WaitReason)
import Hydra.Ledger (ValidationError)
import Hydra.Prelude hiding (keys, label)
import Hydra.Tx (HeadId, UTxOType)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, Signature, VerificationKey, Verified)
import Hydra.Tx.Snapshot (Snapshot)

-- | Church encoding of the operational free monad. Composition does not allocate
-- an intermediate instruction tree; an interpreter can still reify one using
-- 'foldProgram'. The rule has the same meaning under either representation.
newtype Program tx (ledger :: Bool) (signing :: Bool) a = Program
  { interpretProgram :: forall r. (a -> r) -> (forall x. Instruction tx ledger signing x -> (x -> r) -> r) -> r
  }

data Instruction tx (ledger :: Bool) (signing :: Bool) a where
  ApplyTransaction :: ChainSlot -> UTxOType tx -> tx -> Instruction tx 'True signing (Either ValidationError (UTxOType tx))
  SignSnapshot :: Snapshot tx -> Instruction tx ledger 'True (Signature (Snapshot tx))
  VerifySnapshot :: [VerificationKey HydraKey] -> MultiSignature (Snapshot tx) -> ByteString -> Instruction tx ledger signing Verified
  -- Compatibility interpretation discards these UTxOs. Keep their conversion
  -- lazy; resolved-change interpreters demand them when constructing a delta.
  RecordAppliedTransaction :: HeadId -> tx -> ~(UTxOType tx) -> ~(UTxOType tx) -> Instruction tx ledger signing ()
  Change :: StateChanged tx -> Instruction tx ledger signing ()
  Emit :: Effect tx -> Instruction tx ledger signing ()
  Decide :: Text -> Bool -> Instruction tx ledger signing Bool
  Finish :: Instruction tx ledger signing Void
  Suspend :: WaitReason tx -> Instruction tx ledger signing Void
  Reject :: LogicError tx -> Instruction tx ledger signing Void

instance Functor (Program tx ledger signing) where
  fmap f (Program program) = Program $ \done step -> program (done . f) step
  {-# INLINE fmap #-}

instance Applicative (Program tx ledger signing) where
  pure a = Program $ \done _ -> done a
  {-# INLINE pure #-}
  Program function <*> Program argument = Program $ \done step ->
    function (\f -> argument (done . f) step) step
  {-# INLINE (<*>) #-}

instance Monad (Program tx ledger signing) where
  Program program >>= next = Program $ \done step ->
    program (\a -> interpretProgram (next a) done step) step
  {-# INLINE (>>=) #-}

instruction :: Instruction tx ledger signing a -> Program tx ledger signing a
instruction op = Program $ \done step -> step op done
{-# INLINE instruction #-}

change :: StateChanged tx -> Program tx ledger signing ()
change = instruction . Change
{-# INLINE change #-}

emit :: Effect tx -> Program tx ledger signing ()
emit = instruction . Emit
{-# INLINE emit #-}

decide :: Text -> Bool -> Program tx ledger signing Bool
decide label = instruction . Decide label
{-# INLINE decide #-}

-- | Trace a requirement and reject the program when it does not hold.
require :: Text -> Bool -> LogicError tx -> Program tx ledger signing ()
require label condition err = do
  satisfied <- decide label condition
  unless satisfied $ reject err
{-# INLINE require #-}

applyTransaction :: ChainSlot -> UTxOType tx -> tx -> Program tx 'True signing (Either ValidationError (UTxOType tx))
applyTransaction slot utxo tx = instruction $ ApplyTransaction slot utxo tx
{-# INLINE applyTransaction #-}

-- | Signing material belongs to interpreter configuration, never the program.
signSnapshot :: Snapshot tx -> Program tx ledger 'True (Signature (Snapshot tx))
signSnapshot = instruction . SignSnapshot

verifySnapshot :: [VerificationKey HydraKey] -> MultiSignature (Snapshot tx) -> ByteString -> Program tx ledger signing Verified
verifySnapshot keys signatures bytes = instruction $ VerifySnapshot keys signatures bytes

recordAppliedTransaction :: HeadId -> tx -> UTxOType tx -> UTxOType tx -> Program tx ledger signing ()
recordAppliedTransaction hid tx before after = instruction $ RecordAppliedTransaction hid tx before after
{-# INLINE recordAppliedTransaction #-}

type TransactionProgram tx = Program tx 'True 'False
type SnapshotProgram tx = Program tx 'True 'True
type AcknowledgementProgram tx = Program tx 'False 'False
type HeadProgram tx = Program tx 'True 'True

-- | Run a narrower program with the capabilities available at the input boundary.
promoteProgram :: Program tx ledger signing a -> HeadProgram tx a
promoteProgram = foldProgram $ \case
  ApplyTransaction slot utxo tx -> instruction $ ApplyTransaction slot utxo tx
  SignSnapshot snapshot -> instruction $ SignSnapshot snapshot
  VerifySnapshot keys signatures bytes -> instruction $ VerifySnapshot keys signatures bytes
  RecordAppliedTransaction hid tx before after -> instruction $ RecordAppliedTransaction hid tx before after
  Change event -> change event
  Emit effect -> emit effect
  Decide label answer -> decide label answer
  Finish -> instruction Finish
  Suspend reason -> instruction $ Suspend reason
  Reject err -> instruction $ Reject err

-- | Delimit successful early completion. Waiting and rejection still end the
-- entire update; finishing resumes the caller after this handler.
handler :: Program tx ledger signing () -> Program tx ledger signing ()
handler (Program program) = Program $ \done step ->
  program done $ \op next -> case op of
    Finish -> done ()
    _ -> step op next

-- | Complete the nearest handler, or the whole program when outside a handler.
finish :: Program tx ledger signing a
finish = absurd <$> instruction Finish
{-# INLINE finish #-}

suspend :: WaitReason tx -> Program tx ledger signing a
suspend reason = absurd <$> instruction (Suspend reason)
{-# INLINE suspend #-}

reject :: LogicError tx -> Program tx ledger signing a
reject reason = absurd <$> instruction (Reject reason)
{-# INLINE reject #-}

-- | Fold the actual rule into another interpretation, including reification,
-- controlled ledger responses, or operation counting.
foldProgram :: forall m tx ledger signing a. Monad m => (forall x. Instruction tx ledger signing x -> m x) -> Program tx ledger signing a -> m a
foldProgram interpret (Program program) = program pure (\op next -> interpret op >>= next)
{-# INLINE foldProgram #-}
