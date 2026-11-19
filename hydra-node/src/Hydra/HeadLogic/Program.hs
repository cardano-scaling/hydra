{-# LANGUAGE RankNTypes #-}

-- | The typed instruction language and composition of HeadLogic rules.
module Hydra.HeadLogic.Program (
  Program (..),
  Instruction (..),
  instruction,
  change,
  emit,
  decide,
  applyTransaction,
  signSnapshot,
  verifySnapshot,
  suspend,
  reject,
  foldProgram,
) where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.Outcome (Effect, StateChanged, WaitReason)
import Hydra.Ledger (ValidationError)
import Hydra.Prelude hiding (keys, label)
import Hydra.Tx (UTxOType)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, Signature, VerificationKey, Verified)
import Hydra.Tx.Snapshot (Snapshot)

-- | Church encoding of the operational free monad. Composition does not allocate
-- an intermediate instruction tree; an interpreter can still reify one using
-- 'foldProgram'.
newtype Program tx a = Program
  { interpretProgram ::
      forall r.
      (a -> r) ->
      (forall x. Instruction tx x -> (x -> r) -> r) ->
      r
  }

data Instruction tx a where
  ApplyTransaction ::
    ChainSlot ->
    UTxOType tx ->
    tx ->
    Instruction tx (Either ValidationError (UTxOType tx))
  SignSnapshot ::
    Snapshot tx ->
    Instruction tx (Signature (Snapshot tx))
  VerifySnapshot ::
    [VerificationKey HydraKey] ->
    MultiSignature (Snapshot tx) ->
    ByteString ->
    Instruction tx Verified
  Change ::
    StateChanged tx ->
    Instruction tx ()
  Emit ::
    Effect tx ->
    Instruction tx ()
  Decide ::
    Text ->
    Bool ->
    Instruction tx Bool
  Suspend ::
    WaitReason tx ->
    Instruction tx Void
  Reject ::
    LogicError tx ->
    Instruction tx Void

instance Functor (Program tx) where
  fmap f (Program program) = Program $ \done step -> program (done . f) step
  {-# INLINE fmap #-}

instance Applicative (Program tx) where
  pure a = Program $ \done _ -> done a
  {-# INLINE pure #-}
  Program function <*> Program argument = Program $ \done step ->
    function (\f -> argument (done . f) step) step
  {-# INLINE (<*>) #-}

instance Monad (Program tx) where
  Program program >>= next = Program $ \done step ->
    program (\a -> interpretProgram (next a) done step) step
  {-# INLINE (>>=) #-}

instruction :: Instruction tx a -> Program tx a
instruction op = Program $ \done step -> step op done
{-# INLINE instruction #-}

change :: StateChanged tx -> Program tx ()
change = instruction . Change
{-# INLINE change #-}

emit :: Effect tx -> Program tx ()
emit = instruction . Emit
{-# INLINE emit #-}

decide :: Text -> Bool -> Program tx Bool
decide label = instruction . Decide label
{-# INLINE decide #-}

applyTransaction ::
  ChainSlot ->
  UTxOType tx ->
  tx ->
  Program tx (Either ValidationError (UTxOType tx))
applyTransaction slot utxo tx = instruction $ ApplyTransaction slot utxo tx
{-# INLINE applyTransaction #-}

signSnapshot :: Snapshot tx -> Program tx (Signature (Snapshot tx))
signSnapshot = instruction . SignSnapshot

verifySnapshot ::
  [VerificationKey HydraKey] ->
  MultiSignature (Snapshot tx) ->
  ByteString ->
  Program tx Verified
verifySnapshot keys signatures bytes = instruction $ VerifySnapshot keys signatures bytes

suspend :: WaitReason tx -> Program tx a
suspend reason = absurd <$> instruction (Suspend reason)
{-# INLINE suspend #-}

reject :: LogicError tx -> Program tx a
reject reason = absurd <$> instruction (Reject reason)
{-# INLINE reject #-}

-- | Fold the actual rule into another interpretation, including reification,
-- controlled ledger responses, or operation counting.
foldProgram ::
  forall m tx a.
  Monad m =>
  (forall x. Instruction tx x -> m x) ->
  Program tx a ->
  m a
foldProgram interpret (Program program) = program pure (\op next -> interpret op >>= next)
{-# INLINE foldProgram #-}
