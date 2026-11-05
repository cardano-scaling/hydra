{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Experimental ReqTx state recovery with explicit preconditions and errors.
-- This format is separate from production persistence and admits no other rules.
module Test.Hydra.HeadLogic.Resolved where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.HeadLogic (
  CoordinatedHeadState (allTxs, localTxs, localUTxO, seenSnapshot),
  HeadState (Open),
  OpenState (coordinatedHeadState, headId),
  SeenSnapshot (RequestedSnapshot),
  StateChanged (
    SnapshotRequestDecided,
    TransactionAppliedToLocalUTxO,
    TransactionReceived,
    TxInvalid,
    headId,
    snapshotNumber,
    transaction,
    tx,
    utxo,
    validationError
  ),
  seenSnapshotNumber,
 )
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.Ledger (ValidationError)
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx (..), UTxOType)
import Hydra.Tx.Snapshot (SnapshotNumber)

data ExperimentError
  = UnsupportedChange
  | RequiresOpenHead
  | WrongHead
  | DeltaDoesNotMatchState
  | TransactionDoesNotMatchRegistry
  deriving stock (Eq, Show)

-- | Retain event diagnostics as well as state operations. The envelope supplies
-- the head identity for applied/invalid transactions and compatibility events.
data ResolvedChange tx
  = Remember tx
  | ApplyDelta tx (UTxOType tx) (UTxOType tx)
  | Invalidate (UTxOType tx) tx ValidationError
  | RequestSnapshot SnapshotNumber
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ResolvedChange tx)
deriving stock instance IsTx tx => Show (ResolvedChange tx)
instance IsTx tx => ToCBOR (ResolvedChange tx) where toCBOR = genericToCBOR
instance IsTx tx => FromCBOR (ResolvedChange tx) where fromCBOR = genericFromCBOR

data ResolvedLog tx = ResolvedLog {headId :: HeadId, changes :: [ResolvedChange tx]}
  deriving stock (Generic)
deriving stock instance IsTx tx => Eq (ResolvedLog tx)
deriving stock instance IsTx tx => Show (ResolvedLog tx)

instance IsTx tx => ToCBOR (ResolvedLog tx) where
  toCBOR (ResolvedLog headId changes) = toCBOR ("hydra-reqtx-resolved-v2" :: Text) <> toCBOR headId <> toCBOR changes
instance IsTx tx => FromCBOR (ResolvedLog tx) where
  fromCBOR = do
    tag <- fromCBOR
    unless (tag == ("hydra-reqtx-resolved-v2" :: Text)) $ fail "unsupported resolved log version"
    ResolvedLog <$> fromCBOR <*> fromCBOR

resolve :: IsTx tx => HeadId -> Interpreter.StagedChange tx -> Either ExperimentError (ResolvedChange tx)
resolve expectedHead = \case
  Interpreter.AppliedTransaction headId tx before after -> do
    unless (headId == expectedHead) $ Left WrongHead
    Right $ ApplyDelta tx (before `withoutUTxO` after) (after `withoutUTxO` before)
  Interpreter.ExistingChange TransactionReceived{tx} -> Right $ Remember tx
  Interpreter.ExistingChange TxInvalid{headId, utxo, transaction, validationError} -> do
    unless (headId == expectedHead) $ Left WrongHead
    Right $ Invalidate utxo transaction validationError
  Interpreter.ExistingChange SnapshotRequestDecided{snapshotNumber} -> Right $ RequestSnapshot snapshotNumber
  _ -> Left UnsupportedChange

-- | Translate only committed changes. Rejection discards its staging buffer
-- before resolution, using exactly the production evaluator's terminal rules.
runResolved :: IsTx tx => HeadId -> Interpreter.InterpreterConfig tx ledger signing -> Program.Program tx ledger signing () -> Either ExperimentError (Interpreter.Result tx (ResolvedChange tx))
runResolved headId config program = case fst $ Interpreter.evaluateChanges id (\() _ -> ()) () config program of
  Interpreter.Completed changes effects -> (`Interpreter.Completed` effects) <$> traverse (resolve headId) changes
  Interpreter.Waiting reason changes -> Interpreter.Waiting reason <$> traverse (resolve headId) changes
  Interpreter.RejectedWith err -> Right $ Interpreter.RejectedWith err

committed :: Interpreter.Result tx event -> [event]
committed = \case
  Interpreter.Completed changes _ -> changes
  Interpreter.Waiting _ changes -> changes
  Interpreter.RejectedWith _ -> []

-- | Restore the original ReqTx events for consumers that need their diagnostics.
-- No ledger operation is needed and StateChanged's encoding stays unchanged.
compatibilityEvents :: ResolvedLog tx -> [StateChanged tx]
compatibilityEvents ResolvedLog{headId, changes} = fmap event changes
 where
  event = \case
    Remember tx -> TransactionReceived tx
    ApplyDelta tx _ _ -> TransactionAppliedToLocalUTxO headId tx
    Invalidate utxo transaction validationError -> TxInvalid{headId, utxo, transaction, validationError}
    RequestSnapshot snapshotNumber -> SnapshotRequestDecided snapshotNumber

-- | Replay explicit UTxO changes in log order. Wrong phase/head, duplicate
-- application and inconsistent deltas fail instead of silently changing nothing.
-- Validation here is structural; no ledger, signing, applyTxTo or rule runs.
replay :: forall tx. IsTx tx => NodeState tx -> ResolvedLog tx -> Either ExperimentError (NodeState tx)
replay initial ResolvedLog{headId, changes} = case initial.headState of
  Open os | os.headId == headId -> foldlM applyChange initial changes
  Open{} -> Left WrongHead
  _ -> Left RequiresOpenHead
 where
  applyChange :: NodeState tx -> ResolvedChange tx -> Either ExperimentError (NodeState tx)
  applyChange st change = case st.headState of
    Open os -> do
      next <- updateCoordinatedState os.coordinatedHeadState change
      pure st{headState = Open os{coordinatedHeadState = next}}
    _ -> Left RequiresOpenHead
  updateCoordinatedState :: CoordinatedHeadState tx -> ResolvedChange tx -> Either ExperimentError (CoordinatedHeadState tx)
  updateCoordinatedState c = \case
    Remember tx -> Right c{allTxs = insert (txId tx) tx c.allTxs}
    Invalidate _ tx _ -> Right c{allTxs = delete (txId tx) c.allTxs}
    RequestSnapshot sn -> Right c{seenSnapshot = RequestedSnapshot (seenSnapshotNumber c.seenSnapshot) sn}
    ApplyDelta tx removed added -> do
      unless (Map.lookup (txId tx) c.allTxs == Just tx && not (any ((== txId tx) . txId) c.localTxs)) $ Left TransactionDoesNotMatchRegistry
      let remaining = c.localUTxO `withoutUTxO` removed
      unless (removed `withoutUTxO` c.localUTxO == mempty && added `withoutUTxO` remaining == added) $ Left DeltaDoesNotMatchState
      pure c{localUTxO = remaining <> added, localTxs = c.localTxs Seq.|> tx}
