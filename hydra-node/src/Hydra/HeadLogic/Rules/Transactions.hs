module Hydra.HeadLogic.Rules.Transactions (ValidatedTransaction, validateTransaction, applyTransactionsWithoutCollision, applyTransactionsProgram, checkTransactionOutputs, pruneTransactions) where

import Data.Sequence qualified as Seq (
  Seq (Empty, (:<|)),
  empty,
  (<|),
 )
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Limits (SnapshotLimits)
import Hydra.HeadLogic.Program (
  Program,
  applyTransaction,
  decide,
 )
import Hydra.HeadLogic.Snapshot.Capacity (checkSnapshotCapacity)
import Hydra.HeadLogic.Transaction.Outputs (namesNoneOf)
import Hydra.HeadLogic.Validation (ValidatedTransaction (..))
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Prelude
import Hydra.Tx (IsTx, UTxOType, utxoFromTx)

-- | Apply transactions, refusing any that creates an output the UTxO already
-- holds. Impossible on L1, possible on L2: a deposit can name a tx's outputs
-- before that tx is submitted.
applyTransactionsWithoutCollision ::
  IsTx tx =>
  Ledger tx ->
  ChainSlot ->
  UTxOType tx ->
  [tx] ->
  Either (tx, ValidationError) (UTxOType tx)
applyTransactionsWithoutCollision Ledger{applyTransactions = apply} slot =
  foldlM $ \utxo tx -> do
    first (tx,) $ checkTransactionOutputs utxo tx
    apply slot utxo [tx]

-- | Validate one ledger application, retaining its result for the live owner.
-- The caller supplies the decision label used in its surrounding workflow.
validateTransaction :: IsTx tx => Text -> ChainSlot -> UTxOType tx -> tx -> Program tx (Either ValidationError (ValidatedTransaction tx))
validateTransaction decision slot before tx = do
  let collision = checkTransactionOutputs before tx
  void $ decide decision (isRight collision)
  case collision of
    Left err -> pure $ Left err
    Right () -> fmap (ValidatedTransaction tx) <$> applyTransaction slot before tx

-- | Apply the same validation used for a transaction request to each member of
-- the snapshot in order, stopping at its first invalid transaction.
applyTransactionsProgram :: IsTx tx => ChainSlot -> UTxOType tx -> [tx] -> Program tx (Either (tx, ValidationError) (UTxOType tx))
applyTransactionsProgram slot = go
 where
  go u [] = pure $ Right u
  go u (tx : rest) = do
    result <- validateTransaction "snapshot transaction outputs are fresh" slot u tx
    case result of
      Left err -> pure $ Left (tx, err)
      Right (ValidatedTransaction _ after) -> go after rest

checkTransactionOutputs ::
  IsTx tx =>
  UTxOType tx ->
  tx ->
  Either ValidationError ()
checkTransactionOutputs utxo tx
  | namesNoneOf (utxoFromTx tx) utxo = Right ()
  | otherwise = Left $ ValidationError "transaction creates an output the head already holds"

-- | Retain transactions that apply after every preceding retained transaction.
pruneTransactions ::
  IsTx tx =>
  SnapshotLimits ->
  ChainSlot ->
  Maybe (UTxOType tx) ->
  Maybe (UTxOType tx) ->
  UTxOType tx ->
  Seq tx ->
  Program tx (Seq tx)
pruneTransactions limits slot commit decommit = go
 where
  go _ Seq.Empty = pure Seq.empty
  go utxo (tx Seq.:<| rest) = do
    result <- applyTransactionsProgram slot utxo [tx]
    case result of
      Left _ -> go utxo rest
      Right after -> do
        disjoint <-
          decide
            "pending transaction outputs avoid the carried deposit"
            (namesNoneOf (utxoFromTx tx) $ fold commit)

        fits <-
          decide
            "pending transaction fits both fanout alternatives"
            (isRight $ checkSnapshotCapacity limits after commit decommit)

        if disjoint && fits
          then (tx Seq.<|) <$> go after rest
          else go utxo rest
