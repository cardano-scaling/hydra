-- | Ledger application with the output-collision rule shared by requests and
-- snapshot pruning. These programs run inside the surrounding workflow phase.
module Hydra.HeadLogic.Rules.Transactions where

import Data.Sequence qualified as Seq
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Program (SnapshotProgram)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotCapacity (checkSnapshotCapacity)
import Hydra.HeadLogic.State.Operations (namesNoneOf)
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Prelude
import Hydra.Tx (IsTx, UTxOType, utxoFromTx)

-- | Apply txs, refusing one that creates an output the UTxO already holds.
-- Impossible on L1, possible on L2: a deposit can name a tx's outputs before
-- that tx is submitted.
applyTransactionsWithoutCollision :: IsTx tx => Ledger tx -> ChainSlot -> UTxOType tx -> [tx] -> Either (tx, ValidationError) (UTxOType tx)
applyTransactionsWithoutCollision Ledger{applyTransactions = apply} slot =
  foldlM $ \utxo tx -> do
    first (tx,) $ checkTransactionOutputs utxo tx
    apply slot utxo [tx]

-- | The collision guard is a rule; the interpreter only applies one transaction.
applyTransactionsProgram :: IsTx tx => ChainSlot -> UTxOType tx -> [tx] -> SnapshotProgram tx (Either (tx, ValidationError) (UTxOType tx))
applyTransactionsProgram slot = go
 where
  go u [] = pure $ Right u
  go u (tx : rest) = do
    let collision = checkTransactionOutputs u tx
    void $ Program.decide "snapshot transaction outputs are fresh" (isRight collision)
    result <- case collision of
      Left err -> pure $ Left err
      Right () -> Program.applyTransaction slot u tx
    case result of
      Left err -> pure $ Left (tx, err)
      Right u' -> go u' rest

-- | Shared admission check for transaction requests and snapshot application.
checkTransactionOutputs :: IsTx tx => UTxOType tx -> tx -> Either ValidationError ()
checkTransactionOutputs utxo tx
  | namesNoneOf (utxoFromTx tx) utxo = Right ()
  | otherwise = Left $ ValidationError "transaction creates an output the head already holds"

-- | Retain transactions that apply after every preceding retained transaction.
-- Aggregation reconstructs the resulting UTxO from the same ordered sequence.
pruneTransactions :: IsTx tx => ChainSlot -> Maybe (UTxOType tx) -> Maybe (UTxOType tx) -> UTxOType tx -> Seq tx -> SnapshotProgram tx (Seq tx)
pruneTransactions slot commit decommit = go
 where
  go _ Seq.Empty = pure Seq.empty
  go utxo (tx Seq.:<| rest) = do
    result <- applyTransactionsProgram slot utxo [tx]
    case result of
      Left _ -> go utxo rest
      Right after -> do
        disjoint <- Program.decide "pending transaction outputs avoid the carried deposit" (namesNoneOf (utxoFromTx tx) $ fold commit)
        fits <- Program.decide "pending transaction fits both fanout alternatives" (isRight $ checkSnapshotCapacity after commit decommit)
        if disjoint && fits then (tx Seq.<|) <$> go after rest else go utxo rest
