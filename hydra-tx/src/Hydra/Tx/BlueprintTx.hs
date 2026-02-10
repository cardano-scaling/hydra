module Hydra.Tx.BlueprintTx where

import "hydra-tx" Hydra.Tx.IsTx (IsTx, UTxOType, txSpendingUTxO)

-- | _Blueprint/Draft_ transaction paired with the related 'UTxO' and used for
-- resolving the inputs and committing/depositing into a Head.
-- Deposit behaviour:
-- - In case blueprint transaction contains outputs then these outputs will be taken into account.
-- - If the blueprint tx contains only inputs then the whole provided UTxO will get deposited into a Head.
-- Commit behaviour:
-- - Only inputs would be considered ignoring any outputs and complete UTxO gets committed.
data CommitBlueprintTx tx = CommitBlueprintTx {lookupUTxO :: UTxOType tx, blueprintTx :: tx}

mkSimpleBlueprintTx :: IsTx tx => UTxOType tx -> CommitBlueprintTx tx
mkSimpleBlueprintTx utxo =
  CommitBlueprintTx
    { lookupUTxO = utxo
    , blueprintTx = txSpendingUTxO utxo
    }
