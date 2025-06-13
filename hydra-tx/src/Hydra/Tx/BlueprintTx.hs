module Hydra.Tx.BlueprintTx where

import Hydra.Tx.IsTx (IsTx, UTxOType, txSpendingUTxO)

-- | _Blueprint/Draft_ transaction paired with the 'UTxO' which resolves it's inputs.
-- The transaction inputs are committed to a `Head` and the 'lookupUTxO' is expected
-- to contain these inputs.
data CommitBlueprintTx tx = CommitBlueprintTx {lookupUTxO :: UTxOType tx, blueprintTx :: tx}

mkSimpleBlueprintTx :: IsTx tx => UTxOType tx -> CommitBlueprintTx tx
mkSimpleBlueprintTx utxo =
  CommitBlueprintTx
    { lookupUTxO = utxo
    , blueprintTx = txSpendingUTxO utxo
    }
