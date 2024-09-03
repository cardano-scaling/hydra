module Hydra.Tx.BlueprintTx where

import Hydra.Tx.IsTx (UTxOType)

-- | _Blueprint/Draft_ transaction paired with the 'UTxO' which resolves it's inputs.
-- The transaction inputs are committed to a `Head` and the 'lookupUTxO' is expected
-- to contain these inputs.
data CommitBlueprintTx tx = CommitBlueprintTx {lookupUTxO :: UTxOType tx, blueprintTx :: tx}
