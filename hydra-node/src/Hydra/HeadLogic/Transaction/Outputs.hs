{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Transaction.Outputs where

import Hydra.Prelude
import Hydra.Tx (IsTx (UTxOType, utxoFromTx, withoutUTxO))

-- | Whether a request applying these txs to this UTxO can absorb the deposit:
-- it must name no output the UTxO holds or the txs create. Deposit datums are
-- not authenticated, so anyone can name existing outputs, and absorbing such a
-- deposit would count value the ledger does not have.
absorbable :: IsTx tx => UTxOType tx -> [tx] -> UTxOType tx -> Bool
absorbable utxo txs =
  let created = foldMap utxoFromTx txs
   in \deposited -> namesNoneOf deposited utxo && namesNoneOf deposited created

-- | Whether a deposit of the given outputs names none of the output references
-- the given UTxO holds.
namesNoneOf :: IsTx tx => UTxOType tx -> UTxOType tx -> Bool
namesNoneOf deposited utxo =
  -- NOTE: remove the UTxO from the deposit, not the deposit from the UTxO: the
  -- deposit is small, so this is cheaper.
  deposited `withoutUTxO` utxo == deposited
