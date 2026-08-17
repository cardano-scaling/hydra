-- | Test-only helpers for shaping 'UTxO' sets, used by generators and
-- mutation tests.
module Test.Hydra.Tx.Utils where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO

-- | Split a given UTxO into two, such that the second UTxO is non-empty. This
-- is useful to pick a UTxO to decommit.
splitUTxO :: UTxO -> (UTxO, UTxO)
splitUTxO utxo =
  case UTxO.toList utxo of
    [] -> (mempty, mempty)
    ((u, o) : us) -> (UTxO.fromList us, UTxO.singleton u o)

adaOnly :: TxOut CtxUTxO -> TxOut CtxUTxO
adaOnly = \case
  TxOut addr value datum refScript ->
    TxOut addr (lovelaceToValue $ selectLovelace value) datum refScript
