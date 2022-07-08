module Hydra.Cardano.Api.CtxTx where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxOut (modifyTxOutDatum)

-- | A convenient type-class for transforming types in 'CtxUTxO' to 'CtxTx'.
--
-- See also 'ToUtxoContext' for the reverse.
class ToTxContext f where
  toTxContext :: f CtxUTxO era -> f CtxTx era

instance ToTxContext TxOutDatum where
  toTxContext = \case
    TxOutDatumNone -> TxOutDatumNone
    TxOutDatumHash era h -> TxOutDatumHash era h
    TxOutDatumInline s sd -> TxOutDatumInline s sd

instance ToTxContext TxOut where
  toTxContext =
    modifyTxOutDatum toTxContext
