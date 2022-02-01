module Hydra.Cardano.Api.CtxTx where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxOut (modifyTxOutDatum)

-- | A convenient type-class for transforming types in 'CtxUTxO' to 'CtxTx'.
--
-- See also 'ToUtxoContext' for the reverse.
class ToTxContext f where
  toTxContext :: f CtxUTxO Era -> f CtxTx Era

instance ToTxContext TxOutDatum where
  toTxContext = \case
    TxOutDatumNone -> TxOutDatumNone
    TxOutDatumHash era h -> TxOutDatumHash era h

instance ToTxContext TxOut where
  toTxContext =
    modifyTxOutDatum toTxContext
