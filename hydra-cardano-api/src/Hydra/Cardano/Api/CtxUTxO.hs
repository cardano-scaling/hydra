module Hydra.Cardano.Api.CtxUTxO where

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxOut (modifyTxOutDatum)

-- | A convenient type-class for transforming types in 'CtxTx' to 'CtxUTxO'.
--
-- See also 'ToTxContext' for the reverse.
class ToUTxOContext f where
  toUTxOContext :: f CtxTx era -> f CtxUTxO era

instance ToUTxOContext TxOutDatum where
  toUTxOContext = \case
    TxOutDatumNone -> TxOutDatumNone
    TxOutDatumHash s h -> TxOutDatumHash s h
    TxOutDatum s d -> TxOutDatumHash s (hashScriptData d)

instance ToUTxOContext TxOut where
  toUTxOContext =
    modifyTxOutDatum toUTxOContext
