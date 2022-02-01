module Hydra.Cardano.Api.TxOutDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ScriptData (ToScriptData, asScriptData)

-- | Construct a 'TxOutDatum' as plain 'ScriptData' from some serialisable data
-- type. Note that this is only possible in 'CtxTx' for 'CtxUTxO' only allows to
-- embed hashes.
mkTxOutDatum :: (ToScriptData a) => a -> TxOutDatum CtxTx Era
mkTxOutDatum =
  TxOutDatum ScriptDataInAlonzoEra . asScriptData

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash, from some serialisable
-- data type.
mkTxOutDatumHash :: (ToScriptData a) => a -> TxOutDatum ctx Era
mkTxOutDatumHash =
  TxOutDatumHash ScriptDataInAlonzoEra . hashScriptData . asScriptData
