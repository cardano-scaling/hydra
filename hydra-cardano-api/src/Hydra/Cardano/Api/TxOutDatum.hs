module Hydra.Cardano.Api.TxOutDatum where

import Cardano.Api
import Hydra.Prelude

import Hydra.Cardano.Api.ScriptData (
  ToScriptData,
  asScriptData,
 )

-- | Construct a 'TxOutDatum' as plain 'ScriptData' from some serialisable data
-- type. Note that this is only possible in 'CtxTx' for 'CtxUTxO' only allows to
-- embed hashes.
mkTxOutDatum :: (ToScriptData a) => a -> TxOutDatum CtxTx AlonzoEra
mkTxOutDatum =
  TxOutDatum ScriptDataInAlonzoEra . asScriptData

-- | Construct a 'TxOutDatum' as a 'ScriptData' hash, from some serialisable
-- data type.
mkTxOutDatumHash :: (ToScriptData a) => a -> TxOutDatum ctx AlonzoEra
mkTxOutDatumHash =
  TxOutDatumHash ScriptDataInAlonzoEra . hashScriptData . asScriptData
