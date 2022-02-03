module Hydra.Cardano.Api.TxOutValue where

import Hydra.Cardano.Api.Prelude

-- | Inject some 'Lovelace' value directly into a 'TxOutValue'
lovelaceToTxOutValue :: Lovelace -> TxOutValue Era
lovelaceToTxOutValue =
  TxOutValue MultiAssetInAlonzoEra . lovelaceToValue

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue :: Value -> TxOutValue Era
mkTxOutValue =
  TxOutValue MultiAssetInAlonzoEra
