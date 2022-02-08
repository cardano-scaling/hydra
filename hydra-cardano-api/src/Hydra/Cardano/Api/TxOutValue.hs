module Hydra.Cardano.Api.TxOutValue where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.MultiAssetSupportedInEra (HasMultiAsset (..))

-- | Inject some 'Lovelace' value directly into a 'TxOutValue'
lovelaceToTxOutValue ::
  forall era.
  (HasMultiAsset era) =>
  Lovelace ->
  TxOutValue era
lovelaceToTxOutValue =
  TxOutValue (multiAssetSupportedInEra @era) . lovelaceToValue

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue ::
  forall era.
  (HasMultiAsset era) =>
  Value ->
  TxOutValue era
mkTxOutValue =
  TxOutValue (multiAssetSupportedInEra @era)
