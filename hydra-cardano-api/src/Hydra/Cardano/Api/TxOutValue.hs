module Hydra.Cardano.Api.TxOutValue where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.MultiAssetSupportedInEra (HasMultiAsset (..))

-- | Inject some 'Value' into a 'TxOutValue'
mkTxOutValue ::
  forall era.
  (HasMultiAsset era) =>
  Value ->
  TxOutValue era
mkTxOutValue =
  TxOutValue (multiAssetSupportedInEra @era)
