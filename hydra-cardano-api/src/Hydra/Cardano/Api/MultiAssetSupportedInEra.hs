module Hydra.Cardano.Api.MultiAssetSupportedInEra where

import Hydra.Cardano.Api.Prelude

-- | Smart-constructor for 'MultiAssetSupportedInEra' to write functions
-- manipulating values that do not commit to a particular era.
class HasMultiAsset era where
  multiAssetSupportedInEra :: MultiAssetSupportedInEra era

instance HasMultiAsset MaryEra where
  multiAssetSupportedInEra = MultiAssetInMaryEra

instance HasMultiAsset AlonzoEra where
  multiAssetSupportedInEra = MultiAssetInAlonzoEra
