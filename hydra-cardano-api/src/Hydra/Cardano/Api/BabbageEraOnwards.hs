module Hydra.Cardano.Api.BabbageEraOnwards where

import Hydra.Cardano.Api.Prelude

-- | Type class to produce 'BabbageEraOnwards' witness values while staying
-- parameterized by era.
class IsBabbageEraOnwards era where
  babbageEraOnwards :: BabbageEraOnwards era

instance IsBabbageEraOnwards BabbageEra where
  babbageEraOnwards = BabbageEraOnwardsBabbage

instance IsBabbageEraOnwards ConwayEra where
  babbageEraOnwards = BabbageEraOnwardsConway
