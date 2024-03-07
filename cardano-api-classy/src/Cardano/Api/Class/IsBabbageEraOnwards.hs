module Cardano.Api.Class.IsBabbageEraOnwards where

import Cardano.Api (BabbageEra, BabbageEraOnwards (..), ConwayEra)

-- | Type class to produce 'BabbageEraOnwards' witness values while staying
-- parameterized by era.
class IsBabbageEraOnwards era where
  babbageEraOnwards :: BabbageEraOnwards era

instance IsBabbageEraOnwards BabbageEra where
  babbageEraOnwards = BabbageEraOnwardsBabbage

instance IsBabbageEraOnwards ConwayEra where
  babbageEraOnwards = BabbageEraOnwardsConway
