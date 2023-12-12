module Hydra.Cardano.Api.MaryEraOnwards where

import Hydra.Cardano.Api.Prelude

-- | Type class to produce 'MaryEraOnwards' witness values while staying
-- parameterized by era.
class IsMaryEraOnwards era where
  maryEraOnwards :: MaryEraOnwards era

instance IsMaryEraOnwards MaryEra where
  maryEraOnwards = MaryEraOnwardsMary

instance IsMaryEraOnwards AlonzoEra where
  maryEraOnwards = MaryEraOnwardsAlonzo

instance IsMaryEraOnwards BabbageEra where
  maryEraOnwards = MaryEraOnwardsBabbage

instance IsMaryEraOnwards ConwayEra where
  maryEraOnwards = MaryEraOnwardsConway
