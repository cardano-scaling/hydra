module Hydra.Cardano.Api.MaryEraOnwards where

import Cardano.Api (AlonzoEra, BabbageEra, ConwayEra, MaryEra, MaryEraOnwards (..))

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
