module Hydra.Cardano.Api.AlonzoEraOnwards where

import Cardano.Api (AlonzoEra, AlonzoEraOnwards (..), BabbageEra, ConwayEra)

-- | Type class to produce 'AlonzoEraOnwards' witness values while staying
-- parameterized by era.
class IsAlonzoEraOnwards era where
  alonzoEraOnwards :: AlonzoEraOnwards era

instance IsAlonzoEraOnwards AlonzoEra where
  alonzoEraOnwards = AlonzoEraOnwardsAlonzo

instance IsAlonzoEraOnwards BabbageEra where
  alonzoEraOnwards = AlonzoEraOnwardsBabbage

instance IsAlonzoEraOnwards ConwayEra where
  alonzoEraOnwards = AlonzoEraOnwardsConway
