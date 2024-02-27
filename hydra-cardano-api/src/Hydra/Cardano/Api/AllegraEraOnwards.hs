module Hydra.Cardano.Api.AllegraEraOnwards where

import Cardano.Api (AllegraEra, AllegraEraOnwards (..), AlonzoEra, BabbageEra, ConwayEra, MaryEra)

-- | Type class to produce 'AllegraEraOnwards' witness values while staying
-- parameterized by era.
class IsAllegraEraOnwards era where
  allegraEraOnwards :: AllegraEraOnwards era

instance IsAllegraEraOnwards AllegraEra where
  allegraEraOnwards = AllegraEraOnwardsAllegra

instance IsAllegraEraOnwards MaryEra where
  allegraEraOnwards = AllegraEraOnwardsMary

instance IsAllegraEraOnwards AlonzoEra where
  allegraEraOnwards = AllegraEraOnwardsAlonzo

instance IsAllegraEraOnwards BabbageEra where
  allegraEraOnwards = AllegraEraOnwardsBabbage

instance IsAllegraEraOnwards ConwayEra where
  allegraEraOnwards = AllegraEraOnwardsConway
