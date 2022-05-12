module Hydra.Cardano.Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra where

import Hydra.Cardano.Api.Prelude

-- | Smart-constructor for 'ReferenceTxInsScriptsInlineDatumsSupportedInEra' to
-- write functions using inline datums that do not commit to a particular era.
class HasInlineDatums era where
  inlineDatumsSupportedInEra :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era

instance HasInlineDatums BabbageEra where
  inlineDatumsSupportedInEra = ReferenceTxInsScriptsInlineDatumsInBabbageEra
