module Hydra.Cardano.Api.Point where

import Hydra.Cardano.Api.Prelude

import qualified Ouroboros.Consensus.Block as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus

-- | TODO: This function is available in more recent versions of the
-- cardano-api, once we upgrade the dependency set, we can ditch it.
toConsensusPointHF ::
  Consensus.HeaderHash block ~ Consensus.OneEraHash xs =>
  ChainPoint ->
  Consensus.Point block
toConsensusPointHF ChainPointAtGenesis = Consensus.GenesisPoint
toConsensusPointHF (ChainPoint slotNo (serialiseToRawBytes -> headerHash)) =
  Consensus.BlockPoint slotNo (Consensus.OneEraHash (toShort headerHash))
