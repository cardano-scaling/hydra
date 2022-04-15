{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Cardano.Api.Point where

import Hydra.Cardano.Api.Prelude

import Data.Maybe (fromJust)
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

fromConsensusPointHF ::
  Consensus.HeaderHash block ~ Consensus.OneEraHash xs =>
  Consensus.Point block ->
  ChainPoint
fromConsensusPointHF Consensus.GenesisPoint = ChainPointAtGenesis
fromConsensusPointHF (Consensus.BlockPoint slot (Consensus.OneEraHash h)) =
  ChainPoint slot (fromJust $ deserialiseFromRawBytes (proxyToAsType Proxy) (fromShort h))

-- NOTE: convenient orphan to compare points
instance Ord ChainPoint where
  compare ChainPointAtGenesis ChainPointAtGenesis = EQ
  compare ChainPointAtGenesis _ = LT
  compare _ ChainPointAtGenesis = GT
  compare (ChainPoint sn _) (ChainPoint sn' _) = compare sn sn'
