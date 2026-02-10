module Test.Hydra.Cluster.Utils where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-cardano-api" Hydra.Cardano.Api (ChainPoint, SlotNo, chainPointToSlotNo)

import Hydra.Cluster.Fixture (KnownNetwork (..))

-- | Creates test cases for each 'KnownNetwork'.
forEachKnownNetwork :: String -> (KnownNetwork -> IO ()) -> Spec
forEachKnownNetwork msg action =
  forM_ (enumFromTo minBound maxBound) $ \network ->
    it (msg <> " (" <> show network <> ")") $ action network

chainPointToSlot :: ChainPoint -> SlotNo
chainPointToSlot chainPoint =
  fromMaybe 0 $ chainPointToSlotNo chainPoint
