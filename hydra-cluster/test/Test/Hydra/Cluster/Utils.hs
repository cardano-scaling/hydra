module Test.Hydra.Cluster.Utils where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cluster.Fixture (KnownNetwork (..))

-- | Creates test cases for each 'KnownNetwork'.
forEachKnownNetwork :: String -> (KnownNetwork -> IO ()) -> Spec
forEachKnownNetwork msg action =
  forM_ (enumFromTo minBound maxBound) $ \network ->
    it (msg <> " (" <> show network <> ")") $ action network
