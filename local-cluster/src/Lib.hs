{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Cardano.Prelude

import Control.Concurrent.Async(Async)
import Data.Time.Clock(getCurrentTime, UTCTime, addUTCTime)

import qualified Data.List as L

data RunningCluster = RunningCluster ClusterConfig [(Async (), Port)]

-- | Value corresponding to running process.
data RunningNode = RunningNode NodeConfig (Async ())

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
    { stateDirectory :: FilePath
    }

-- | Configuration parameters for a single node of the cluster.
data NodeConfig = NodeConfig
    { parentStateDirectory :: FilePath
      -- ^ Parent state directory in which create a state directory for the cluster
    , systemStart :: UTCTime
      -- ^ Genesis block (Byron) start time
    , ports :: PortsConfig
      -- ^ A list of port
    }

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully connected topology.
data PortsConfig = PortsConfig
    { ours :: Port
      -- ^ Our node TCP port.
    , peers ::  [Port]
      -- ^ Other peers TCP ports.
    }

type Port = Int

withCluster
    :: ClusterConfig
    -> (RunningCluster -> IO ())
    -> IO ()
withCluster =
    panic "TODO"

withBFTNode
    :: NodeConfig
    -> (RunningNode -> IO ())
    -> IO ()
withBFTNode =
    panic "TODO"

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart = do
    addUTCTime 1 <$> getCurrentTime

-- | Get permutations of the size (n-1) for a list of n elements, alongside
-- with the element left aside. `[a]` is really expected to be `Set a`.
--
-- >>> rotate [1,2,3]
-- [(1,[2,3]), (2, [1,3]), (3, [1,2])]
rotate :: Ord a => [a] -> [(a, [a])]
rotate =
    L.nub . fmap (\(x:xs) -> (x, L.sort xs)) . L.permutations
