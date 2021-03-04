{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Cardano.Prelude

import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Logging (Tracer, HasSeverityAnnotation(..), Severity(..))

data RunningCluster = RunningCluster ClusterConfig [(Async (), Port)]

-- | Value corresponding to running process.
data RunningNode = RunningNode NodeConfig (Async ())

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
  { stateDirectory :: FilePath
  }

-- | Configuration parameters for a single node of the cluster.
data NodeConfig = NodeConfig
  { -- | Parent state directory in which create a state directory for the cluster
    parentStateDirectory :: FilePath,
    -- | Genesis block (Byron) start time
    systemStart :: UTCTime,
    -- | A list of port
    ports :: PortsConfig
  }

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully connected topology.
data PortsConfig = PortsConfig
  { -- | Our node TCP port.
    ours :: Port,
    -- | Other peers TCP ports.
    peers :: [Port]
  }

type Port = Int

withCluster ::
  Tracer IO ClusterLog ->
  ClusterConfig ->
  (RunningCluster -> IO ()) ->
  IO ()
withCluster =
  panic "TODO"

withBFTNode ::
  NodeConfig ->
  (RunningNode -> IO ()) ->
  IO ()
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
rotate :: a -> a -> a -> [(a, [a])]
rotate a b c =
    [ (a, [b,c])
    , (b, [a,c])
    , (c, [a,b])
    ]

--
-- Logging
--

data ClusterLog = ClusterLog deriving Show

instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        ClusterLog -> Info
