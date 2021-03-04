{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Cardano.Prelude

import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Logging (HasSeverityAnnotation (..), Severity (..), Tracer)
import Ports (randomUnusedTCPPorts)

data RunningCluster = RunningCluster ClusterConfig [PortsConfig]

-- | Value corresponding to running process.
data RunningNode = RunningNode NodeConfig

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
    { stateDirectory :: FilePath
    }

-- | Configuration parameters for a single node of the cluster.
data NodeConfig = NodeConfig
    { -- | Parent state directory in which create a state directory for the cluster
      parentStateDirectory :: FilePath
    , -- | Genesis block (Byron) start time
      systemStart :: UTCTime
    , -- | A list of port
      ports :: PortsConfig
    }

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully connected topology.
data PortsConfig = PortsConfig
    { -- | Our node TCP port.
      ours :: Port
    , -- | Other peers TCP ports.
      peers :: [Port]
    }

type Port = Int

withCluster ::
    Tracer IO ClusterLog ->
    ClusterConfig ->
    (RunningCluster -> IO ()) ->
    IO ()
withCluster tracer ClusterConfig{stateDirectory} action = do
    systemStart <- initSystemStart
    (cfgA, cfgB, cfgC) <- makeNodesConfig stateDirectory systemStart <$> randomUnusedTCPPorts 3
    withBFTNode tracer cfgA $ \_ -> do
        withBFTNode tracer cfgB $ \_ -> do
            withBFTNode tracer cfgC $ \_ -> do
                action (RunningCluster (ClusterConfig stateDirectory) (fmap ports [cfgA, cfgB, cfgC]))

withBFTNode ::
    Tracer IO ClusterLog ->
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

makeNodesConfig :: FilePath -> UTCTime -> [Port] -> (NodeConfig, NodeConfig, NodeConfig)
makeNodesConfig stateDirectory systemStart [a, b, c] =
    ( NodeConfig stateDirectory systemStart $ PortsConfig a [b, c]
    , NodeConfig stateDirectory systemStart $ PortsConfig b [a, c]
    , NodeConfig stateDirectory systemStart $ PortsConfig c [a, b]
    )
makeNodesConfig _ _ _ = panic "we only support topology for 3 nodes"

--
-- Logging
--

data ClusterLog = ClusterLog deriving (Show)

instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        ClusterLog -> Info
