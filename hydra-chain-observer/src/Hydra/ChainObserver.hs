{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver (
  main,
) where

import Hydra.Prelude

import Hydra.Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalChainSyncClient (..), LocalNodeClientProtocols (..), LocalNodeConnectInfo (..), NetworkId, SocketPath, connectToLocalNode)
import Hydra.ChainObserver.Options (Options (..), hydraChainObserverOptions)
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Options.Applicative (execParser)

main :: IO ()
main = do
  Options{networkId, nodeSocket} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer ConnectingToNode{nodeSocket, networkId}
    connectToLocalNode
      (connectInfo nodeSocket networkId)
      clientProtocols

connectInfo :: SocketPath -> NetworkId -> LocalNodeConnectInfo CardanoMode
connectInfo nodeSocket networkId =
  LocalNodeConnectInfo
    { -- REVIEW: This was 432000 before, but all usages in the
      -- cardano-node repository are using this value. This is only
      -- relevant for the Byron era.
      localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
    , localNodeNetworkId = networkId
    , localNodeSocketPath = nodeSocket
    }

clientProtocols :: LocalNodeClientProtocols block point tip slot tx txid txerr query m
clientProtocols =
  LocalNodeClientProtocols
    { localChainSyncClient = NoLocalChainSyncClient
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

type ChainObserverLog :: Type
data ChainObserverLog = ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
