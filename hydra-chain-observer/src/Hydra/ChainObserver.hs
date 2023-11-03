{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver (
  main,
) where

import Hydra.Prelude

import Hydra.Cardano.Api (CardanoMode, ChainSyncClient, ConsensusModeParams (..), EpochSlots (..), LocalChainSyncClient (..), LocalNodeClientProtocols (..), LocalNodeConnectInfo (..), NetworkId, SocketPath, connectToLocalNode)
import Hydra.Chain (HeadId (..))
import Hydra.ChainObserver.Options (Options (..), hydraChainObserverOptions)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Options.Applicative (execParser)
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )

main :: IO ()
main = do
  Options{networkId, nodeSocket} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer ConnectingToNode{nodeSocket, networkId}
    connectToLocalNode
      (connectInfo nodeSocket networkId)
      (clientProtocols tracer)

type ChainObserverLog :: Type
data ChainObserverLog
  = ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | HeadInitTx {headId :: HeadId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

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

clientProtocols ::
  Tracer IO ChainObserverLog ->
  LocalNodeClientProtocols block point tip slot tx txid txerr query IO
clientProtocols tracer =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

chainSyncClient :: Tracer IO ChainObserverLog -> ChainSyncClient block point tip IO ()
chainSyncClient tracer =
  ChainSyncClient $ do
    traceWith tracer HeadInitTx{headId = HeadId "foo"}
    pure $ SendMsgDone ()
