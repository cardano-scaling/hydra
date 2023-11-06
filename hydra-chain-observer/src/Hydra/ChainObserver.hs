{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver (
  main,
) where

import Hydra.Prelude

import Hydra.Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), CardanoMode, ChainPoint, ChainSyncClient, ConsensusModeParams (..), EpochSlots (..), EraInMode (..), LocalChainSyncClient (..), LocalNodeClientProtocols (..), LocalNodeConnectInfo (..), NetworkId, SocketPath, connectToLocalNode)
import Hydra.Chain (HeadId (..))
import Hydra.ChainObserver.Options (Options (..), hydraChainObserverOptions)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Options.Applicative (execParser)
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
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
  | Rollback {point :: ChainPoint}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

type BlockType = BlockInMode CardanoMode

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
  LocalNodeClientProtocols BlockType ChainPoint tip slot tx txid txerr query IO
clientProtocols tracer =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

-- | Fetch all blocks via chain sync and trace their contents.
chainSyncClient :: Tracer IO ChainObserverLog -> ChainSyncClient BlockType ChainPoint tip IO ()
chainSyncClient tracer =
  ChainSyncClient $ do
    pure $ SendMsgRequestNext clientStNext (pure clientStNext)
 where
  clientStIdle :: ClientStIdle BlockType ChainPoint tip IO ()
  clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

  clientStNext :: ClientStNext BlockType ChainPoint tip IO ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode (Block (BlockHeader _slotNo _hash blockNo) _txs) BabbageEraInCardanoMode -> do
              -- FIXME: process transactions
              print blockNo
              traceWith tracer HeadInitTx{headId = HeadId (show blockNo)}
              pure clientStIdle
            _ -> pure clientStIdle
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure clientStIdle
      }
