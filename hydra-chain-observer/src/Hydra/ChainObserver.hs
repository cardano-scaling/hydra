{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver (
  main,
) where

import Hydra.Prelude

import Hydra.Cardano.Api (
  Block (..),
  BlockHeader (..),
  BlockInMode (..),
  CardanoMode,
  ChainPoint,
  ChainSyncClient,
  ConsensusModeParams (..),
  EpochSlots (..),
  EraInMode (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  SocketPath,
  UTxO,
  connectToLocalNode,
  utxoFromTx,
 )
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.Tx (
  CollectComObservation (..),
  HeadObservation (..),
  RawCommitObservation (..),
  RawInitObservation (..),
  mkHeadId,
  observeHeadTx,
 )
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
      (clientProtocols tracer networkId)

type ChainObserverLog :: Type
data ChainObserverLog
  = ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | HeadInitTx {headId :: HeadId}
  | HeadCommitTx {headId :: HeadId}
  | HeadCollectComTx {headId :: HeadId}
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
  NetworkId ->
  LocalNodeClientProtocols BlockType ChainPoint tip slot tx txid txerr query IO
clientProtocols tracer networkId =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

-- | Fetch all blocks via chain sync and trace their contents.
chainSyncClient ::
  Tracer IO ChainObserverLog ->
  NetworkId ->
  ChainSyncClient BlockType ChainPoint tip IO ()
chainSyncClient tracer networkId =
  ChainSyncClient $ do
    pure $ SendMsgRequestNext (clientStNext mempty) (pure $ clientStNext mempty)
 where
  clientStIdle :: UTxO -> ClientStIdle BlockType ChainPoint tip IO ()
  clientStIdle utxo = SendMsgRequestNext (clientStNext utxo) (pure $ clientStNext utxo)

  clientStNext :: UTxO -> ClientStNext BlockType ChainPoint tip IO ()
  clientStNext utxo =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode (Block (BlockHeader _slotNo _hash blockNo) txs) BabbageEraInCardanoMode -> do
              forM_ txs $ \tx -> do
                case observeHeadTx networkId utxo tx of
                  NoHeadTx -> pure ()
                  Init RawInitObservation{headId} -> traceWith tracer $ HeadInitTx{headId = mkHeadId headId}
                  Commit RawCommitObservation{headId} -> traceWith tracer $ HeadCommitTx{headId}
                  CollectCom CollectComObservation{headId} -> traceWith tracer $ HeadCollectComTx{headId}
              let utxo' = utxo <> foldMap utxoFromTx txs
              pure $ clientStIdle utxo'
            _ -> pure $ clientStIdle utxo
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure $ clientStIdle utxo
      }
