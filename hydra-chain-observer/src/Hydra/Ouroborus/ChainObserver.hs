{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Ouroborus.ChainObserver where

import Hydra.Prelude

import Hydra.Cardano.Api (
  BlockHeader (BlockHeader),
  BlockInMode (..),
  CardanoEra (..),
  ChainPoint,
  ChainSyncClient,
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  Era,
  IsShelleyBasedEra (shelleyBasedEra),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  SocketPath,
  UTxO,
  connectToLocalNode,
  getChainPoint,
  getTxBody,
  getTxId,
  pattern Block,
 )
import Hydra.Chain.CardanoClient (queryTip, runCardanoNode)
import Hydra.ChainObserver.NodeClient (
  ChainObservation (..),
  ChainObserverLog (..),
  NodeClient (..),
  ObserverHandler,
  logObservation,
  observeAll,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Tx.Observe (HeadObservation (..))
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )

ouroborusClient ::
  Tracer IO ChainObserverLog ->
  SocketPath ->
  NetworkId ->
  NodeClient IO
ouroborusClient tracer nodeSocket networkId =
  NodeClient
    { follow = \startChainFrom observerHandler -> do
        traceWith tracer ConnectingToNode{nodeSocket, networkId}
        chainPoint <- case startChainFrom of
          Nothing -> runCardanoNode (connectInfo nodeSocket networkId) (shelleyBasedEra @Era) queryTip
          Just x -> pure x
        traceWith tracer StartObservingFrom{chainPoint}
        connectToLocalNode
          (connectInfo nodeSocket networkId)
          (clientProtocols tracer networkId (chainPoint :| []) observerHandler)
    , networkId
    }

type BlockType :: Type
type BlockType = BlockInMode

connectInfo :: SocketPath -> NetworkId -> LocalNodeConnectInfo
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
  NonEmpty ChainPoint ->
  ObserverHandler IO ->
  LocalNodeClientProtocols BlockType ChainPoint ChainTip slot tx txid txerr query IO
clientProtocols tracer networkId prefix observerHandler =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId prefix observerHandler
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

-- | Thrown when the user-provided custom points of intersection are unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on not-so-stable points of the chain. When they turn
-- the node back on, those points may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
type IntersectionNotFoundException :: Type
newtype IntersectionNotFoundException = IntersectionNotFound {requestedPoints :: NonEmpty ChainPoint}
  deriving stock (Show)

instance Exception IntersectionNotFoundException

-- | Fetch all blocks via chain sync and trace their contents.
chainSyncClient ::
  forall m.
  MonadThrow m =>
  Tracer m ChainObserverLog ->
  NetworkId ->
  NonEmpty ChainPoint ->
  ObserverHandler m ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient tracer networkId prefix observerHandler =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect (toList prefix) clientStIntersect
 where
  clientStIntersect :: ClientStIntersect BlockType ChainPoint ChainTip m ()
  clientStIntersect =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure $ clientStIdle mempty)
      , recvMsgIntersectNotFound = \_ ->
          ChainSyncClient $ throwIO (IntersectionNotFound prefix)
      }

  clientStIdle :: UTxO -> ClientStIdle BlockType ChainPoint ChainTip m ()
  clientStIdle utxo = SendMsgRequestNext (pure ()) (clientStNext utxo)

  clientStNext :: UTxO -> ClientStNext BlockType ChainPoint ChainTip m ()
  clientStNext utxo =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          let receivedTxIds = case blockInMode of
                BlockInMode ConwayEra (Block _ conwayTxs) -> getTxId . getTxBody <$> conwayTxs
                _ -> []

              (BlockInMode _ (Block bh@(BlockHeader _ _ blockNo) _)) = blockInMode
              point = getChainPoint bh
          traceWith tracer RollForward{point, receivedTxIds}

          let txs = case blockInMode of
                BlockInMode ConwayEra (Block _ conwayTxs) -> conwayTxs
                _ -> []

              (utxo', observations) = observeAll networkId utxo txs

          mapM_ (traceWith tracer) $ mapMaybe logObservation observations
          let observationsAt = ChainObservation point blockNo <$> observations
          observerHandler $
            if null observationsAt
              then [ChainObservation point blockNo NoHeadTx]
              else observationsAt

          pure $ clientStIdle utxo'
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure $ clientStIdle utxo
      }
