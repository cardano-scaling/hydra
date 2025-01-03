{-# LANGUAGE DuplicateRecordFields #-}

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
import Hydra.Chain.CardanoClient (queryTip)
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.ChainObserver.NodeClient (
  ChainObservation (..),
  ChainObserverLog (..),
  NodeClient (..),
  ObserverHandler,
  logOnChainTx,
  observeAll,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.SerialisedScriptRegistry (SerialisedScriptRegistry)
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
  SerialisedScriptRegistry ->
  NodeClient IO
ouroborusClient tracer nodeSocket networkId serialisedScriptRegistry =
  NodeClient
    { follow = \startChainFrom observerHandler -> do
        traceWith tracer ConnectingToNode{nodeSocket, networkId}
        chainPoint <- case startChainFrom of
          Nothing -> queryTip networkId nodeSocket
          Just x -> pure x
        traceWith tracer StartObservingFrom{chainPoint}
        connectToLocalNode
          (connectInfo nodeSocket networkId)
          (clientProtocols tracer networkId chainPoint serialisedScriptRegistry observerHandler)
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
  ChainPoint ->
  SerialisedScriptRegistry ->
  ObserverHandler IO ->
  LocalNodeClientProtocols BlockType ChainPoint ChainTip slot tx txid txerr query IO
clientProtocols tracer networkId startingPoint serialisedScriptRegistry observerHandler =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId startingPoint serialisedScriptRegistry observerHandler
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

-- | Thrown when the user-provided custom point of intersection is unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on a not-so-stable point of the chain. When they turn
-- the node back on, that point may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
type IntersectionNotFoundException :: Type
newtype IntersectionNotFoundException = IntersectionNotFound {requestedPoint :: ChainPoint}
  deriving stock (Show)

instance Exception IntersectionNotFoundException

-- | Fetch all blocks via chain sync and trace their contents.
chainSyncClient ::
  forall m.
  MonadThrow m =>
  Tracer m ChainObserverLog ->
  NetworkId ->
  ChainPoint ->
  SerialisedScriptRegistry ->
  ObserverHandler m ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient tracer networkId startingPoint serialisedScriptRegistry observerHandler =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect [startingPoint] clientStIntersect
 where
  clientStIntersect :: ClientStIntersect BlockType ChainPoint ChainTip m ()
  clientStIntersect =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure $ clientStIdle mempty)
      , recvMsgIntersectNotFound = \_ ->
          ChainSyncClient $ throwIO (IntersectionNotFound startingPoint)
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

              (utxo', observations) = observeAll networkId serialisedScriptRegistry utxo txs
              onChainTxs = mapMaybe convertObservation observations

          forM_ onChainTxs (traceWith tracer . logOnChainTx)
          let observationsAt = HeadObservation point blockNo <$> onChainTxs
          observerHandler $
            if null observationsAt
              then [Tick point blockNo]
              else observationsAt

          pure $ clientStIdle utxo'
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure $ clientStIdle utxo
      }
