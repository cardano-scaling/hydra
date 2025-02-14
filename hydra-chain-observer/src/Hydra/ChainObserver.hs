{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Data.Version (showVersion)
import Hydra.Cardano.Api (
  BlockHeader (BlockHeader),
  BlockInMode (..),
  BlockNo,
  CardanoEra (..),
  ChainPoint,
  ChainSyncClient,
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId (..),
  NetworkMagic (..),
  SocketPath,
  Tx,
  UTxO,
  connectToLocalNode,
  getChainPoint,
  getTxBody,
  getTxId,
  pattern Block,
 )
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Chain (OnChainTx (..))
import Hydra.Chain.CardanoClient (queryTip)
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.Chain.Direct.Tx (HeadObservation (..), observeHeadTx)
import Hydra.ChainObserver.Options (Backend (..), Options (..), hydraChainObserverOptions)
import Hydra.Contract (ScriptInfo)
import Hydra.Contract qualified as Contract
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Options (hydraNodeVersion)
import Hydra.Tx.HeadId (HeadId (..))
import Network.HTTP.Simple (getResponseBody, httpNoBody, parseRequestThrow, setRequestBodyJSON)
import Network.URI (URI)
import Options.Applicative (execParser)
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )

type ObserverHandler m = [ChainObservation] -> m ()

data ChainObservation = ChainObservation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , onChainTx :: Maybe (OnChainTx Tx)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ChainObservation where
  arbitrary = genericArbitrary

defaultObserverHandler :: Applicative m => ObserverHandler m
defaultObserverHandler = const $ pure ()

main :: IO ()
main = do
  Options{backend, startChainFrom, explorerBaseURI} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer KnownScripts{scriptInfo = Contract.scriptInfo}
    case backend of
      Direct{networkId, nodeSocket} -> do
        traceWith tracer ConnectingToNode{nodeSocket, networkId}
        chainPoint <- case startChainFrom of
          Nothing -> queryTip networkId nodeSocket
          Just x -> pure x
        traceWith tracer StartObservingFrom{chainPoint}

        let observerHandler observations =
              case explorerBaseURI of
                Nothing -> pure ()
                Just uri -> forM_ observations $ reportObservation networkId uri

        connectToLocalNode
          (connectInfo nodeSocket networkId)
          (clientProtocols tracer networkId chainPoint observerHandler)

-- | Submit observation to a 'hydra-explorer' at given base 'URI'.
reportObservation :: NetworkId -> URI -> ChainObservation -> IO ()
reportObservation networkId baseURI observation = do
  req <- parseRequestThrow url <&> setRequestBodyJSON observation
  httpNoBody req <&> getResponseBody
 where
  networkParam = case networkId of
    Mainnet -> "mainnet"
    (Testnet (NetworkMagic magic)) -> show magic

  version = showVersion hydraNodeVersion

  url = "POST " <> show baseURI <> "/observations/" <> networkParam <> "/" <> version

type ChainObserverLog :: Type
data ChainObserverLog
  = KnownScripts {scriptInfo :: ScriptInfo}
  | ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | StartObservingFrom {chainPoint :: ChainPoint}
  | HeadInitTx {headId :: HeadId}
  | HeadCommitTx {headId :: HeadId}
  | HeadCollectComTx {headId :: HeadId}
  | HeadDecrementTx {headId :: HeadId}
  | HeadCloseTx {headId :: HeadId}
  | HeadFanoutTx {headId :: HeadId}
  | HeadAbortTx {headId :: HeadId}
  | HeadContestTx {headId :: HeadId}
  | Rollback {point :: ChainPoint}
  | RollForward {point :: ChainPoint, receivedTxIds :: [TxId]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

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
  ObserverHandler IO ->
  LocalNodeClientProtocols BlockType ChainPoint ChainTip slot tx txid txerr query IO
clientProtocols tracer networkId startingPoint observerHandler =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId startingPoint observerHandler
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
  ObserverHandler m ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient tracer networkId startingPoint observerHandler =
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

              (utxo', observations) = observeAll networkId utxo txs
              onChainTxs = mapMaybe convertObservation observations

          forM_ onChainTxs (traceWith tracer . logOnChainTx)
          let observationsAt = ChainObservation point blockNo . Just <$> onChainTxs
          observerHandler $
            if null observationsAt
              then [ChainObservation point blockNo Nothing]
              else observationsAt

          pure $ clientStIdle utxo'
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure $ clientStIdle utxo
      }

  logOnChainTx :: OnChainTx Tx -> ChainObserverLog
  logOnChainTx = \case
    OnInitTx{headId} -> HeadInitTx{headId}
    OnCommitTx{headId} -> HeadCommitTx{headId}
    OnCollectComTx{headId} -> HeadCollectComTx{headId}
    OnDecrementTx{headId} -> HeadDecrementTx{headId}
    OnCloseTx{headId} -> HeadCloseTx{headId}
    OnFanoutTx{headId} -> HeadFanoutTx{headId}
    OnAbortTx{headId} -> HeadAbortTx{headId}
    OnContestTx{headId} -> HeadContestTx{headId}

observeTx :: NetworkId -> UTxO -> Tx -> (UTxO, Maybe HeadObservation)
observeTx networkId utxo tx =
  let utxo' = adjustUTxO tx utxo
   in case observeHeadTx networkId utxo tx of
        NoHeadTx -> (utxo, Nothing)
        observation -> (utxo', pure observation)

observeAll :: NetworkId -> UTxO -> [Tx] -> (UTxO, [HeadObservation])
observeAll networkId utxo txs =
  second reverse $ foldr go (utxo, []) txs
 where
  go :: Tx -> (UTxO, [HeadObservation]) -> (UTxO, [HeadObservation])
  go tx (utxo'', observations) =
    case observeTx networkId utxo'' tx of
      (utxo', Nothing) -> (utxo', observations)
      (utxo', Just observation) -> (utxo', observation : observations)
