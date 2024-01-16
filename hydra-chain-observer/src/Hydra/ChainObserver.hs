{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Control.Concurrent (forkFinally)
import Control.Exception ()
import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Hydra.Cardano.Api (
  Block (..),
  BlockInMode (..),
  CardanoMode,
  ChainPoint,
  ChainSyncClient,
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  EraInMode (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  SocketPath,
  Tx,
  UTxO,
  chainTipToChainPoint,
  connectToLocalNode,
  getTxBody,
  getTxId,
 )
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Chain.CardanoClient (queryTip)
import Hydra.Chain.Direct.Tx (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  InitObservation (..),
  observeHeadTx,
 )
import Hydra.ChainObserver.Options (Options (..), hydraChainObserverOptions)
import Hydra.Contract (ScriptInfo)
import Hydra.Contract qualified as Contract
import Hydra.HeadId (HeadId (..))
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Network (Host (..), PortNumber)
import Hydra.Node.EventQueue (EventQueue (..), Queued (..), createEventQueue)
import Network.Socket (
  AddrInfo (..),
  SocketType (..),
  accept,
  bind,
  close,
  defaultHints,
  defaultProtocol,
  getAddrInfo,
  listen,
  socket,
  socketToHandle,
  withSocketsDo,
 )
import Options.Applicative (execParser)
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )
import System.IO (hClose, hPutStrLn)

runIPCServer :: Tracer IO ChainObserverLog -> Host -> EventQueue IO HeadObservation -> IO ()
runIPCServer tracer host@Host{hostname, port} eq = withSocketsDo $ do
  bracket
    openTCPListener
    close
    ( \sock -> do
        traceWith tracer (ServerStarted port)
        forever $ do
          (conn, _) <- accept sock
          -- XXX: Fork a new thread to handle the new accepted connection
          forkFinally
            (handleClient conn)
            ( \_ -> do
                close conn
                close sock
            )
    )
 where
  openTCPListener = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
    sockAddr <- case is of
      (inf : _) -> pure inf
      _ -> die "getAdrrInfo failed"
    sock <- socket (addrFamily sockAddr) Stream defaultProtocol
    traceWith tracer $ ConnectingTo host
    bind sock (addrAddress sockAddr)
    traceWith tracer $ ConnectedTo host
    listen sock 5
    return sock

  handleClient conn = do
    hdl <- socketToHandle conn ReadWriteMode
    hSetBuffering hdl LineBuffering
    traceWith tracer NewClientConnection
    pushObservation hdl `finally` hClose hdl

  pushObservation hdl = forever $ do
    Queued{queuedEvent} <- nextEvent eq
    hPutStrLn hdl (prettyJSONString $ toJSON queuedEvent)

  prettyJSONString = Text.unpack . Text.strip . decodeUtf8 . Aeson.encode

main :: IO ()
main = do
  Options{networkId, nodeSocket, host, port, startChainFrom} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer KnownScripts{scriptInfo = Contract.scriptInfo}
    traceWith tracer ConnectingToNode{nodeSocket, networkId}
    chainPoint <- case startChainFrom of
      Nothing -> queryTip networkId nodeSocket
      Just x -> pure x
    traceWith tracer StartObservingFrom{chainPoint}
    eq@EventQueue{putEvent} <- createEventQueue
    race
      ( runIPCServer tracer Host{hostname = show host, port} eq
          `catch` \(e :: SomeException) -> die $ displayException e
      )
      ( connectToLocalNode
          (connectInfo nodeSocket networkId)
          (clientProtocols tracer networkId chainPoint putEvent)
      )
      >>= \case
        Left{} -> error "Something went wrong: "
        Right a -> pure a

type ChainObserverLog :: Type
data ChainObserverLog
  = KnownScripts {scriptInfo :: ScriptInfo}
  | ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | StartObservingFrom {chainPoint :: ChainPoint}
  | HeadInitTx {headId :: HeadId}
  | HeadCommitTx {headId :: HeadId}
  | HeadCollectComTx {headId :: HeadId}
  | HeadCloseTx {headId :: HeadId}
  | HeadFanoutTx {headId :: HeadId}
  | HeadAbortTx {headId :: HeadId}
  | HeadContestTx {headId :: HeadId}
  | Rollback {point :: ChainPoint}
  | RollForward {point :: ChainPoint, receivedTxIds :: [TxId]}
  | ServerStarted {listeningPort :: PortNumber}
  | -- FIXME: export Log module from hydra-net and use AddrInfo instead of Host
    ConnectingTo {address :: Host}
  | ConnectedTo {address :: Host}
  | NewClientConnection
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type BlockType :: Type
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
  ChainPoint ->
  (HeadObservation -> IO ()) ->
  LocalNodeClientProtocols BlockType ChainPoint ChainTip slot tx txid txerr query IO
clientProtocols tracer networkId startingPoint observerHandle =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId startingPoint observerHandle
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
  (HeadObservation -> m ()) ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient tracer networkId startingPoint observerHandle =
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
  clientStIdle utxo = SendMsgRequestNext (clientStNext utxo) (pure $ clientStNext utxo)

  clientStNext :: UTxO -> ClientStNext BlockType ChainPoint ChainTip m ()
  clientStNext utxo =
    ClientStNext
      { recvMsgRollForward = \blockInMode tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode _ (Block _header txs) BabbageEraInCardanoMode -> do
              let point = chainTipToChainPoint tip
              let receivedTxIds = getTxId . getTxBody <$> txs
              traceWith tracer RollForward{point, receivedTxIds}
              let (utxo', observations) = observeAll networkId utxo txs
              -- FIXME we should be exposing OnChainTx instead of working around NoHeadTx.
              forM_ observations $ \observation -> do
                maybe (pure ()) (traceWith tracer) . logObservation $ observation
                observerHandle observation
              pure $ clientStIdle utxo'
            _ -> pure $ clientStIdle utxo
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure $ clientStIdle utxo
      }

logObservation ::
  HeadObservation ->
  Maybe ChainObserverLog
logObservation = \case
  NoHeadTx -> Nothing
  Init InitObservation{headId} -> pure $ HeadInitTx{headId}
  Commit CommitObservation{headId} -> pure $ HeadCommitTx{headId}
  CollectCom CollectComObservation{headId} -> pure $ HeadCollectComTx{headId}
  Close CloseObservation{headId} -> pure $ HeadCloseTx{headId}
  Fanout FanoutObservation{headId} -> pure $ HeadFanoutTx{headId}
  Abort AbortObservation{headId} -> pure $ HeadAbortTx{headId}
  Contest ContestObservation{headId} -> pure $ HeadContestTx{headId}

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
