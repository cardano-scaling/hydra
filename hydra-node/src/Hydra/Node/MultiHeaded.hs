{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | A node which is able to handle multiple head instances.
--
-- This "node" uses the existing `Hydra.Node` as a basic building block, providing
-- specific implementations of `Network` and `Chain` components to allow multiple
-- instances to coexist.
--
-- * All nodes share a single `TinyWallet` instance which is initialised upon startup,
--   with credentials that will also be shared among all heads.
-- * There is a single UDP port on which the server listens. All messages are dispatched
--   to the corresponding nodes according to peers' coordinates as provided by some
--   name resolution mechanism
module Hydra.Node.MultiHeaded where

import Control.Concurrent.STM (dupTChan)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad.Class.MonadAsync (Async, async, cancel, link)
import Control.Monad.Class.MonadSTM (modifyTVar', newEmptyTMVar, newTQueueIO, newTVarIO, putTMVar, readTQueue, readTVarIO, takeTMVar, writeTQueue, writeTVar)
import Data.Aeson (decodeFileStrict, eitherDecodeStrict, encode)
import Data.ByteString (hGetLine, hPutStr)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (unpack)
import Hydra.API.ClientInput (ClientInput (Init))
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (HeadInitialized))
import Hydra.Cardano.Api (AsType (AsVerificationKey), ChainPoint, Tx)
import Hydra.Chain (HeadId)
import Hydra.Chain.Direct (initialChainState, loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Chain.Direct.HeadObserver (runChainObserver)
import Hydra.Chain.Direct.Tx (HeadInitObservation (..))
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (Environment (..), Event (ClientEvent, NetworkEvent), HeadState (..), defaultTTL)
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Ledger.Cardano.Configuration (newGlobals, newLedgerEnv, protocolParametersFromJson, readJsonFileThrow, shelleyGenesisFromJson)
import Hydra.Logging (traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Network, Node, NodeOptions))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (Host), NetworkComponent, NodeId)
import Hydra.Network.Message (Message)
import Hydra.Network.MultiHead (Enveloped (..), withMultiHead)
import Hydra.Network.UDP (PeersResolver, udpClient, udpServer)
import Hydra.Node (HydraNode (..), chainCallback, createEventQueue, createNodeState, initEnvironment, putEvent, runHydraNode)
import Hydra.Options (RunOptions (..), cardanoLedgerGenesisFile, cardanoLedgerProtocolParametersFile, cardanoVerificationKeys)
import Hydra.Party (Party (..))
import Hydra.Persistence (Persistence (..))
import Hydra.Prelude
import Network.Socket (
  AddrInfo (addrSocketType),
  AddrInfoFlag (AI_PASSIVE),
  Family (AF_INET),
  PortNumber,
  SocketOption (ReuseAddr),
  SocketType (Stream),
  accept,
  addrAddress,
  addrFamily,
  addrFlags,
  bind,
  defaultHints,
  defaultProtocol,
  getAddrInfo,
  listen,
  setSocketOption,
  socket,
  socketToHandle,
 )
import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath (takeExtension, (<.>))

data Remote = Remote
  { remoteId :: NodeId
  , cardanoVerificationKey :: FilePath
  , hydraVerificationKey :: FilePath
  , address :: Host
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Command
  = StartHead {peers :: [Text]} -- list parties
  | HeadInput {headId :: HeadId, clientInput :: ClientInput Tx}
  | StopHead {headId :: HeadId}
  | -- |FIXME: not a command, refactor this
    ObservingHeadInit HeadInitObservation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Result
  = HeadStarted {headId :: HeadId}
  | HeadOutput {headId :: HeadId, serverOutput :: ServerOutput Tx}
  | InputSent {headId :: HeadId}
  | HeadStopped {headId :: HeadId}
  | NoSuchHead {headId :: HeadId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withNode :: RunOptions -> (TQueue IO (Command, TMVar IO Result) -> IO ()) -> IO ()
withNode opts action = do
  commandQueue <- newTQueueIO
  race_
    (runNode opts commandQueue)
    (action commandQueue)

runNode :: RunOptions -> TQueue IO (Command, TMVar IO Result) -> IO ()
runNode opts cmdQueue = do
  let RunOptions{verbosity, monitoringPort, host, port} = opts
  env@Environment{party} <- initEnvironment opts
  withTracer verbosity $ \tracer' -> do
    withMonitoring monitoringPort tracer' $ \tracer -> do
      traceWith tracer (NodeOptions opts)
      let RunOptions{hydraScriptsTxId, chainConfig} = opts
      -- create shared wallet
      wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
      -- create heads heads state
      heads <- newTVarIO mempty
      -- start chain observer to be notified when a new head is initialised
      withAsync (runChainObserver (contramap DirectChain tracer) chainConfig Nothing onHeadInit) $ \_ -> do
        -- create UDP server
        -- We only create the server here and not the full NetworkComponent because
        -- the server is shared between all head instances we will create
        bchan <- newBroadcastTChanIO
        withAsync (udpServer (contramap Network tracer') (Host (show host) port) (udpCallback bchan)) $ \_ -> do
          let startNode hid chainPoint remotes inputQueue outputQueue = do
                -- TODO: should be already initialised with head state?
                nodeState <- createNodeState IdleState{chainState = initialChainState}
                let chainConfig' = chainConfig{cardanoVerificationKeys = cardanoVerificationKey <$> remotes}
                chainContext <- loadChainContext chainConfig' party hydraScriptsTxId
                eq <- createEventQueue
                chan <- atomically $ dupTChan bchan
                otherParties <- mapM (loadParty . hydraVerificationKey) remotes
                let env' = env{otherParties}
                withDirectChain (contramap DirectChain tracer) chainConfig' chainContext chainPoint wallet (chainCallback nodeState eq) $ \chain -> do
                  withMultiHead hid (withOneNodeUDP chan remotes) (putEvent eq . NetworkEvent defaultTTL) $ \hn -> do
                    withLocalAPIServer inputQueue outputQueue (putEvent eq . ClientEvent) $ \server -> do
                      let RunOptions{ledgerConfig} = opts
                      withCardanoLedger ledgerConfig $ \ledger ->
                        runHydraNode (contramap Node tracer) $
                          HydraNode{eq, hn, nodeState, oc = chain, server, ledger, env = env', persistence = noPersistence}
          runMultiHeadedNode party heads startNode cmdQueue
 where
  onHeadInit i = atomically $ do
    resp <- newEmptyTMVar
    writeTQueue cmdQueue (ObservingHeadInit i, resp)

  withCardanoLedger ledgerConfig action = do
    globals <-
      newGlobals
        <$> readJsonFileThrow shelleyGenesisFromJson (cardanoLedgerGenesisFile ledgerConfig)

    ledgerEnv <-
      newLedgerEnv
        <$> readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)

    action (Ledger.cardanoLedger globals ledgerEnv)

  --  udpCallback :: NetworkCallback (Enveloped (Message Tx)) IO
  udpCallback chan = atomically . writeTChan chan

loadParty :: FilePath -> IO Party
loadParty p =
  Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p

withOneNodeUDP :: TChan (Enveloped (Message Tx)) -> [Remote] -> NetworkComponent IO (Enveloped (Message Tx)) ()
withOneNodeUDP chan remotes callback k =
  withAsync dispatch $ \_ ->
    k $ udpClient (const $ pure $ address <$> remotes)
 where
  dispatch = forever $ atomically (readTChan chan) >>= callback

resolvePeers :: TVar IO (Map HeadId MultiHeadState) -> PeersResolver IO (Enveloped (Message Tx))
resolvePeers heads Enveloped{headId} =
  maybe [] ((fmap address) . remotes) . Map.lookup headId <$> readTVarIO heads

data MultiHeadState = MultiHeadState
  { remotes :: [Remote]
  , node :: Async IO ()
  , headIdVar :: TVar IO (Maybe HeadId)
  , inputQueue :: TQueue IO (ClientInput Tx)
  , outputQueue :: TQueue IO (ServerOutput Tx)
  }

runMultiHeadedNode ::
  Party ->
  -- | State of started nodes
  TVar IO (Map HeadId MultiHeadState) ->
  -- | How to start a new node
  (TVar IO (Maybe HeadId) -> Maybe ChainPoint -> [Remote] -> TQueue IO (ClientInput Tx) -> TQueue IO (ServerOutput Tx) -> IO ()) ->
  -- | Command interface
  -- This is used to communicate with the "client" , enacting commands and passing
  -- results.
  TQueue IO (Command, TMVar IO Result) ->
  IO ()
runMultiHeadedNode party heads startNode cmdQueue = forever $ do
  atomically (readTQueue cmdQueue) >>= \(cmd, result) ->
    case cmd of
      StartHead peers -> do
        remotes <- mapM lookupRemoteByName peers
        newNodeState@MultiHeadState{inputQueue, outputQueue} <- startNewNode remotes Nothing
        -- trigger initialisation of head and synchronously wait for it
        -- We need the headId to properly update state for this node
        atomically $ writeTQueue inputQueue Init
        headId <- waitForInit outputQueue
        atomically $ do
          modifyTVar' heads $ \headsMap -> Map.insert headId newNodeState headsMap
          putTMVar result (HeadStarted headId)
      ObservingHeadInit HeadInitObservation{headId, headInitChainPoint, parties}
        | party `elem` parties -> do
          -- FIXME: This is lame, it's there because there's a race condition between the moment one
          -- starts a new node for a new head and the moment we know the head id and can thus register
          -- the node with the head id.
          -- What we should do instead is trigger the head initialisation independently of the node
          -- creation and only create the node once we observe the init tx back from the chain
          threadDelay 2
          -- Is there a head already started with this id?
          multiHeadState <- Map.lookup headId <$> readTVarIO heads
          headIdVar <- case multiHeadState of
            Nothing -> do
              remotes <- mapM lookupRemoteByParty parties
              newNodeState@MultiHeadState{headIdVar} <- startNewNode remotes headInitChainPoint
              atomically $ do
                modifyTVar' heads $ \headsMap -> Map.insert headId newNodeState headsMap
              pure headIdVar
            Just MultiHeadState{headIdVar} -> pure headIdVar
          atomically $ writeTVar headIdVar (Just headId)
        | otherwise -> pure ()
      HeadInput{headId, clientInput} -> do
        res <- atomically $ do
          multiHeadState <- Map.lookup headId <$> readTVar heads
          case multiHeadState of
            Nothing -> pure $ NoSuchHead headId
            Just MultiHeadState{inputQueue} -> writeTQueue inputQueue clientInput >> pure (InputSent headId)
        atomically $ putTMVar result res
      StopHead hid -> do
        multiHeadState <- Map.lookup hid <$> readTVarIO heads
        case multiHeadState of
          Nothing -> atomically $ putTMVar result (NoSuchHead hid)
          Just MultiHeadState{node} -> do
            cancel node
            atomically $ putTMVar result (HeadStopped hid)
 where
  startNewNode remotes chainPoint = do
    headIdVar <- newTVarIO Nothing
    inputQueue <- newTQueueIO
    outputQueue <- newTQueueIO
    node <- async $ startNode headIdVar chainPoint remotes inputQueue outputQueue
    pure $ MultiHeadState{remotes, headIdVar, node, inputQueue, outputQueue}

waitForInit :: TQueue IO (ServerOutput Tx) -> IO HeadId
waitForInit queue = do
  out <- atomically $ do
    res <- readTQueue queue
    case res of
      HeadInitialized headId -> pure $ Just headId
      _ -> pure Nothing
  maybe (waitForInit queue) pure out

-- | Find a remote by its name.
-- By convention we look into the working directory of the current process for a
-- file named `XXX.peer`
-- FIXME: provide a better name resolution mechanism
lookupRemoteByName :: Text -> IO Remote
lookupRemoteByName p =
  fromMaybe (error $ "cannot find peer file for " <> p) <$> decodeFileStrict (unpack p <.> "peer")

-- | Find a remote by `Party`.
--
-- FIXME: provide a better name resolution mechanism
lookupRemoteByParty :: Party -> IO Remote
lookupRemoteByParty p = do
  peerFiles <- filter ((== ".peer") . takeExtension) <$> (getCurrentDirectory >>= listDirectory)
  remotes <- catMaybes <$> mapM decodeFileStrict peerFiles
  fromMaybe (error $ "cannot find remote for party:  " <> show p) <$> lookupParty remotes
 where
  lookupParty :: [Remote] -> IO (Maybe Remote)
  lookupParty remotes = do
    parties <- mapM (\r -> loadParty (hydraVerificationKey r) >>= pure . (,r)) remotes
    pure $ List.lookup p parties

withLocalAPIServer :: TQueue IO (ClientInput Tx) -> TQueue IO (ServerOutput Tx) -> (ClientInput Tx -> IO ()) -> (Server Tx IO -> IO ()) -> IO ()
withLocalAPIServer inq outq commands k =
  withAsync (forever $ atomically (readTQueue inq) >>= commands) $ \_ ->
    k $ Server{sendOutput = atomically . writeTQueue outq}

noPersistence :: Persistence (HeadState Tx) IO
noPersistence =
  Persistence
    { save = const $ pure ()
    , load = pure Nothing
    }

-- * Interactive Server
runServer :: PortNumber -> TQueue IO (Command, TMVar IO Result) -> IO ()
runServer port queue = do
  print @Text $ "Starting command server on " <> show port
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  let hints =
        defaultHints
          { addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          , addrFamily = AF_INET
          }
  addr : _ <- getAddrInfo (Just hints) Nothing (Just $ show port)
  bind sock (addrAddress addr)
  listen sock 5
  forever $ do
    print @Text $ "Accepting connections on port " <> show port
    (clientSock, _) <- accept sock
    h <- socketToHandle clientSock ReadWriteMode
    hSetBuffering h NoBuffering
    client <- async $ interpretCommands h queue
    link client

interpretCommands :: Handle -> TQueue IO (Command, TMVar IO Result) -> IO ()
interpretCommands h queue = forever $ do
  cmd <- either (\e -> error $ "fail to decode command " <> show e) id . eitherDecodeStrict <$> hGetLine h
  res <- atomically $ do
    res <- newEmptyTMVar
    writeTQueue queue (cmd, res)
    pure res
  result <- atomically $ takeTMVar res
  hPutStr h (LBS.toStrict (encode result <> "\n"))
