{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | A node which is able to handle multiple head instances.
--
-- The code is definitely not pretty and not worth of publication at the moment,
-- and it's very imperative too with lot of plumbing and wiring to connect the
-- various parts and fly messages around.
--
-- Conceptually, it's rather simple: When the multi-head node is requested to
-- start a new head, it spins up a singular `Hydra.Node` with the proper
-- configuration (cardano keys, hydra keys, network connections...). Then client
-- can interact with any of the open heads, including operating on the heads
-- like committing/closing/fanning out, etc.
--
-- For this to work, the multi-head node needs to do some multiplexing of the underlying nodes:
--
-- * The `MultiHeaded` node is configured with secret keys for both Hdyra and
--   Cardano, which will be used for all heads and all nodes. This works because
--   there's only one shared wallet so as long as the wallet has enough fuel to
--   pay for the transactions, it's ok
--
-- * It starts a `Network` server listening on a UDP port, which will multiplex
--   incoming messages and wrap outgoing messages annotated with the
--   `HeadId`. UDP seems like an interesting option there because it does not
--   require maintaining connections to remote nodes
-- * There's supposed to be some mechanism (_resolver_) to identify and locate
--   `Remote` peers. Currently these are just JSON-formatted files in a well-known
--   location but it's not far fetched to think of some other form of _directory_
--   for nodes, like DNS TXT entries, or even a hosted service
-- * It also starts a `ChainObserver` which is used to observe the chain for
--   `InitTx` which are of interest to our node
--
--     * This is currently done in a very ad hoc way as we distinguish the case
--       of the initiating node and the other cases
--     * There is a client interface which is JSON based, exposed through the
--       configured API port. It provides some commands on top of `ClientInput` and
--       some additional outputs over `ServerOutput`, but the client is notified of all
--       the events occuring in anyone of the started node, annotated with the `HeadId`
--     * Starting a new head is triggered either by a client command or by
--       observing an `InitTx` on-chain
--     * This triggers starting a new `Hydra.Node` with a complete stack of
--       services tailored for the particular situation
--     * Each node has its own `Direct` chain connection which is started from
--       the point _before_ the `InitTx` so that the tx can be observed and the state
--       updated accordingly. This is annoying to do but is needed becuase while it's
--       easy to know the `ChainPoint` for a tx, it's not possible to retrieve an
--       arbitrary `Block` from the node given such a `ChainPoint`: One has to _follow_
--       the chain and only gets the `next` blocks
--     * The `API` part is rewired to connect to the enclosing multi-head node's
--       client interface
--     * The `Network` part encapsulates a UDP component and a
--       `Hydra.Network.MultiHead` component that annotates each message with the
--       `HeadId`
--     * The new node is passed a list of `Remote` representing the other parties
--       of hte head with all the required information (host:port, vkeys). This list is
--       constructed by resolving "symbolic" name (eg. read some files)
--     * Note that in the case of starting a node from observing a Head, the
--       only identifier we have is the `Party`, which is a `VerificationKey
--       HydraKey`. We need to lookup through all the known nodes to resolve that key
--       to the `Remote` structure which is probably something that would be cumbersome
--       IRL. It would probably make sense to add some symbolic name in the datum of
--       the `Initial` UTxO created for the party, so that all parties can more easily
--       infer other parties' information from the chain
module Hydra.Node.MultiHeaded where

import Control.Concurrent.STM (dupTChan)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad.Class.MonadAsync (Async, async, cancel, link)
import Control.Monad.Class.MonadSTM (modifyTVar', newTQueueIO, newTVarIO, readTQueue, readTVarIO, writeTQueue, writeTVar)
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
import Hydra.Chain.Direct.HeadObserver (ChainEvent (..), runChainObserver)
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
import Hydra.Options (ChainConfig (..), RunOptions (..), cardanoLedgerGenesisFile, cardanoLedgerProtocolParametersFile, cardanoVerificationKeys)
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

withNode :: RunOptions -> (TQueue IO Command -> TQueue IO Result -> IO ()) -> IO ()
withNode opts action = do
  commandQueue <- newTQueueIO
  resultQueue <- newTQueueIO
  race_
    (runNode opts commandQueue resultQueue)
    (action commandQueue resultQueue)

runNode :: RunOptions -> TQueue IO Command -> TQueue IO Result -> IO ()
runNode opts cmdQueue resQueue = do
  let RunOptions{verbosity, monitoringPort, nodeId, host, port} = opts
  env@Environment{party} <- initEnvironment opts
  withTracer verbosity $ \tracer' -> do
    withMonitoring monitoringPort tracer' $ \tracer -> do
      traceWith tracer (NodeOptions opts)
      let RunOptions{hydraScriptsTxId, chainConfig} = opts
      -- create shared wallet
      wallet <- mkTinyWallet (contramap DirectChain tracer) chainConfig
      -- create heads heads state
      heads <- newTVarIO mempty
      -- create chain state
      chainPoints <- newTVarIO mempty
      -- start chain observer to be notified when a new head is initialised
      withAsync (runChainObserver (contramap DirectChain tracer) chainConfig Nothing (onChainEvent chainPoints)) $ \_ -> do
        -- create UDP server
        -- We only create the server here and not the full NetworkComponent because
        -- the server is shared between all head instances we will create
        bchan <- newBroadcastTChanIO
        withAsync (udpServer (contramap Network tracer') (Host (show host) port) (udpCallback bchan)) $ \_ -> do
          let startNode hid chainPoint remotes inputQueue outputQueue = do
                -- TODO: should be already initialised with head state?
                nodeState <- createNodeState IdleState{chainState = initialChainState}
                let chainConfig' =
                      chainConfig
                        { cardanoVerificationKeys = cardanoVerificationKey <$> remotes
                        , startChainFrom = chainPoint
                        }
                chainContext <- loadChainContext chainConfig' party hydraScriptsTxId
                eq <- createEventQueue
                chan <- atomically $ dupTChan bchan
                otherParties <- mapM (loadParty . hydraVerificationKey) remotes
                let env' = env{otherParties}
                withDirectChain (contramap DirectChain tracer) chainConfig' chainContext chainPoint wallet (chainCallback nodeState eq) $ \chain -> do
                  withMultiHead hid (withOneNodeUDP chan remotes) (putEvent eq . NetworkEvent defaultTTL) $ \hn -> do
                    withLocalAPIServer hid resQueue inputQueue outputQueue (putEvent eq . ClientEvent) $ \server -> do
                      let RunOptions{ledgerConfig} = opts
                      withCardanoLedger ledgerConfig $ \ledger ->
                        runHydraNode (contramap Node tracer) $
                          HydraNode{eq, hn, nodeState, oc = chain, server, ledger, env = env', persistence = noPersistence}
          runMultiHeadedNode nodeId party chainPoints heads startNode cmdQueue resQueue
 where
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

  onChainEvent :: TVar IO [ChainPoint] -> ChainEvent -> IO ()
  onChainEvent chain = \case
    (HeadInit hio@HeadInitObservation{headInitChainPoint}) -> atomically $ do
      modifyTVar' chain $ forwardPoint headInitChainPoint
      writeTQueue cmdQueue (ObservingHeadInit hio)
    (Forward{point}) -> atomically $ modifyTVar' chain $ forwardPoint (Just point)
    (Backward{point}) -> atomically $ modifyTVar' chain $ rollbackPoint point

rollbackPoint :: ChainPoint -> [ChainPoint] -> [ChainPoint]
rollbackPoint pt [] = [pt]
rollbackPoint pt (p : ps)
  | pt == p = p : ps
  | otherwise = rollbackPoint pt ps

forwardPoint :: Maybe ChainPoint -> [ChainPoint] -> [ChainPoint]
forwardPoint Nothing points = points
forwardPoint (Just pt) [] = [pt]
forwardPoint (Just pt) (p : ps)
  | p /= pt = pt : p : ps
  | otherwise = p : ps

previousPoint :: Maybe ChainPoint -> TVar IO [ChainPoint] -> STM IO (Maybe ChainPoint)
previousPoint Nothing _ = pure Nothing
previousPoint (Just pt) chain = do
  readTVar chain >>= pure . findPrevious pt

findPrevious :: ChainPoint -> [ChainPoint] -> Maybe ChainPoint
findPrevious pt = \case
  (p : p' : ps)
    | p == pt -> Just p'
    | otherwise -> findPrevious pt (p' : ps)
  [_] -> Nothing
  [] -> Nothing

loadParty :: FilePath -> IO Party
loadParty p =
  Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p

withOneNodeUDP :: ToCBOR msg => TChan (Enveloped msg) -> [Remote] -> NetworkComponent IO (Enveloped msg) ()
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
  NodeId ->
  Party ->
  -- | STate of chain
  TVar IO [ChainPoint] ->
  -- | State of started nodes
  TVar IO (Map HeadId MultiHeadState) ->
  -- | How to start a new node
  (TVar IO (Maybe HeadId) -> Maybe ChainPoint -> [Remote] -> TQueue IO (ClientInput Tx) -> TQueue IO (ServerOutput Tx) -> IO ()) ->
  -- | Command interface
  -- This is used to communicate with the "client" , enacting commands and passing
  -- results.
  TQueue IO Command ->
  TQueue IO Result ->
  IO ()
runMultiHeadedNode nodeId party chain heads startNode cmdQueue resQueue = forever $ do
  atomically (readTQueue cmdQueue) >>= \cmd ->
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
          writeTQueue resQueue (HeadStarted headId)
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
              remotes <- filter ((/= nodeId) . remoteId) <$> mapM lookupRemoteByParty parties
              -- FIXME: this is a PITA, we need to keep track of the chain in order to retrieve
              -- the block /before/ the head tx block so that the underlying node can sync up
              -- properly. This is so becauase the chain sync protocol's intersect message starts
              -- following the chain /after/ the given point and not /at/ the given point
              startPoint <- atomically $ previousPoint headInitChainPoint chain
              newNodeState@MultiHeadState{headIdVar} <- startNewNode remotes startPoint
              atomically $ do
                modifyTVar' heads $ \headsMap -> Map.insert headId newNodeState headsMap
                writeTQueue resQueue (HeadStarted headId)
              pure headIdVar
            Just MultiHeadState{headIdVar} -> pure headIdVar
          atomically $ writeTVar headIdVar (Just headId)
        | otherwise -> pure ()
      HeadInput{headId, clientInput} ->
        atomically $ do
          multiHeadState <- Map.lookup headId <$> readTVar heads
          res <- case multiHeadState of
            Nothing -> pure $ NoSuchHead headId
            Just MultiHeadState{inputQueue} -> writeTQueue inputQueue clientInput >> pure (InputSent headId)
          writeTQueue resQueue res
      StopHead hid -> do
        multiHeadState <- Map.lookup hid <$> readTVarIO heads
        case multiHeadState of
          Nothing -> atomically $ writeTQueue resQueue (NoSuchHead hid)
          Just MultiHeadState{node} -> do
            cancel node
            atomically $ writeTQueue resQueue (HeadStopped hid)
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

withLocalAPIServer ::
  TVar IO (Maybe HeadId) ->
  TQueue IO Result ->
  TQueue IO (ClientInput Tx) ->
  TQueue IO (ServerOutput Tx) ->
  (ClientInput Tx -> IO ()) ->
  (Server Tx IO -> IO ()) ->
  IO ()
withLocalAPIServer hid resq inq outq commands k =
  withAsync dispatchResult $ \_ ->
    withAsync (forever $ atomically (readTQueue inq) >>= commands) $ \_ ->
      k $ Server{sendOutput = atomically . writeTQueue outq}
 where
  dispatchResult = forever $
    atomically $ do
      readTVar hid >>= \case
        Nothing -> pure ()
        Just headId -> readTQueue outq >>= writeTQueue resq . HeadOutput headId

noPersistence :: Persistence (HeadState Tx) IO
noPersistence =
  Persistence
    { save = const $ pure ()
    , load = pure Nothing
    }

-- * Interactive Server
runServer :: PortNumber -> TQueue IO Command -> TQueue IO Result -> IO ()
runServer port inq outq = do
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
    client <- async $ interpretCommands h inq outq
    link client

interpretCommands :: Handle -> TQueue IO Command -> TQueue IO Result -> IO ()
interpretCommands h inq outq =
  race_ takeCommands putResults
 where
  takeCommands = forever $ do
    cmd <- either (\e -> error $ "fail to decode command " <> show e) id . eitherDecodeStrict <$> hGetLine h
    atomically $ writeTQueue inq cmd

  putResults = forever $ do
    res <- atomically $ readTQueue outq
    hPutStr h (LBS.toStrict (encode res <> "\n"))
