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

import Control.Monad.Class.MonadAsync (async, link)
import Control.Monad.Class.MonadSTM (newEmptyTMVar, newTQueueIO, takeTMVar, writeTQueue)
import Data.ByteString (hGetLine, hPutStr)
import Data.Text (unpack)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Server (Server)
import Hydra.Cardano.Api (Tx, VerificationKey)
import Hydra.Chain (HeadId)
import Hydra.Chain.Direct (initialChainState, loadChainContext, mkTinyWallet, withDirectChain)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (Environment (..), Event (ClientEvent, NetworkEvent), HeadState (..), defaultTTL)
import qualified Hydra.Ledger.Cardano as Ledger
import Hydra.Ledger.Cardano.Configuration (newGlobals, newLedgerEnv, protocolParametersFromJson, readJsonFileThrow, shelleyGenesisFromJson)
import Hydra.Logging (traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Network, Node, NodeOptions))
import Hydra.Logging.Monitoring (withMonitoring)
import Hydra.Network (Host (Host), NetworkCallback)
import Hydra.Network.Message (Message)
import Hydra.Network.MultiHead (Enveloped, withMultiHead)
import Hydra.Network.UDP (withUDPNetwork)
import Hydra.Node (HydraNode (..), chainCallback, createEventQueue, createNodeState, initEnvironment, putEvent, runHydraNode)
import Hydra.Options (RunOptions (..), cardanoLedgerGenesisFile, cardanoLedgerProtocolParametersFile, cardanoVerificationKeys)
import Hydra.Persistence (Persistence)
import Hydra.Prelude
import Network.Socket (AddrInfo (addrSocketType), AddrInfoFlag (AI_PASSIVE), Family (AF_INET), PortNumber, SocketOption (ReuseAddr), SocketType (Stream), accept, addrAddress, addrFamily, addrFlags, bind, defaultHints, defaultProtocol, getAddrInfo, listen, setSocketOption, socket, socketToHandle)
import Prelude (read)

data Remote = Remote
  { cardanoVerificationKey :: FilePath
  , hydraVerificationKey :: VerificationKey HydraKey
  }

data Command
  = StartHead
  | StopHead Text
  deriving stock (Eq, Show, Read)

data Result = HeadStarted HeadId
  deriving stock (Eq, Show)

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
      -- create UDP network
      let resolver = error "TODO: define resolver"
      let udp = withUDPNetwork (contramap Network tracer) (Host (show host) port) resolver
      let startNode net hid remotes = do
            -- TODO: should be already initialised with head state
            nodeState <- createNodeState IdleState{chainState = initialChainState}
            let chainConfig' = chainConfig{cardanoVerificationKeys = cardanoVerificationKey <$> remotes}
            chainContext <- loadChainContext chainConfig' party hydraScriptsTxId
            eq <- createEventQueue
            withDirectChain (contramap DirectChain tracer) chainConfig chainContext Nothing wallet (chainCallback nodeState eq) $ \chain -> do
              withMultiHead hid net (putEvent eq . NetworkEvent defaultTTL) $ \hn -> do
                withLocalAPIServer (putEvent eq . ClientEvent) $ \server -> do
                  let RunOptions{ledgerConfig} = opts
                  withCardanoLedger ledgerConfig $ \ledger ->
                    runHydraNode (contramap Node tracer) $
                      HydraNode{eq, hn, nodeState, oc = chain, server, ledger, env, persistence = noPersistence}
      udp callback $ \_net -> do
        runMultiHeadedNode (startNode udp) cmdQueue
 where
  withCardanoLedger ledgerConfig action = do
    globals <-
      newGlobals
        <$> readJsonFileThrow shelleyGenesisFromJson (cardanoLedgerGenesisFile ledgerConfig)

    ledgerEnv <-
      newLedgerEnv
        <$> readJsonFileThrow protocolParametersFromJson (cardanoLedgerProtocolParametersFile ledgerConfig)

    action (Ledger.cardanoLedger globals ledgerEnv)

  callback :: NetworkCallback (Enveloped (Message Tx)) IO
  callback = error "not implemented"

runMultiHeadedNode :: (TVar IO (Maybe HeadId) -> [Remote] -> IO ()) -> TQueue IO (Command, TMVar IO Result) -> IO ()
runMultiHeadedNode = error "not implemented"

withLocalAPIServer :: (ClientInput Tx -> IO ()) -> (Server Tx IO -> IO ()) -> IO ()
withLocalAPIServer = error "not implemented"

noPersistence :: Persistence (HeadState Tx) IO
noPersistence = error "not implemented"

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
  cmd <- read . unpack . decodeUtf8 <$> hGetLine h
  putText $ "Executing command " <> show cmd
  result <- atomically $ do
    res <- newEmptyTMVar
    writeTQueue queue (cmd, res)
    takeTMVar res
  hPutStr h (fromString (show result) <> "\n")
