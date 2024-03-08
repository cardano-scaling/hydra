module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.ChainObserver (ChainObservation)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateHeadObservations, initialTickState)
import Hydra.Explorer.Options (Options (..), toArgStartChainFrom)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Options qualified as Options
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (serveDirectoryFileServer, throwError)
import Servant.API (Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler, Server, err500, serve)
import System.Environment (withArgs)

type API :: Type
type API =
  "heads"
    :> Get
        '[JSON]
        [HeadState]
    :<|> "tick"
      :> Get
          '[JSON]
          TickState
    :<|> Raw

type GetHeads :: Type
type GetHeads = IO [HeadState]

type GetTick :: Type
type GetTick = IO TickState

api :: Proxy API
api = Proxy

server ::
  GetHeads ->
  GetTick ->
  Server API
server getHeads getTick =
  handleGetHeads getHeads
    :<|> handleGetTick getTick
    :<|> serveDirectoryFileServer "static"

handleGetHeads ::
  GetHeads ->
  Handler [HeadState]
handleGetHeads getHeads = do
  result <- liftIO $ try getHeads
  case result of
    Right heads -> return heads
    Left (_ :: SomeException) -> throwError err500

handleGetTick ::
  GetTick ->
  Handler TickState
handleGetTick getTick = do
  result <- liftIO $ try getTick
  case result of
    Right tick -> return tick
    Left (_ :: SomeException) -> throwError err500

logMiddleware :: Tracer IO APIServerLog -> Middleware
logMiddleware tracer app' req sendResponse = do
  liftIO $
    traceWith tracer $
      APIHTTPRequestReceived
        { method = Method $ requestMethod req
        , path = PathInfo $ rawPathInfo req
        }
  app' req sendResponse

httpApp :: Tracer IO APIServerLog -> GetHeads -> GetTick -> Application
httpApp tracer getHeads getTick =
  logMiddleware tracer
    . simpleCors
    . serve api
    $ server getHeads getTick

observerHandler :: TVar IO ExplorerState -> [ChainObservation] -> IO ()
observerHandler explorerState observations = do
  atomically $
    modifyTVar' explorerState $
      aggregateHeadObservations observations

readModelGetHeadIds :: TVar IO ExplorerState -> GetHeads
readModelGetHeadIds explorerStateTVar = do
  ExplorerState{heads} <- readTVarIO explorerStateTVar
  pure heads

readModelGetTick :: TVar IO ExplorerState -> GetTick
readModelGetTick explorerStateTVar = do
  ExplorerState{tick} <- readTVarIO explorerStateTVar
  pure tick

run :: Options -> IO ()
run opts = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    explorerState <- newTVarIO (ExplorerState [] initialTickState)
    let getTick = readModelGetTick explorerState
        getHeads = readModelGetHeadIds explorerState
        chainObserverArgs =
          Options.toArgNodeSocket nodeSocket
            <> Options.toArgNetworkId networkId
            <> toArgStartChainFrom startChainFrom
    race_
      ( withArgs chainObserverArgs $
          Hydra.ChainObserver.main (observerHandler explorerState)
      )
      ( traceWith tracer (APIServerStarted port)
          *> Warp.runSettings (settings tracer) (httpApp tracer getHeads getTick)
      )
 where
  settings tracer =
    Warp.defaultSettings
      & Warp.setPort (fromIntegral port)
      & Warp.setHost "0.0.0.0"
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})

  Options
    { networkId
    , port
    , nodeSocket
    , startChainFrom
    } = opts
