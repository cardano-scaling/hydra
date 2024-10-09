module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.ChainObserver.NodeClient (ChainObservation)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateHeadObservations, initialTickState)
import Hydra.Explorer.Options (BlockfrostOptions (..), DirectOptions (..), Options (..), toArgProjectPath, toArgStartChainFrom)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Options qualified as Options
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant (serveDirectoryFileServer)
import Servant.API (Get, JSON, Raw, (:<|>) (..), (:>))
import Servant.Server (Application, Handler, Server, serve)
import System.Environment (withArgs)

type API :: Type
type API =
  "heads" :> Get '[JSON] [HeadState]
    :<|> "tick" :> Get '[JSON] TickState
    :<|> Raw

server ::
  GetExplorerState ->
  Server API
server getExplorerState =
  handleGetHeads getExplorerState
    :<|> handleGetTick getExplorerState
    :<|> serveDirectoryFileServer "static"

handleGetHeads ::
  GetExplorerState ->
  Handler [HeadState]
handleGetHeads getExplorerState =
  liftIO getExplorerState <&> \ExplorerState{heads} -> heads

handleGetTick ::
  GetExplorerState ->
  Handler TickState
handleGetTick getExplorerState = do
  liftIO getExplorerState <&> \ExplorerState{tick} -> tick

logMiddleware :: Tracer IO APIServerLog -> Middleware
logMiddleware tracer app' req sendResponse = do
  liftIO $
    traceWith tracer $
      APIHTTPRequestReceived
        { method = Method $ requestMethod req
        , path = PathInfo $ rawPathInfo req
        }
  app' req sendResponse

httpApp :: Tracer IO APIServerLog -> GetExplorerState -> Application
httpApp tracer getExplorerState =
  logMiddleware tracer
    . simpleCors
    . serve (Proxy @API)
    $ server getExplorerState

observerHandler :: ModifyExplorerState -> [ChainObservation] -> IO ()
observerHandler modifyExplorerState observations = do
  modifyExplorerState $
    aggregateHeadObservations observations

type GetExplorerState = IO ExplorerState

type ModifyExplorerState = (ExplorerState -> ExplorerState) -> IO ()

createExplorerState :: IO (GetExplorerState, ModifyExplorerState)
createExplorerState = do
  v <- newTVarIO (ExplorerState [] initialTickState)
  pure (getExplorerState v, modifyExplorerState v)
 where
  getExplorerState = readTVarIO
  modifyExplorerState v = atomically . modifyTVar' v

run :: Options -> IO ()
run opts = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    (getExplorerState, modifyExplorerState) <- createExplorerState

    let chainObserverArgs =
          case opts of
            DirectOpts DirectOptions{networkId, nodeSocket, startChainFrom} ->
              ["direct"]
                <> Options.toArgNodeSocket nodeSocket
                <> Options.toArgNetworkId networkId
                <> toArgStartChainFrom startChainFrom
            BlockfrostOpts BlockfrostOptions{projectPath, startChainFrom} ->
              ["blockfrost"]
                <> toArgProjectPath projectPath
                <> toArgStartChainFrom startChainFrom
    race_
      ( withArgs chainObserverArgs $
          Hydra.ChainObserver.main (observerHandler modifyExplorerState)
      )
      (Warp.runSettings (settings tracer) (httpApp tracer getExplorerState))
 where
  portToBind =
    case opts of
      DirectOpts DirectOptions{port} -> port
      BlockfrostOpts BlockfrostOptions{port} -> port

  settings tracer =
    Warp.defaultSettings
      & Warp.setPort (fromIntegral portToBind)
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop (traceWith tracer $ APIServerStarted portToBind)
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
