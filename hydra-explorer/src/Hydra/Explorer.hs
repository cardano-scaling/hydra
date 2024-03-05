module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.ChainObserver (ChainObservation)
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState, TickState, aggregateHeadObservations, initialTickState)
import Hydra.Explorer.Options (Options (..), hydraExplorerOptions, toArgStartChainFrom)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Options qualified as Options
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative (execParser)
import Servant (serveDirectoryFileServer, throwError)
import Servant.API (Get, Header, JSON, Raw, addHeader, (:<|>) (..), (:>))
import Servant.API.ResponseHeaders (Headers)
import Servant.Server (Application, Handler, Tagged, err500, serve)
import System.Environment (withArgs)

type CorsHeaders :: [Type]
type CorsHeaders =
  [ Header "Access-Control-Allow-Origin" String
  , Header "Access-Control-Allow-Methods" String
  , Header "Access-Control-Allow-Headers" String
  ]

-- REVIEW: maybe rename
type GetHeadsHeaders :: [Type]
type GetHeadsHeaders = Header "Accept" String ': CorsHeaders

type API :: Type
type API =
  "heads"
    :> Get
        '[JSON]
        ( Headers
            GetHeadsHeaders
            [HeadState]
        )
    :<|> "tick"
      :> Get
          '[JSON]
          ( Headers
              GetHeadsHeaders
              TickState
          )
    :<|> Raw

type GetHeads :: Type
type GetHeads = IO [HeadState]

type GetTick :: Type
type GetTick = IO TickState

api :: Proxy API
api = Proxy

server ::
  forall (m :: Type -> Type).
  GetHeads ->
  GetTick ->
  Handler (Headers GetHeadsHeaders [HeadState])
    :<|> Handler (Headers GetHeadsHeaders TickState)
    :<|> Tagged m Application
server getHeads getTick =
  handleGetHeads getHeads
    :<|> handleGetTick getTick
    :<|> serveDirectoryFileServer "static"

handleGetHeads ::
  GetHeads ->
  Handler (Headers GetHeadsHeaders [HeadState])
handleGetHeads getHeads = do
  result <- liftIO $ try getHeads
  case result of
    Right heads -> do
      return $ addHeader "application/json" $ addCorsHeaders heads
    Left (_ :: SomeException) -> throwError err500

handleGetTick ::
  GetTick ->
  Handler (Headers GetHeadsHeaders TickState)
handleGetTick getTick = do
  result <- liftIO $ try getTick
  case result of
    Right tick -> do
      return $ addHeader "application/json" $ addCorsHeaders tick
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
  logMiddleware tracer $ serve api $ server getHeads getTick

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

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    opts <- execParser hydraExplorerOptions
    let Options
          { networkId
          , port
          , nodeSocket
          , startChainFrom
          } = opts
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
          *> Warp.runSettings (settings tracer port) (httpApp tracer getHeads getTick)
      )
 where
  settings tracer port =
    Warp.defaultSettings
      & Warp.setPort (fromIntegral port)
      & Warp.setHost "0.0.0.0"
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})

addCorsHeaders :: a -> Headers CorsHeaders a
addCorsHeaders = addHeader "*" . addHeader "*" . addHeader "*"
