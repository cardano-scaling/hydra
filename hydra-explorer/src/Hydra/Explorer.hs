module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Chain.Direct.Tx (HeadObservation)
import Hydra.Explorer.ExplorerState (ExplorerState, HeadState, aggregateHeadObservations)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Network (PortNumber)
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Server, throwError)
import Servant.API (Get, Header, JSON, addHeader, (:>))
import Servant.API.ResponseHeaders (Headers)
import Servant.Server (Application, Handler, err500, serve)
import System.Environment (withArgs)

type API =
  "heads"
    :> Get
        '[JSON]
        ( Headers
            '[ Header "Accept" String
             , Header "Access-Control-Allow-Origin" String
             , Header "Access-Control-Allow-Methods" String
             , Header "Access-Control-Allow-Headers" String
             ]
            [HeadState]
        )

type GetHeads = IO [HeadState]

api :: Proxy API
api = Proxy

server :: GetHeads -> Server API
server = handleGetHeads

handleGetHeads ::
  GetHeads ->
  Handler
    ( Headers
        '[ Header "Accept" String
         , Header "Access-Control-Allow-Origin" String
         , Header "Access-Control-Allow-Methods" String
         , Header "Access-Control-Allow-Headers" String
         ]
        [HeadState]
    )
handleGetHeads getHeads = do
  result <- liftIO $ try getHeads
  case result of
    Right heads -> do
      return $ addHeader "application/json" $ addCorsHeaders heads
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

httpApp :: Tracer IO APIServerLog -> GetHeads -> Application
httpApp tracer getHeads =
  logMiddleware tracer $ serve api $ server getHeads

observerHandler :: TVar IO ExplorerState -> [HeadObservation] -> IO ()
observerHandler explorerState observations = do
  atomically $
    modifyTVar' explorerState $
      aggregateHeadObservations observations

readModelGetHeadIds :: TVar IO ExplorerState -> GetHeads
readModelGetHeadIds = readTVarIO

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    explorerState <- newTVarIO (mempty :: ExplorerState)
    let getHeads = readModelGetHeadIds explorerState
    args <- getArgs
    race
      -- FIXME: this is going to be problematic on mainnet.
      ( withArgs (args <> ["--start-chain-from", "0"]) $
          Hydra.ChainObserver.main (observerHandler explorerState)
      )
      ( traceWith tracer (APIServerStarted (fromIntegral port :: PortNumber))
          *> Warp.runSettings (settings tracer) (httpApp tracer getHeads)
      )
      >>= \case
        Left{} -> error "Something went wrong"
        Right a -> pure a
 where
  port = 9090

  settings tracer =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "0.0.0.0"
      & Warp.setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})

addCorsHeaders ::
  a ->
  Headers
    [ Header "Access-Control-Allow-Origin" String
    , Header "Access-Control-Allow-Methods" String
    , Header "Access-Control-Allow-Headers" String
    ]
    a
addCorsHeaders = addHeader "*" . addHeader "*" . addHeader "*"
