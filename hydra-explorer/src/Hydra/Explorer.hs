module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Network (PortNumber)

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Chain.Direct.Tx (
  HeadObservation (..),
 )
import Hydra.Explorer.ExplorerState (ExplorerState, HeadState, aggregateHeadObservations)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai (
  Application,
  Response,
  pathInfo,
  rawPathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (withArgs)

observerHandler :: TVar IO ExplorerState -> [HeadObservation] -> IO ()
observerHandler explorerState observations = do
  atomically $
    modifyTVar' explorerState $
      aggregateHeadObservations observations

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    explorerState <- newTVarIO (mempty :: ExplorerState)
    let getHeads = readModelGetHeadIds explorerState
    args <- getArgs
    race
      -- FIXME: this is going to be problematic on mainnet.
      (withArgs (args <> ["--start-chain-from", "0"]) $ Hydra.ChainObserver.main (observerHandler explorerState))
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

  readModelGetHeadIds :: TVar IO ExplorerState -> GetHeads
  readModelGetHeadIds = readTVarIO

type GetHeads = IO [HeadState]

httpApp :: Tracer IO APIServerLog -> GetHeads -> Application
httpApp tracer getHeads req send = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod req
      , path = PathInfo $ rawPathInfo req
      }
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> send $ responseLBS status200 corsHeaders ""
    ("GET", ["heads"]) -> handleGetHeads getHeads req send
    (_, _) -> send handleNotFound

handleGetHeads ::
  -- | Read model of all known head ids
  GetHeads ->
  Application
handleGetHeads getHeads _req send = do
  heads <- getHeads
  send . responseLBS status200 (contentTypeHeader : corsHeaders) $ Aeson.encode heads

handleError :: Response
handleError =
  responseLBS status500 corsHeaders "INVALID REQUEST"

handleNotFound :: Response
handleNotFound =
  responseLBS status404 corsHeaders "NOT FOUND"

handleFile :: FilePath -> Response
handleFile filepath = responseFile status200 corsHeaders filepath Nothing

corsHeaders :: [(HeaderName, ByteString)]
corsHeaders =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Methods", "*")
  , ("Access-Control-Allow-Headers", "*")
  ]

contentTypeHeader :: (HeaderName, ByteString)
contentTypeHeader = ("Content-Type", "application/json")
