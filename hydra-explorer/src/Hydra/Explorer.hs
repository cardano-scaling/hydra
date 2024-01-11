module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (atomically, modifyTVar', newTVarIO)
import Data.ByteString.Char8 (unpack)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.Cardano.Api (ChainPoint, TxId)
import Hydra.Chain.Direct.Tx (HeadObservation)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Hydra.Network (PortNumber)
import Network.HTTP.Types (parseQuery, status200)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status404, status500)
import Network.Wai (
  Application,
  Response,
  pathInfo,
  rawPathInfo,
  rawQueryString,
  requestMethod,
  responseFile,
  responseLBS,
 )
import Network.Wai.Handler.Warp qualified as Warp
import Prelude (read)

type ExplorerState = Map ChainPoint [(TxId, HeadObservation)]

observerHandler :: TVar IO ExplorerState -> ChainPoint -> [(TxId, HeadObservation)] -> IO ()
observerHandler explorerState point observations =
  atomically $
    modifyTVar' explorerState (Map.insert point observations)

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    explorerState <- newTVarIO (mempty :: ExplorerState)
    race
      (Hydra.ChainObserver.main (observerHandler explorerState))
      ( traceWith tracer (APIServerStarted (fromIntegral port :: PortNumber))
          *> Warp.runSettings (settings tracer) (httpApp tracer explorerState)
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
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

httpApp :: Tracer IO APIServerLog -> TVar IO ExplorerState -> Application
httpApp tracer explorerState req send = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod req
      , path = PathInfo $ rawPathInfo req
      }
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> send $ responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["heads"]) -> do
      let queryParams = parseQuery $ rawQueryString req
          pageParam = join $ List.lookup "page" queryParams
          page :: Int = maybe 0 (Prelude.read . unpack) pageParam
      send $
        responseLBS status200 corsHeaders $
          "OK. Handling /heads route with pagination. Page: " <> show page
    -- FIXME: do proper file serving, this is dangerous
    ("GET", path) -> send $ handleFile $ toString $ mconcat $ List.intersperse "/" ("." : path)
    (_, _) -> send handleNotFound

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
