module Hydra.Explorer where

import Hydra.ChainObserver qualified
import Hydra.Prelude

import Data.ByteString.Char8 (unpack)
import Data.List qualified as List
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
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

main :: IO ()
main = do
  withTracer (Verbose "hydra-explorer") $ \tracer -> do
    race
      Hydra.ChainObserver.main
      ( traceWith tracer (APIServerStarted (fromIntegral port :: PortNumber))
          *> Warp.runSettings (settings tracer) (httpApp tracer)
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

httpApp :: Tracer IO APIServerLog -> Application
httpApp tracer req send = do
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
          page :: Int = maybe 0 (read . unpack) pageParam
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
