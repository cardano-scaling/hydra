module Main where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (newTQueueIO, readTQueue, writeTQueue)
import Hydra.Network (Host, readHost)
import Hydra.Painter (Pixel (..), paintPixel, withClient)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status200, status404, status500)
import Network.Wai (
  Application,
  Response,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS
import Safe (readMay)

main :: IO ()
main = do
  key <- fromMaybe (error "set HYDRAW_CARDANO_SIGNING_KEY environment variable") <$> lookupEnv "HYDRAW_CARDANO_SIGNING_KEY"
  host <- readHost . fromMaybe (error "set HYDRA_API_HOST environment variable") =<< lookupEnv "HYDRA_API_HOST"
  withClient host $ \cnx -> do
    Wai.websocketsOr WS.defaultConnectionOptions (websocketApp host) (httpApp key cnx)
      & Warp.runSettings settings
 where
  port = 1337
  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "0.0.0.0"
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

websocketApp :: Host -> WS.PendingConnection -> IO ()
websocketApp host pendingConnection = do
  qA <- newTQueueIO
  qB <- newTQueueIO

  cnx <- WS.acceptRequest pendingConnection
  concurrently_
    (producer cnx (atomically . writeTQueue qA) (atomically (readTQueue qB)))
    (consumer (atomically . writeTQueue qB) (atomically (readTQueue qA)))
 where
  consumer yield await =
    withClient host $ \cnx ->
      concurrently_
        (forever $ await >>= WS.send cnx)
        (forever $ WS.receive cnx >>= yield)

  producer cnx yield await =
    concurrently_
      (forever $ await >>= WS.send cnx)
      (forever $ WS.receive cnx >>= yield)

httpApp :: FilePath -> WS.Connection -> Application
httpApp key cnx req send =
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> do
      send $
        responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["index.html"]) -> send $ handleFile "index.html"
    ("GET", ["bundle.js"]) -> send $ handleFile "bundle.js"
    ("GET", ["style.css"]) -> send $ handleFile "style.css"
    ("GET", ["logo.png"]) -> send $ handleFile "logo.png"
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, r, g, b] ->
          send =<< handleGetPaint key cnx (x, y) (r, g, b)
        _ ->
          send handleError
    (_, _) ->
      send handleNotFound

handleGetPaint :: FilePath -> WS.Connection -> (Word8, Word8) -> (Word8, Word8, Word8) -> IO Response
handleGetPaint key cnx (x, y) (red, green, blue) = do
  putStrLn $ show (x, y) <> " -> " <> show (red, green, blue)
  paintPixel key cnx Pixel{x, y, red, green, blue}
  pure $ responseLBS status200 corsHeaders "OK"

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
