module Main where

import Hydra.Prelude

import Hydra.Network (Host, readHost)
import Hydra.Painter (Pixel (..), paintPixel)
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
import Safe (readMay)

main :: IO ()
main = do
  key <- fromMaybe (error "set HYDRA_SIGNING_KEY environment variable") <$> lookupEnv "HYDRA_SIGNING_KEY"
  host <- readHost . fromMaybe (error "set HYDRA_API_HOST environment variable") =<< lookupEnv "HYDRA_API_HOST"
  Warp.runSettings settings (app key host)
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

app :: FilePath -> Host -> Application
app key host req send =
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> do
      send $
        responseLBS status200 corsHeaders ""
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["index.html"]) -> send $ handleFile "index.html"
    ("GET", ["bundle.js"]) -> send $ handleFile "bundle.js"
    ("GET", ["style.css"]) -> send $ handleFile "style.css"
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, r, g, b] ->
          send =<< handleGetPaint key host (x, y) (r, g, b)
        _ ->
          send handleError
    (_, _) ->
      send handleNotFound

handleGetPaint :: FilePath -> Host -> (Word8, Word8) -> (Word8, Word8, Word8) -> IO Response
handleGetPaint key host (x, y) (red, green, blue) = do
  putStrLn $ show (x, y) <> " -> " <> show (red, green, blue)
  paintPixel key host Pixel{x, y, red, green, blue}
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
