{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId)
import Hydra.Network (Host, readHost)
import Hydra.Painter (Pixel (..), paintPixel, readNetworkId)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status200, status500)
import Network.Wai (
  Application,
  Response,
  pathInfo,
  requestMethod,
  responseLBS,
 )
import qualified Network.Wai.Handler.Warp as Warp
import Safe (readMay)

main :: IO ()
main = do
  key <- fromMaybe (error "set HYDRA_SIGNING_KEY environment variable") <$> lookupEnv "HYDRA_SIGNING_KEY"
  networkId <- readNetworkId . fromMaybe (error "set NETWORK_ID environment variable") <$> lookupEnv "NETWORK_ID"
  host <- readHost . fromMaybe (error "set HYDRA_API_HOST environment variable") =<< lookupEnv "HYDRA_API_HOST"
  Warp.runSettings settings (app key networkId host)
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

app :: FilePath -> NetworkId -> Host -> Application
app key networkId host req send =
  case (requestMethod req, pathInfo req) of
    ("HEAD", _) -> do
      send $
        responseLBS status200 corsHeaders ""
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, r, g, b] ->
          send =<< handleGetPaint key networkId host (x, y) (r, g, b)
        _ ->
          send handleError
    (_, _) ->
      send handleError

handleGetPaint :: FilePath -> NetworkId -> Host -> (Word8, Word8) -> (Word8, Word8, Word8) -> IO Response
handleGetPaint key networkId host (x, y) (red, green, blue) = do
  putStrLn $ show (x, y) <> " -> " <> show (red, green, blue)
  paintPixel key networkId host Pixel{x, y, red, green, blue}
  pure $ responseLBS status200 corsHeaders "OK"

handleError :: Response
handleError =
  responseLBS status500 corsHeaders "INVALID REQUEST"

corsHeaders :: [(HeaderName, ByteString)]
corsHeaders =
  [ ("Access-Control-Allow-Origin", "*")
  , ("Access-Control-Allow-Methods", "*")
  , ("Access-Control-Allow-Headers", "*")
  ]
