module Main where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.Network (Host, readHost)
import Hydra.Painter (Pixel (..), paintPixel, withClient, withClientNoRetry)
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.Wai (
  Application,
  pathInfo,
  requestMethod,
  responseFile,
  responseLBS,
 )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as Wai
import Network.WebSockets qualified as WS
import Safe (readMay)

main :: IO ()
main = do
  key <- requireEnv "HYDRAW_CARDANO_SIGNING_KEY"
  host <- parseHost =<< requireEnv "HYDRA_API_HOST"
  network <- parseNetwork =<< requireEnv "HYDRAW_NETWORK"
  Warp.runSettings settings $
    Wai.websocketsOr WS.defaultConnectionOptions (websocketApp host) (httpApp network key host)
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

  parseHost :: String -> IO Host
  parseHost str =
    case readHost str of
      Nothing -> fail $ "Could not parse host address: " <> str
      Just host -> pure host

  -- Like cardano-cli: "mainnet" or a number for a given testnet network magic.
  parseNetwork :: String -> IO NetworkId
  parseNetwork str =
    case parseMainnet str <|> parseTestnetMagic str of
      Nothing -> fail $ "Could not parse network id: " <> str <> " (Expected 'mainnet' or a number)"
      Just nid -> pure nid

  parseMainnet :: String -> Maybe NetworkId
  parseMainnet str = Mainnet <$ guard (str == "mainnet")

  parseTestnetMagic :: String -> Maybe NetworkId
  parseTestnetMagic = fmap (Testnet . NetworkMagic) . readMaybe

-- | Like 'lookupEnv' but terminate program with a message if environment
-- variable is not set.
requireEnv :: String -> IO String
requireEnv name =
  lookupEnv name >>= \case
    Just value -> pure value
    Nothing -> die $ "Error: Required environment variable " <> name <> " not set"

websocketApp :: Host -> WS.PendingConnection -> IO ()
websocketApp host pendingConnection = do
  frontend <- WS.acceptRequest pendingConnection
  withClient host $ \backend ->
    raceLabelled_
      ("forever-receive-frontend-send-backend", forever $ WS.receive frontend >>= WS.send backend)
      ("forever-receive-backend-send-frontend", forever $ WS.receive backend >>= WS.send frontend)

httpApp :: NetworkId -> FilePath -> Host -> Application
httpApp networkId key host req send =
  case (requestMethod req, pathInfo req) of
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, red, green, blue] -> do
          putStrLn $ show (x, y) <> " -> " <> show (red, green, blue)
          -- \| spawn a connection in a new thread
          void $ asyncLabelled "client-paint-pixel" $ withClientNoRetry False host $ \cnx ->
            paintPixel networkId key host cnx Pixel{x, y, red, green, blue}
          send $ responseLBS status200 corsHeaders "OK"
        _ ->
          send handleError
    ("HEAD", _) -> send $ responseLBS status200 corsHeaders ""
    -- Statically serve files
    ("GET", []) -> send $ handleFile "index.html"
    ("GET", ["index.html"]) -> send $ handleFile "index.html"
    ("GET", ["bundle.js"]) -> send $ handleFile "bundle.js"
    ("GET", ["style.css"]) -> send $ handleFile "style.css"
    ("GET", ["logo.png"]) -> send $ handleFile "logo.png"
    _ ->
      send handleNotFound
 where
  handleError = responseLBS status400 corsHeaders "INVALID REQUEST"

  handleNotFound = responseLBS status404 corsHeaders "NOT FOUND"

  handleFile filepath = responseFile status200 corsHeaders filepath Nothing

  corsHeaders =
    [ ("Access-Control-Allow-Origin", "*")
    , ("Access-Control-Allow-Methods", "*")
    , ("Access-Control-Allow-Headers", "*")
    ]
