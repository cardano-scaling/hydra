module Main where

import Relude

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
main =
  Warp.runSettings settings app
 where
  port = 1337
  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost "localhost"
      & Warp.setBeforeMainLoop
        ( do
            putStrLn "Server started..."
            putStrLn $ "Listening on: tcp/" <> show port
        )

app :: Application
app req send =
  case (requestMethod req, pathInfo req) of
    ("GET", "paint" : args) -> do
      case traverse (readMay . toString) args of
        Just [x, y, r, g, b] ->
          send =<< handleGetPaint (x, y) (r, g, b)
        _ ->
          send handleError
    (_, _) ->
      send handleError

handleGetPaint :: (Int, Int) -> (Int, Int, Int) -> IO Response
handleGetPaint (x, y) (r, g, b) = do
  putStrLn $ show (x, y) <> " -> " <> show (r, g, b)
  pure $ responseLBS status200 [] "OK"

handleError :: Response
handleError =
  responseLBS status500 [] "INVALID REQUEST"
