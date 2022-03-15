import Hydra.Prelude

import Hydra.Network (readHost)
import Hydra.Painter (Pixel (..), paintPixel, readNetworkId)

main :: IO ()
main = do
  key <- fromMaybe (error "set HYDRA_SIGNING_KEY environment variable") <$> lookupEnv "HYDRA_SIGNING_KEY"
  networkId <- readNetworkId . error "set NETWORK_ID environment variable" <$> lookupEnv "NETWORK_ID"
  host <- readHost . fromMaybe (error "set HYDRA_API_HOST environment variable") =<< lookupEnv "NETWORK_ID"
  args <- getArgs
  case readMaybe <$> args of
    [Just x, Just y, Just red, Just green, Just blue] -> paintPixel key networkId host Pixel{x, y, red, green, blue}
    _ -> error "Expecting 5 word8 arguments: x,y,red/green/blue"
