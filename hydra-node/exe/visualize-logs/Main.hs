module Main where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Logging.Messages (HydraLog)
import Hydra.Utils (readJsonFileThrow)

main :: IO ()
main = do
  results <- readJsonFileThrow parseJSON "devnet/alice-logs.txt" :: IO (Either String (HydraLog Tx))
  pure ()
