{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Hydra.Cardano.Api
import Hydra.Prelude

import Control.Lens ((^?))
import Data.Aeson (eitherDecode', encode)
import Data.Aeson.Lens (key, nonNull, _Object)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Map.Strict qualified as Map
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog)
import Hydra.Node (HydraNodeLog)

data Decoded tx
  = DecodedHydraLog (HydraLog tx)
  | DecodedHydraNodeLog (HydraNodeLog tx)
  deriving (Show)

main :: IO ()
main = do
  logLines <- readFileLBS "../devnet/alice-logs.txt"
  let allLogLines = zip [1 ..] (C8.lines logLines)
  decodedLines <- forM allLogLines $ \(ix, l) ->
    let line = spy' "line" $ maybe l encode ((spy' "L" l) ^? key "message" . nonNull)
     in case decodeAs line (undefined :: Envelope (HydraLog Tx)) of
          Left e ->
            case decodeAs line (undefined :: Envelope (HydraNodeLog Tx)) of
              Left e -> error $ "Line " <> show ix <> ": error - " <> show e <> " line: " <> show line
              Right decoded -> pure $ DecodedHydraNodeLog $ decoded.message
          Right decoded -> pure $ DecodedHydraLog $ decoded.message
  print decodedLines
  pure ()

decodeAs :: forall a. FromJSON a => C8.ByteString -> a -> Either String a
decodeAs l _ =
  case eitherDecode' l :: Either String a of
    Left e -> Left e
    Right decoded -> pure decoded
