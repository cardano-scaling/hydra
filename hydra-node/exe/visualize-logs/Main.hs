{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (encodeUtf8)

import Control.Lens ((^?))
import Data.Aeson (eitherDecode')
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text.Encoding (encodeUtf8)
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
  let allLogLines = zip [1 :: Int ..] (C8.lines logLines)
  decodedLines <- forM allLogLines $ \(ix, l) ->
    case l ^? key "message" . _String of
      Nothing -> error "Failed to find key 'message' which was expected"
      Just line ->
        let envelope = fromStrict $ encodeUtf8 line
         in case decodeAs envelope (undefined :: Envelope (HydraLog Tx)) of
              Left e -> error $ "Line " <> show ix <> ": error - " <> show e <> " line: " <> show line
              Right decoded -> pure $ DecodedHydraLog $ decoded.message
  print decodedLines
  pure ()

decodeAs :: forall a. FromJSON a => C8.ByteString -> a -> Either String a
decodeAs l _ =
  case eitherDecode' l :: Either String a of
    Left e -> Left e
    Right decoded -> pure decoded
