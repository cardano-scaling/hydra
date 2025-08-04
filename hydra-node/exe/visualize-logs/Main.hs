{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (encodeUtf8)

import Conduit
import Control.Lens ((^?))
import Data.Aeson (eitherDecode')
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text.Encoding (encodeUtf8)
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (..))

newtype Decoded tx
  = DecodedHydraLog Text
  deriving newtype (Show)

main :: IO ()
main = do
  decodedLines <-
    runConduitRes $
      sourceFileBS "../devnet/alice-logs.txt"
        .| linesUnboundedAsciiC
        .| mapMC
          ( \l ->
              case l ^? key "message" . _String of
                Nothing -> error "Failed to find key 'message' which was expected"
                Just line ->
                  let envelope = fromStrict $ encodeUtf8 line
                   in case decodeAs envelope (undefined :: Envelope (HydraLog Tx)) of
                        Left e -> error $ show e <> line
                        Right decoded ->
                          case decoded.message of
                            NodeOptions opt -> pure $ DecodedHydraLog $ "NODE STARTING: " <> show opt
                            Node msg -> pure $ DecodedHydraLog $ "NODE LOG: " <> show msg
                            _ -> pure $ DecodedHydraLog "_____"
          )
        .| sinkList
  forM_ decodedLines $ \l ->
    putTextLn (show l)

decodeAs :: forall a. FromJSON a => C8.ByteString -> a -> Either String a
decodeAs l _ =
  case eitherDecode' l :: Either String a of
    Left e -> Left e
    Right decoded -> pure decoded
