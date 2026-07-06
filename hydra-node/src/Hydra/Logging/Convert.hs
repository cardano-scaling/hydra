-- | Convert CBOR-encoded hydra-node logs (as produced with @--log-format
-- cbor@) back to newline-delimited JSON for inspection.
--
-- The CBOR log stream is an RFC 8742 CBOR sequence: concatenated
-- self-delimiting items, each an 'Envelope' ('HydraLog' 'Tx') optionally
-- wrapped in tag 55799 (\"self-described CBOR\", bytes @D9 D9 F7@) — see
-- 'Hydra.Logging.encodeEnvelopeCbor'. Decoding is incremental and runs in
-- constant memory, so multi-gigabyte log files are fine.
module Hydra.Logging.Convert where

import Hydra.Prelude

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR.Read
import Control.Monad.ST (RealWorld, stToIO)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (Tx)
import Hydra.Logging (Envelope)
import Hydra.Logging.Messages (HydraLog)

-- | Result of converting a CBOR log stream.
data ConvertResult = ConvertResult
  { converted :: Word64
  -- ^ Number of log entries successfully converted.
  , failedAt :: Maybe ConvertFailure
  -- ^ 'Just' if the stream ended in undecodable data (e.g. a torn write
  -- from a killed node, or corruption).
  }
  deriving stock (Eq, Show)

data ConvertFailure = ConvertFailure
  { byteOffset :: Word64
  -- ^ Offset (in bytes from the start of the input) of the item that failed.
  , failureReason :: Text
  }
  deriving stock (Eq, Show)

-- | Thrown on unexpected I/O level problems (not on undecodable input, which
-- is reported via 'ConvertResult').
newtype ConvertLogsException = ConvertLogsException Text
  deriving stock (Show)

instance Exception ConvertLogsException where
  displayException (ConvertLogsException reason) = toString reason

chunkSize :: Int
chunkSize = 65536

-- | Convert a log stream read from the first handle to the second one.
-- Auto-detects the input format: JSON input (first byte @{@ or whitespace) is
-- passed through unchanged, CBOR input is decoded and re-encoded as JSON
-- lines.
convertLogStream :: Handle -> Handle -> IO ConvertResult
convertLogStream inH outH = do
  firstChunk <- BS.hGetSome inH chunkSize
  case BS8.uncons (BS8.dropWhile (`elem` (" \t\r\n" :: [Char])) firstChunk) of
    Nothing ->
      -- Empty input.
      pure ConvertResult{converted = 0, failedAt = Nothing}
    Just (c, _)
      | c == '{' -> passThroughJson firstChunk
      | otherwise -> decodeItems 0 0 firstChunk
 where
  passThroughJson firstChunk = do
    BS.hPut outH firstChunk
    let go !n = do
          chunk <- BS.hGetSome inH chunkSize
          if BS.null chunk
            then pure ConvertResult{converted = n, failedAt = Nothing}
            else BS.hPut outH chunk >> go n
    -- NOTE: We do not count lines when passing through.
    go 0

  -- Decode one item at a time; 'baseOffset' tracks the absolute offset of the
  -- current item start for error reporting.
  decodeItems :: Word64 -> Word64 -> ByteString -> IO ConvertResult
  decodeItems !count !baseOffset chunk
    | BS.null chunk = do
        next <- BS.hGetSome inH chunkSize
        if BS.null next
          then pure ConvertResult{converted = count, failedAt = Nothing}
          else decodeItems count baseOffset next
    | otherwise = do
        idecode <- stToIO $ CBOR.Read.deserialiseIncremental decodeLogItem
        step count baseOffset chunk idecode

  step :: Word64 -> Word64 -> ByteString -> CBOR.Read.IDecode RealWorld (Envelope (HydraLog Tx)) -> IO ConvertResult
  step !count !baseOffset pending = \case
    CBOR.Read.Partial resume -> do
      chunk <- if BS.null pending then BS.hGetSome inH chunkSize else pure pending
      if BS.null chunk
        then stToIO (resume Nothing) >>= finalStep count baseOffset
        else stToIO (resume (Just chunk)) >>= step count baseOffset BS.empty
    CBOR.Read.Done rest offset envelope -> do
      LBS.hPut outH (Aeson.encode envelope <> "\n")
      decodeItems (count + 1) (baseOffset + fromIntegral offset) rest
    CBOR.Read.Fail _ offset err ->
      pure
        ConvertResult
          { converted = count
          , failedAt =
              Just
                ConvertFailure
                  { byteOffset = baseOffset + fromIntegral offset
                  , failureReason = show err
                  }
          }

  -- After feeding EOF: the decoder either finishes (input ended exactly on an
  -- item boundary is handled in decodeItems, so this is an item that needed
  -- no more bytes) or reports the truncation.
  finalStep :: Word64 -> Word64 -> CBOR.Read.IDecode RealWorld (Envelope (HydraLog Tx)) -> IO ConvertResult
  finalStep !count !baseOffset = \case
    CBOR.Read.Partial _ ->
      pure
        ConvertResult
          { converted = count
          , failedAt =
              Just
                ConvertFailure
                  { byteOffset = baseOffset
                  , failureReason = "unexpected end of input (truncated log entry)"
                  }
          }
    CBOR.Read.Done _ _ envelope -> do
      LBS.hPut outH (Aeson.encode envelope <> "\n")
      pure ConvertResult{converted = count + 1, failedAt = Nothing}
    CBOR.Read.Fail _ offset err ->
      pure
        ConvertResult
          { converted = count
          , failedAt =
              Just
                ConvertFailure
                  { byteOffset = baseOffset + fromIntegral offset
                  , failureReason = show err
                  }
          }

-- | Decode a single log entry, tolerating (and skipping) the optional
-- self-described CBOR tag 55799 each item is wrapped in.
decodeLogItem :: CBOR.Decoder s (Envelope (HydraLog Tx))
decodeLogItem = do
  tokenType <- CBOR.peekTokenType
  case tokenType of
    CBOR.TypeTag -> do
      tag <- CBOR.decodeTag
      unless (tag == 55799) $
        fail $
          "unexpected CBOR tag " <> show tag <> " (expected 55799)"
      fromCBOR
    _ -> fromCBOR
