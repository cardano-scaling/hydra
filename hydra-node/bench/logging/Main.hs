module Main where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  flushTBQueue,
  readTBQueue,
  writeTBQueue,
 )
import Control.Tracer (Tracer (..), traceWith)
import Criterion (bench, bgroup, nfIO)
import Criterion.Main (defaultMain)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)

import Hydra.Logging (Envelope (..), defaultQueueSize, mkEnvelope, withTracerOutputTo, withTracerLogFile)

main :: IO ()
main = do
  n <- fromMaybe 10000 . (>>= readMaybe) <$> lookupEnv "LOG_N"
  defaultMain
    [ bgroup
        "logging"
        [ bench "buffered" $ nfIO (runBuffered n)
        , bench "flush-each" $ nfIO (runFlushEach n)
        , bench "fast-logger" $ nfIO (runFastLogger n)
        ]
    ]

runBuffered :: Int -> IO ()
runBuffered n =
  withTempLogFile $ \hdl ->
    withTracerOutputTo hdl "logging-bench" $ \tracer ->
      replicateM_ n (traceWith tracer benchMessage)

runFastLogger :: Int -> IO ()
runFastLogger n =
  withTempLogFilePath $ \path ->
    withTracerLogFile path "logging-bench" $ \tracer ->
      replicateM_ n (traceWith tracer benchMessage)

runFlushEach :: Int -> IO ()
runFlushEach n =
  withTempLogFile $ \hdl ->
    withTracerOutputToFlushEach hdl "logging-bench" $ \tracer ->
      replicateM_ n (traceWith tracer benchMessage)

benchMessage :: Value
benchMessage = object ["bench" .= ("logging" :: Text), "value" .= (1 :: Int)]

withTempLogFile :: (Handle -> IO a) -> IO a
withTempLogFile action = do
  tmpDir <- getTemporaryDirectory
  (path, hdl) <- openTempFile tmpDir "hydra-logging-bench.log"
  action hdl `finally` (hClose hdl >> removeFile path)

-- | Create a temp file path for fast-logger, removing the file on exit.
withTempLogFilePath :: (FilePath -> IO a) -> IO a
withTempLogFilePath action = do
  tmpDir <- getTemporaryDirectory
  (path, hdl) <- openTempFile tmpDir "hydra-logging-bench.log"
  hClose hdl
  action path `finally` removeFile path

-- | Flush each log entry to model the legacy behavior.
withTracerOutputToFlushEach ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputToFlushEach hdl namespace action = do
  msgQueue <- newLabelledTBQueueIO @_ @(Envelope msg) "logging-msg-queue" defaultQueueSize
  withAsyncLabelled ("logging-writeLogs", writeLogs msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
 where
  tracer queue =
    Tracer $
      mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

  writeLogs queue =
    forever $ do
      atomically (readTBQueue queue) >>= write . Aeson.encode
      hFlush hdl

  flushLogs queue = liftIO $ do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write . Aeson.encode)
    hFlush hdl

  write bs = LBS.hPut hdl (bs <> "\n")
