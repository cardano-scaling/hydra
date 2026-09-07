module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Exception (IOException)
import Control.Tracer.JSON (defaultLogBuffering, defaultQueueSize, traceWith, withTracerOutputTo)
import Data.Aeson (object, (.=))
import Data.Text.IO qualified as Text.IO
import System.FilePath ((</>))
import System.IO (hClose)
import System.Process (createPipe)

spec :: Spec
spec = do
  it "dumps logs to the given handle in JSON" $ do
    -- Write to a file handle instead of capturing the process-global stdout:
    -- capturing swaps the stdout file descriptor under every concurrently
    -- running test and raced the tracer's own shutdown flush.
    withTempDir "logging-spec" $ \dir -> do
      let logFile = dir </> "log.jsonl"
      withFile logFile WriteMode $ \h ->
        withTracerOutputTo LineBuffering h "test" $ \tracer ->
          traceWith tracer (object ["foo" .= (42 :: Int)])
      captured <- readFileBS logFile
      toString (decodeUtf8 @Text captured) `shouldContain` "{\"foo\":42}"

  -- A pipe is what a log reader actually gets, and unlike a file it cannot be
  -- satisfied after the fact by the flush on tracer shutdown.
  it "flushes entries without waiting for the buffer to fill" $
    withPipe $ \(readEnd, writeEnd) ->
      withTracerOutputTo defaultLogBuffering writeEnd "test" $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])
        -- The writer thread is asynchronous, so this blocks until it has both
        -- written and flushed the entry.
        line <- failAfter 5 $ Text.IO.hGetLine readEnd
        toString line `shouldContain` "{\"foo\":42}"

  it "keeps logging after the reader of its output has gone away" $
    withPipe $ \(readEnd, writeEnd) ->
      withTracerOutputTo defaultLogBuffering writeEnd "test" $ \tracer -> do
        hClose readEnd
        -- Writing to a pipe nobody reads raises an IOException, as GHC ignores
        -- SIGPIPE. The writer has to survive that: were it to die, the queue
        -- would fill and every subsequent 'traceWith' would block forever.
        failAfter 5 $
          forM_ [1 .. 2 * fromIntegral defaultQueueSize :: Int] $ \i ->
            traceWith tracer (object ["foo" .= i])
 where
  withPipe :: ((Handle, Handle) -> IO a) -> IO a
  withPipe = bracket createPipe $ \(readEnd, writeEnd) ->
    forM_ [readEnd, writeEnd] $ \h ->
      hClose h `catch` \(_ :: IOException) -> pure ()
