module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Data.Text.IO qualified as Text.IO
import Hydra.Logging (traceWith, withTracerOutputTo)
import System.FilePath ((</>))
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

  -- The node logs through a block-buffered handle, so an entry only reaches
  -- whoever is reading the other end once the writer flushes. Without an
  -- explicit flush the first entries sit in the buffer until 64KB has
  -- accumulated, which makes a node that is slow to start indistinguishable,
  -- to 'docker logs' or to a supervisor, from one that never started at all.
  -- A pipe is what such a reader actually gets, and unlike a file it cannot be
  -- satisfied after the fact by the unconditional flush on tracer shutdown.
  it "flushes entries without waiting for the buffer to fill" $ do
    (readEnd, writeEnd) <- createPipe
    withTracerOutputTo (BlockBuffering (Just 64000)) writeEnd "test" $ \tracer -> do
      traceWith tracer (object ["foo" .= (42 :: Int)])
      -- The writer thread is asynchronous, so this blocks until it has both
      -- written and flushed the entry.
      line <- failAfter 5 $ Text.IO.hGetLine readEnd
      toString line `shouldContain` "{\"foo\":42}"
