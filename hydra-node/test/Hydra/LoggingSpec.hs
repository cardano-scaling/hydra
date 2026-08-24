module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Hydra.Logging (traceWith, withTracerOutputTo)
import System.FilePath ((</>))

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
