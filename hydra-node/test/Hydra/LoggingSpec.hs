module Hydra.LoggingSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "aeson" Data.Aeson (object, (.=))
import "silently" System.IO.Silently (capture_)

import Hydra.Logging (traceWith, withTracerOutputTo)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracerOutputTo LineBuffering stdout "test" $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    -- This test is flakey in CI. Suspected race condition.
    liftIO $ threadDelay 5

    captured `shouldContain` "{\"foo\":42}"
