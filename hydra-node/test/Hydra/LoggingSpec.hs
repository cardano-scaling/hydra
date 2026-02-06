module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Hydra.Logging (traceWith, withTracerOutputTo)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracerOutputTo LineBuffering stdout "test" $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    -- This test is flakey in CI. Suspected race condition.
    liftIO $ threadDelay 5

    captured `shouldContain` "{\"foo\":42}"
