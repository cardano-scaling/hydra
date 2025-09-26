module Hydra.LoggingSpec where

import Hydra.Prelude

import Data.Aeson (object, (.=))
import Hydra.Logging (Verbosity (Verbose), traceWith, withTracer)
import System.IO.Silently (capture_)
import Test.Hspec (Spec, it, shouldContain)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracer (Verbose "test") $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    -- This test is flakey in CI. Suspected race condition.
    liftIO $ threadDelay 5

    captured `shouldContain` "{\"foo\":42}"
