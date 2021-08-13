module Hydra.LoggingSpec where

import Data.Aeson (object, (.=))
import Hydra.Logging (Verbosity (Verbose), traceWith, withTracer)
import Hydra.Prelude
import System.IO.Silently (capture_)
import Test.Hspec (Spec, describe, it, shouldContain)

spec :: Spec
spec = describe "Logging Tracer" $ do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracer (Verbose "test") $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    captured `shouldContain` "{\"foo\":42}"
