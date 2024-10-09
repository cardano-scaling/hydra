module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key)
import Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Envelope (..), Verbosity (Verbose), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog)
import System.IO.Silently (capture_)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracer (Verbose "test") $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    -- This test is flakey in CI. Suspected race condition.
    liftIO $ threadDelay 5

    captured `shouldContain` "{\"foo\":42}"

  prop "Validates logs.yaml schema" $
    prop_validateJSONSchema @(Envelope (HydraLog Tx ())) "logs.json" id

  prop "Schema covers all defined log entries" $
    prop_specIsComplete @(HydraLog Tx ()) "logs.json" (key "properties" . key "message")
