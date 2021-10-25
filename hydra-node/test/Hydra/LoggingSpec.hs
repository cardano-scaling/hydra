{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Lens (key)
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateToJSON, withJsonSpecifications)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Envelope (..), Verbosity (Verbose), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog)
import System.FilePath ((</>))
import System.IO.Silently (capture_)
import Test.QuickCheck.Property (conjoin, withMaxSuccess)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracer (Verbose "test") $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    captured `shouldContain` "{\"foo\":42}"

  aroundAll (withJsonSpecifications "api-log.yaml") $ do
    specify "HydraLog" $ \(specs, tmp) ->
      -- TODO(AB): Add arbitrary instances for network log entries
      withMaxSuccess 1 $
        conjoin
          [ prop_validateToJSON @(Envelope (HydraLog SimpleTx ())) specs (tmp </> "HydraLog")
          , prop_specIsComplete @(HydraLog SimpleTx ()) specs apiSpecificationSelector
          ]

-- NOTE(AB): We need this orphan instance because it's a constraint for HydraLog
-- As any Value is valid, generating a dummy object should be good enough
instance Arbitrary Value where
  arbitrary = pure $ object []

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "properties" . key "message"
