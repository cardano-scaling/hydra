{-# LANGUAGE TypeApplications #-}

module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key)
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateToJSON, withJsonSpecifications)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Envelope (..), Verbosity (Verbose), traceWith, withTracer)
import Hydra.Logging.Messages (HydraLog)
import System.FilePath ((</>))
import System.IO.Silently (capture_)
import Test.QuickCheck.Property (conjoin, withMaxSuccess, property)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracer (Verbose "test") $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    captured `shouldContain` "{\"foo\":42}"

  aroundAll (withJsonSpecifications "api-log.yaml") $ do
    -- TODO(AB): Add arbitrary instance for DirectChainLog
    xspecify "HydraLog" $ \(specs, tmp) -> do
      property $
        -- TODO(AB): Add arbitrary instances for network log entries
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(Envelope (HydraLog SimpleTx ())) specs (tmp </> "HydraLog")
            , prop_specIsComplete @(HydraLog SimpleTx ()) specs apiSpecificationSelector
            ]

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "properties" . key "message"
