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
import Test.QuickCheck.Property (conjoin, property, withMaxSuccess)

spec :: Spec
spec = do
  it "dumps logs to stdout in JSON with timestamp" $ do
    captured <- capture_ $ do
      withTracer (Verbose "test") $ \tracer -> do
        traceWith tracer (object ["foo" .= (42 :: Int)])

    captured `shouldContain` "{\"foo\":42}"

  aroundAll (withJsonSpecifications "api-log.yaml") $ do
    specify "HydraLog" $ \(specs, tmp) -> do
      property $
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(Envelope (HydraLog SimpleTx ())) specs "messages" (tmp </> "HydraLog")
            , prop_specIsComplete @(HydraLog SimpleTx ()) specs apiSpecificationSelector
            ]

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "properties" . key "message"
