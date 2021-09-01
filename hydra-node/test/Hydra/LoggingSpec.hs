{-# LANGUAGE TypeApplications #-}

module Hydra.LoggingSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Lens (to)
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, _Array)
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateToJSON, withJsonSpecifications)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Verbosity (Verbose), traceWith, withTracer)
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
          [ prop_validateToJSON @(Enveloppe (HydraLog SimpleTx ())) specs (tmp </> "HydraLog")
          , prop_specIsComplete @(HydraLog SimpleTx ()) specs apiSpecificationSelector
          ]

apiSpecificationSelector :: SpecificationSelector a
apiSpecificationSelector = key "properties" . key "message" . key "oneOf" . _Array . to toList

-- NOTE(AB): This type is used currently only for testing purpose in
-- to provide a simple way to generate arbitrary log entries. In the
-- actual logging code we directly build an `Object` using aeson
-- `Value` combinators.
data Enveloppe a = Enveloppe
  { namespace :: Text
  , timestamp :: UTCTime
  , thread :: Int
  , message :: a
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (Enveloppe a) where
  arbitrary = genericArbitrary
