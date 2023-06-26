{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_validateJSONSchema)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck.Property (property, withMaxSuccess)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxResponse))
  roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxRequest))

  prop "Validate /commit publish api schema" $
    property $
      withMaxSuccess 1 $
        prop_validateJSONSchema @DraftCommitTxRequest "api.json" (key "components" . key "messages")

  prop "Validate /commit subscribe api schema" $
    property $
      withMaxSuccess 1 $
        prop_validateJSONSchema @DraftCommitTxResponse "api.json" (key "channels" . key "/commit" . key "subscribe")
