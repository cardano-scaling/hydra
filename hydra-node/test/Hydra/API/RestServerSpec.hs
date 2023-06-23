{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck.Property (conjoin, property, withMaxSuccess)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecs
    (Proxy @(ReasonablySized (DraftCommitTxResponse Tx)))

  roundtripAndGoldenSpecs
    (Proxy @(ReasonablySized DraftCommitTxRequest))

  prop "Validate /commit publish api schema" $
    property $
      withMaxSuccess 1 $ do
        conjoin
          [ prop_validateJSONSchema @DraftCommitTxRequest "api.json" (key "components" . key "messages")
          ]

  prop "Validate /commit subscribe api schema" $
    property $
      withMaxSuccess 1 $ do
        conjoin
          [ prop_validateJSONSchema @(DraftCommitTxResponse Tx) "api.json" (key "channels" . key "/commit" . key "subscribe")
          , prop_specIsComplete @(DraftCommitTxResponse Tx) "api.json" apiSpecificationSelector
          ]

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "components" . key "messages"
