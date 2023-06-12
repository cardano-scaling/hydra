{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateJSONSchema, withJsonSpecifications)
import Hydra.Ledger.Cardano (Tx)
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck.Property (conjoin, property, withMaxSuccess)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecs
    (Proxy @(ReasonablySized (DraftCommitTxResponse Tx)))

  roundtripAndGoldenSpecs
    (Proxy @(ReasonablySized (DraftCommitTxRequest Tx)))

  aroundAll withJsonSpecifications $
    -- XXX: It's unintuitive that dir/api.yaml is in fact a JSON file holding
    -- the JSON schema!
    specify "Validate /commit publish api schema" $ \dir -> do
      property $
        withMaxSuccess 1 $ do
          conjoin
            [ prop_validateJSONSchema @(DraftCommitTxRequest Tx) (dir </> "api.yaml") (key "channels" . key "/commit" . key "publish" . key "message" . key "payload")
            , prop_specIsComplete @(ReasonablySized (DraftCommitTxRequest Tx)) (dir </> "api.yaml") apiSpecificationSelector
            ]
  aroundAll withJsonSpecifications $
    -- XXX: It's unintuitive that dir/api.yaml is in fact a JSON file holding
    -- the JSON schema!
    specify "Validate /commit subscribe api schema" $ \dir -> do
      property $
        withMaxSuccess 1 $ do
          conjoin
            [ prop_validateJSONSchema @(DraftCommitTxResponse Tx) (dir </> "api.yaml") (key "channels" . key "/commit" . key "subscribe" . key "message" . key "payload")
            , prop_specIsComplete @(ReasonablySized (DraftCommitTxResponse Tx)) (dir </> "api.yaml") apiSpecificationSelector
            ]

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "components" . key "message"
