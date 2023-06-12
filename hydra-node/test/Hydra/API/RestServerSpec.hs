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
    specify "DraftCommitTxRequest" $ \dir -> do
      property $
        withMaxSuccess 1 $
          conjoin
            [ prop_validateJSONSchema @(DraftCommitTxRequest Tx) (dir </> "api.yaml") (key "channels" . key "/commit" . key "publish" . key "message")
            , prop_specIsComplete @(ReasonablySized (DraftCommitTxRequest Tx)) (dir </> "api.yaml") apiSpecificationSelector
            ]

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "components" . key "message"
