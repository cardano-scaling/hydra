{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateToJSON, withJsonSpecifications)
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

  aroundAll withJsonSpecifications $ do
    specify "DraftCommitTxRequest" $ \dir -> do
      property $
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(ReasonablySized (DraftCommitTxRequest Tx)) (dir </> "api.yaml") "messages" (dir </> "DraftCommitTxRequest")
            , prop_specIsComplete @(ReasonablySized (DraftCommitTxRequest Tx)) (dir </> "api.yaml") apiSpecificationSelector
            ]

apiSpecificationSelector :: SpecificationSelector
apiSpecificationSelector = key "components" . key "message"