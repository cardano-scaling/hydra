{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.APISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.ClientInput (ClientInput)
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateToJSON, withJsonSpecifications)
import Hydra.Ledger.Cardano (Tx)
import Hydra.ServerOutput (ServerOutput)
import System.FilePath ((</>))
import Test.QuickCheck.Property (conjoin, withMaxSuccess)

spec :: Spec
spec = parallel $ do
  context "validates JSON representations against API specification" $ do
    aroundAll withJsonSpecifications $ do
      specify "ClientInput" $ \dir ->
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(ClientInput Tx) (dir </> "api.yaml") "inputs" (dir </> "ClientInput")
            , prop_specIsComplete @(ClientInput Tx) (dir </> "api.yaml") (apiSpecificationSelector "inputs")
            ]
      specify "ServerOutput" $ \dir ->
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(ServerOutput Tx) (dir </> "api.yaml") "outputs" (dir </> "ServerOutput")
            , prop_specIsComplete @(ServerOutput Tx) (dir </> "api.yaml") (apiSpecificationSelector "outputs")
            ]

apiSpecificationSelector ::
  Text -> SpecificationSelector
apiSpecificationSelector namespace = key "properties" . key namespace . key "items"
