{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.APISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.ClientInput (ClientInput)
import Hydra.JSONSchema (SpecificationSelector, prop_specIsComplete, prop_validateToJSON, withJsonSpecifications)
import Hydra.Ledger.Cardano (CardanoTx)
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
            [ prop_validateToJSON @(ClientInput CardanoTx) (dir </> "api.json") "inputs" (dir </> "ClientInput")
            , prop_specIsComplete @(ClientInput CardanoTx) (dir </> "api.json") (apiSpecificationSelector "inputs")
            ]
      specify "ServerOutput" $ \dir ->
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(ServerOutput CardanoTx) (dir </> "api.json") "outputs" (dir </> "ServerOutput")
            , prop_specIsComplete @(ServerOutput CardanoTx) (dir </> "api.json") (apiSpecificationSelector "outputs")
            ]

apiSpecificationSelector ::
  Text -> SpecificationSelector
apiSpecificationSelector namespace = key "properties" . key namespace . key "items"
