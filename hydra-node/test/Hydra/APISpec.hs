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
    aroundAll (withJsonSpecifications "api.yaml") $ do
      specify "ClientInput" $ \(specs, tmp) ->
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(ClientInput CardanoTx) specs "inputs" (tmp </> "ClientInput")
            , prop_specIsComplete @(ClientInput CardanoTx) specs (apiSpecificationSelector "inputs")
            ]
      specify "ServerOutput" $ \(specs, tmp) ->
        withMaxSuccess 1 $
          conjoin
            [ prop_validateToJSON @(ServerOutput CardanoTx) specs "outputs" (tmp </> "ServerOutput")
            , prop_specIsComplete @(ServerOutput CardanoTx) specs (apiSpecificationSelector "outputs")
            ]

apiSpecificationSelector ::
  Text -> SpecificationSelector
apiSpecificationSelector namespace = key "properties" . key namespace . key "items"
