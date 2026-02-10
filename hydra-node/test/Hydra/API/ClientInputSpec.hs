module Hydra.API.ClientInputSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (counterexample, forAll, property)
import "aeson" Data.Aeson (Result (..), fromJSON)
import "hspec-golden-aeson" Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
 )
import "hydra-cardano-api" Hydra.Cardano.Api (serialiseToTextEnvelope)
import "hydra-node" Hydra.API.ClientInput (ClientInput)
import "hydra-node" Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema)
import "hydra-node" Test.Hydra.API.ClientInput ()
import "hydra-tx" Hydra.Ledger.Cardano (Tx)
import "hydra-tx" Test.Hydra.Tx.Gen ()
import "lens-aeson" Data.Aeson.Lens (key)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (ClientInput Tx))

  prop "matches JSON schema" $
    prop_validateJSONSchema @(ClientInput Tx) "api.json" $
      key "components" . key "schemas" . key "ClientInput"

  -- XXX: This seems no to be working? Adding a new message does not lead to a failure here
  prop "schema covers all defined client inputs" $
    prop_specIsComplete @(ClientInput Tx) "api.json" $
      key "channels" . key "/" . key "publish" . key "message"

  describe "FromJSON (ValidatedTx era)" $ do
    prop "accepts transactions produced via cardano-cli" $
      forAll (arbitrary @Tx) $ \tx ->
        let envelope = toJSON $ serialiseToTextEnvelope (Just "Tx Babbage") tx
         in case fromJSON @Tx envelope of
              Success{} -> property True
              Error e -> counterexample (toString $ toText e) $ property False
