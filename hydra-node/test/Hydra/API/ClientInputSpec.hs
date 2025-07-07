module Hydra.API.ClientInputSpec where

import Hydra.Prelude
    ( ($),
      Bool(False, True),
      Maybe(Just),
      (.),
      Arbitrary(arbitrary),
      ToJSON(toJSON),
      Proxy(Proxy),
      MinimumSized,
      ToString(toString),
      ToText(toText) )
import Test.Hydra.Prelude ( prop, describe, parallel, Spec )

import Data.Aeson (Result (..), fromJSON)
import Data.Aeson.Lens (key)
import Hydra.API.ClientInput (ClientInput)
import Hydra.Cardano.Api (serialiseToTextEnvelope)
import Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
 )
import Test.QuickCheck (counterexample, forAll, property)

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
