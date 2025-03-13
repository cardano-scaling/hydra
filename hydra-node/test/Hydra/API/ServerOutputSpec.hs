module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.ServerOutput (ClientMessage, Greetings (..), ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
 )
import Test.QuickCheck (conjoin, withMaxSuccess)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (ServerOutput Tx))

  prop "matches JSON schema" $
    withMaxSuccess 1 $
      conjoin
        [ prop_validateJSONSchema @(TimedServerOutput Tx) "api.json" $
            key "components" . key "schemas" . key "ServerOutput"
        , prop_validateJSONSchema @(Greetings Tx) "api.json" $
            key "components" . key "schemas" . key "Greetings"
        , prop_validateJSONSchema @(ClientMessage Tx) "api.json" $
            key "components" . key "schemas" . key "ClientMessage"
        ]

  -- XXX: This seems no to be working? Adding a new message does not lead to a failure here
  prop "schema covers all defined server outputs" $
    conjoin
      [ prop_specIsComplete @(TimedServerOutput Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      , prop_specIsComplete @(Greetings Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      , prop_specIsComplete @(ClientMessage Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      ]
