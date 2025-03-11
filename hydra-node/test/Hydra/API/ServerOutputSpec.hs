module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.ServerOutput (ClientMessage, Greetings (..), ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_specIsComplete)
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
  roundtripAndGoldenSpecsWithSettings,
 )
import Test.QuickCheck (conjoin)

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (ServerOutput Tx))

  -- XXX: Should move these to websocket server tests
  prop "schema covers all defined server outputs" $
    conjoin
      [ prop_specIsComplete @(TimedServerOutput Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      , prop_specIsComplete @(Greetings Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      , prop_specIsComplete @(ClientMessage Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      ]

  -- NOTE: The golden file produced by this is also used by the
  -- 'validate:outputs' target in ./docs/package.json.
  roundtripAndGoldenSpecsWithSettings
    defaultSettings{sampleSize = 5}
    $ Proxy @(ReasonablySized (TimedServerOutput Tx))
