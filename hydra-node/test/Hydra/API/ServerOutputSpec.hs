module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.ServerOutput (ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenSpecsWithSettings,
 )

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (ServerOutput SimpleTx)))

  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (ServerOutput Tx)))

  prop "matches JSON schema" $
    prop_validateJSONSchema @(TimedServerOutput Tx) "api.json" $
      key "components" . key "schemas" . key "ServerOutput"

  -- NOTE: The golden file produced by this is also used by the
  -- 'validate:outputs' target in ./docs/package.json.
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (TimedServerOutput Tx)))

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
