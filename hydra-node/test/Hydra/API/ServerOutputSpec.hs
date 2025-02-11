module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson.Lens (key)
import Hydra.API.ServerOutput (TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.JSONSchema (prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
  roundtripAndGoldenSpecsWithSettings,
 )

spec :: Spec
spec = parallel $ do
  roundtripAndGoldenADTSpecsWithSettings
    defaultSettings{sampleSize = 1}
    $ Proxy @(StateChanged Tx)

  prop "matches JSON schema" $
    prop_validateJSONSchema @(TimedServerOutput Tx) "api.json" $
      key "components" . key "schemas" . key "StateChanged"

  -- NOTE: The golden file produced by this is also used by the
  -- 'validate:outputs' target in ./docs/package.json.
  roundtripAndGoldenSpecsWithSettings
    defaultSettings{sampleSize = 5}
    $ Proxy @(ReasonablySized (TimedServerOutput Tx))
