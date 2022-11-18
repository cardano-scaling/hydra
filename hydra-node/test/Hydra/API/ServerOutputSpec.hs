{-# LANGUAGE TypeApplications #-}

module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.ServerOutput (ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
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
