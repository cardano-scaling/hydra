{-# LANGUAGE TypeApplications #-}

module Hydra.ClientInputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.ClientInput (ClientInput)
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
    (Proxy @(ReasonablySized (ClientInput SimpleTx)))
  roundtripAndGoldenSpecsWithSettings
    settings
    (Proxy @(ReasonablySized (ClientInput Tx)))

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
