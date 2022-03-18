{-# LANGUAGE TypeApplications #-}

module Hydra.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.ServerOutput (ServerOutput)
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

settings :: Settings
settings =
  defaultSettings
    { sampleSize = 200
    }
