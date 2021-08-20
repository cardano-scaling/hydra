{-# LANGUAGE TypeApplications #-}

module Hydra.ClientInputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.ClientInput (ClientInput)
import Hydra.Ledger.Simple (SimpleTx)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)

spec :: Spec
spec = roundtripAndGoldenSpecs (Proxy @(ClientInput SimpleTx))
