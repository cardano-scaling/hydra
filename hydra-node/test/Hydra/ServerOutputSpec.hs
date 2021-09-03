{-# LANGUAGE TypeApplications #-}

module Hydra.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Ledger.Simple (SimpleTx)
import Hydra.ServerOutput (ServerOutput)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)

spec :: Spec
spec = parallel $ roundtripAndGoldenSpecs (Proxy @(ServerOutput SimpleTx))
