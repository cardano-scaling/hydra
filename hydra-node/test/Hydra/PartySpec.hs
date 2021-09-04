{-# LANGUAGE TypeApplications #-}

module Hydra.PartySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Party (Party)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)

spec :: Spec
spec = roundtripAndGoldenSpecs (Proxy @Party)
