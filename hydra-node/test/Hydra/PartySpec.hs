{-# LANGUAGE TypeApplications #-}

module Hydra.PartySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Party (Party (UnsafeParty))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Party)
  it "shows verification key as hex" $ do
    show (UnsafeParty 10) `shouldContain` "000a"
  it "supports aliases" $ do
    show (UnsafeParty 10) `shouldContain` "Bob"
