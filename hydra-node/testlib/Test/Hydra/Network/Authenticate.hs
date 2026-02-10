{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Network.Authenticate where

import "cardano-crypto-class" Cardano.Crypto.Util (SignableRepresentation)
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Test.Hydra.Tx.Gen ()

import Hydra.Network.Authenticate (AuthLog, Signed)

instance (Arbitrary msg, SignableRepresentation msg) => Arbitrary (Signed msg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AuthLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
