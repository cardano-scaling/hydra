{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Network.Authenticate where

import Cardano.Crypto.Util (SignableRepresentation)
import Hydra.Network.Authenticate (AuthLog, Signed)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen ()

instance (Arbitrary msg, SignableRepresentation msg) => Arbitrary (Signed msg) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AuthLog where
  arbitrary = genericArbitrary
  shrink = genericShrink
