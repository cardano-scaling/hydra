{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.Environment where

import Hydra.Prelude (pure)
import Test.Hydra.Prelude

import Hydra.Node.Environment (Environment (..), placeholderSigningKey)
import Test.Hydra.Node.DepositPeriod ()
import Test.Hydra.Node.UnsyncedPeriod ()
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck.Instances.Text ()

-- 'arbitrary' here generates random Environment values but pins
-- 'signingKey' to 'placeholderSigningKey'. That matches what
-- 'FromJSON Environment' uses, so the 'Greetings' JSON roundtrip
-- (which checks @decode . encode == Right value@) is 'Eq'-stable.
instance Arbitrary Environment where
  arbitrary = do
    env <- genericArbitrary
    pure env{signingKey = placeholderSigningKey}
  shrink env =
    [env'{signingKey = placeholderSigningKey} | env' <- genericShrink env]
