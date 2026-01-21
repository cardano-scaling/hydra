{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger where

import Test.Hydra.Prelude

import Hydra.Ledger (ValidationError (..))
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Instances.Text ()

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary
