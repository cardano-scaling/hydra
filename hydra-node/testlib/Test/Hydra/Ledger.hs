{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger where

import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Ledger (ValidationError (..))
import "quickcheck-instances" Test.QuickCheck.Instances.Natural ()
import "quickcheck-instances" Test.QuickCheck.Instances.Text ()

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary
