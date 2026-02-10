{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Adapter module to the actual logging framework.
-- All Hydra node components implements /Structured logging/ via [contra-tracer](https://hackage.haskell.org/package/contra-tracer)
-- generic logging framework. All logs are output in [JSON](https://www.json.org/json-en.html).
module Test.Hydra.Logging where

import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Logging

import "quickcheck-instances" Test.QuickCheck.Instances.Text ()

instance Arbitrary Verbosity where
  arbitrary = genericArbitrary
  shrink = genericShrink
