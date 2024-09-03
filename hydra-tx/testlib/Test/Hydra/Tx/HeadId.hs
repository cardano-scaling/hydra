{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Hydra.Tx.HeadId where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Hydra.Tx.HeadId (HeadId (..), HeadSeed (..))
import Test.QuickCheck (vectorOf)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

instance Arbitrary HeadId where
  arbitrary = UnsafeHeadId . BS.pack <$> vectorOf 16 arbitrary

instance Arbitrary HeadSeed where
  arbitrary = UnsafeHeadSeed . BS.pack <$> vectorOf 16 arbitrary
