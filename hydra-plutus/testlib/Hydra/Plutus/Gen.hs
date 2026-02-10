{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Plutus.Gen where

import "hydra-prelude" Hydra.Prelude
import "QuickCheck" Test.QuickCheck (Arbitrary (..), vector)
import "bytestring" Data.ByteString qualified as BS

import Hydra.Data.ContestationPeriod (ContestationPeriod (..))
import Hydra.Data.Party (Party (..), partyFromVerificationKeyBytes)

instance Arbitrary ContestationPeriod where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Party where
  arbitrary = partyFromVerificationKeyBytes . BS.pack <$> vector 32
