{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Plutus.Gen where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Hydra.Data.ContestationPeriod (ContestationPeriod (..))
import Hydra.Data.Party (Party (..), partyFromVerificationKeyBytes)
import Test.QuickCheck (Arbitrary (..), vector)

instance Arbitrary ContestationPeriod where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Party where
  arbitrary = partyFromVerificationKeyBytes . BS.pack <$> vector 32
