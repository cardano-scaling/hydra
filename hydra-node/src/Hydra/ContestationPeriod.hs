module Hydra.ContestationPeriod where

import Data.Ratio ((%))
import qualified Hydra.Data.ContestationPeriod as OnChain
import Hydra.Prelude
import Test.QuickCheck (Positive (getPositive))

-- | A positive number of seconds.
newtype ContestationPeriod = UnsafeContestationPeriod Natural
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

instance Arbitrary ContestationPeriod where
  -- NOTE: fromInteger to avoid overlapping instances for 'Arbitrary Natural'
  arbitrary = UnsafeContestationPeriod . fromInteger . getPositive <$> (arbitrary :: Gen (Positive Integer))

mkContestationPeriod :: Natural -> Maybe ContestationPeriod
mkContestationPeriod n
  | n == 0 = Nothing
  | otherwise = Just (UnsafeContestationPeriod n)

contestationPeriodSeconds :: ContestationPeriod -> Natural
contestationPeriodSeconds (UnsafeContestationPeriod s) = s

-- | Convert an off-chain contestation period to its on-chain representation.
contestationPeriodToChain :: ContestationPeriod -> OnChain.ContestationPeriod
contestationPeriodToChain (UnsafeContestationPeriod s) =
  OnChain.UnsafeContestationPeriod . fromIntegral $ s * 1000

-- | Convert an on-chain contestation period to its off-chain representation.
-- NOTE: Does truncate to whole seconds.
contestationPeriodFromChain :: OnChain.ContestationPeriod -> ContestationPeriod
contestationPeriodFromChain cp =
  UnsafeContestationPeriod . truncate $ toInteger (OnChain.milliseconds cp) % 1000
