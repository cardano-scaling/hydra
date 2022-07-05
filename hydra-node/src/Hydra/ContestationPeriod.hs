module Hydra.ContestationPeriod where

import qualified Data.Time as Time
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

periodToNominalDiffTime :: ContestationPeriod -> NominalDiffTime
periodToNominalDiffTime (UnsafeContestationPeriod s) = Time.secondsToNominalDiffTime $ fromIntegral s
