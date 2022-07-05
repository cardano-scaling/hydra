module Hydra.ContestationPeriod where

import Data.Ratio ((%))
import Data.Time (secondsToNominalDiffTime)
import qualified Hydra.Data.ContestationPeriod as OnChain
import Hydra.Prelude
import Test.QuickCheck (Positive (getPositive))

-- | A positive number of seconds.
newtype ContestationPeriod = UnsafeContestationPeriod Natural
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

instance Arbitrary ContestationPeriod where
  -- NOTE: fromInteger to avoid overlapping instances for 'Arbitrary Natural'
  arbitrary =
    UnsafeContestationPeriod
      . fromInteger
      . getPositive
      <$> (arbitrary :: Gen (Positive Integer))

-- | Convert an off-chain contestation period to its on-chain representation.
toChain :: ContestationPeriod -> OnChain.ContestationPeriod
toChain (UnsafeContestationPeriod s) =
  OnChain.UnsafeContestationPeriod
    . fromIntegral
    $ s * 1000

-- | Convert an on-chain contestation period to its off-chain representation.
-- NOTE: Does truncate to whole seconds.
fromChain :: OnChain.ContestationPeriod -> ContestationPeriod
fromChain cp =
  UnsafeContestationPeriod
    . truncate
    $ toInteger (OnChain.milliseconds cp) % 1000

toNominalDiffTime :: ContestationPeriod -> NominalDiffTime
toNominalDiffTime (UnsafeContestationPeriod s) =
  secondsToNominalDiffTime $ fromIntegral s
