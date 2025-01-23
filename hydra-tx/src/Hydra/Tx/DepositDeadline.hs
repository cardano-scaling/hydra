module Hydra.Tx.DepositDeadline where

import Hydra.Prelude hiding (Show, show)

import Data.Fixed (Pico)
import Data.Time (secondsToNominalDiffTime)
import Test.QuickCheck (choose)
import Text.Show (Show (..))

-- | A positive, non-zero number of seconds.
newtype DepositDeadline = UnsafeDepositDeadline Natural
  deriving stock (Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance Show DepositDeadline where
  show (UnsafeDepositDeadline s) = show s <> "s"

instance Arbitrary DepositDeadline where
  arbitrary = UnsafeDepositDeadline . fromInteger <$> choose (1, 300)

-- | Create a 'DepositDeadline' from a 'NominalDiffTime'. This will fail if a
-- negative NominalDiffTime is provided and truncates to 1s if values < 1s are given.
depositFromNominalDiffTime :: MonadFail m => NominalDiffTime -> m DepositDeadline
depositFromNominalDiffTime dt =
  if seconds > 0
    then pure . UnsafeDepositDeadline $ ceiling seconds
    else fail $ "depositFromNominalDiffTime: deposit deadline <= 0: " <> show dt
 where
  seconds :: Pico = realToFrac dt

depositToNominalDiffTime :: DepositDeadline -> NominalDiffTime
depositToNominalDiffTime (UnsafeDepositDeadline s) =
  secondsToNominalDiffTime $ fromIntegral s
