module Hydra.Tx.ContestationPeriod where

import Hydra.Prelude hiding (Show, show)

import Data.Fixed (Pico)
import Data.Ratio ((%))
import Data.Time (secondsToNominalDiffTime)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Test.QuickCheck (choose, oneof)
import Text.Show (Show (..))

-- | A positive, non-zero number of seconds.
newtype ContestationPeriod = UnsafeContestationPeriod Natural
  deriving stock (Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance Show ContestationPeriod where
  show (UnsafeContestationPeriod s) = show s <> "s"

instance Arbitrary ContestationPeriod where
  arbitrary = do
    UnsafeContestationPeriod
      . fromInteger
      <$> oneof
        [ choose (1, confirmedHorizon)
        , pure confirmedHorizon
        , choose (confirmedHorizon, oneDay)
        , pure oneDay
        , pure oneWeek
        , pure oneMonth
        , pure oneYear
        ]
   where
    confirmedHorizon = 2160 * 20 -- k blocks on mainnet
    oneDay = 3600 * 24
    oneWeek = oneDay * 7
    oneMonth = oneDay * 30
    oneYear = oneDay * 365

-- | Create a 'ContestationPeriod' from a 'NominalDiffTime'. This will fail if a
-- negative NominalDiffTime is provided and truncates to 1s if values < 1s are given.
fromNominalDiffTime :: MonadFail m => NominalDiffTime -> m ContestationPeriod
fromNominalDiffTime dt =
  if seconds > 0
    then pure . UnsafeContestationPeriod $ ceiling seconds
    else fail $ "fromNominalDiffTime: contestation period <= 0: " <> show dt
 where
  seconds :: Pico = realToFrac dt

toNominalDiffTime :: ContestationPeriod -> NominalDiffTime
toNominalDiffTime (UnsafeContestationPeriod s) =
  secondsToNominalDiffTime $ fromIntegral s

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
