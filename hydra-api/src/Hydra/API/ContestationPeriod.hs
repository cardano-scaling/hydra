module Hydra.API.ContestationPeriod where

import Control.Monad.Class.MonadTime (NominalDiffTime)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (secondsToNominalDiffTime)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary, arbitrary, choose, oneof)

-- | A positive, non-zero number of seconds.
newtype ContestationPeriod = UnsafeContestationPeriod Natural
  deriving stock (Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

instance Show ContestationPeriod where
  show (UnsafeContestationPeriod s) = show s <> "s"

instance Arbitrary ContestationPeriod where
  arbitrary = do
    UnsafeContestationPeriod . fromInteger
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

toNominalDiffTime :: ContestationPeriod -> NominalDiffTime
toNominalDiffTime (UnsafeContestationPeriod s) =
  secondsToNominalDiffTime $ fromIntegral s
