module Hydra.Tx.DepositPeriod where

import Hydra.Prelude hiding (Show, show)

import Data.Ratio ((%))
import Data.Time (secondsToNominalDiffTime)
import Hydra.Data.DepositPeriod qualified as OnChain
import Text.Show (Show (..))

-- | A positive duration used as the deposit validity window.
-- Nodes within the same Head must configure identical values.
newtype DepositPeriod = DepositPeriod {toNominalDiffTime :: NominalDiffTime}
  deriving stock (Eq, Ord)
  deriving newtype (Read, Num, Enum, Real, ToJSON, FromJSON)

instance Show DepositPeriod where
  show (DepositPeriod dt) = show (round dt :: Integer) <> "s"

instance Integral DepositPeriod where
  quotRem (DepositPeriod a) (DepositPeriod b) = (DepositPeriod $ fromInteger q, DepositPeriod r)
   where
    (q, r) = properFraction (a / b)

  toInteger (DepositPeriod a) = round a

-- | Convert an off-chain deposit period to its on-chain representation.
toChain :: DepositPeriod -> OnChain.DepositPeriod
toChain (DepositPeriod dt) = OnChain.depositPeriodFromDiffTime dt

-- | Convert an on-chain deposit period to its off-chain representation.
-- NOTE: Truncates to whole milliseconds.
fromChain :: OnChain.DepositPeriod -> DepositPeriod
fromChain dp =
  DepositPeriod
    . secondsToNominalDiffTime
    $ fromRational (toInteger (OnChain.milliseconds dp) % 1000)
