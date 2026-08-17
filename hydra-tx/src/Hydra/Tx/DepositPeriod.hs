module Hydra.Tx.DepositPeriod where

import Hydra.Prelude hiding (Show, show)

import Hydra.Data.DepositPeriod qualified as OnChain
import Text.Show (Show (..))

-- | A positive duration used as the deposit validity window.
-- Nodes within the same Head must configure identical values.
newtype DepositPeriod = DepositPeriod {toNominalDiffTime :: NominalDiffTime}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Read, Num, Real, ToJSON, FromJSON)

instance ToCBOR DepositPeriod where
  toCBOR = genericToCBOR

instance FromCBOR DepositPeriod where
  fromCBOR = genericFromCBOR

instance Show DepositPeriod where
  show (DepositPeriod dt) = show (round dt :: Integer) <> "s"

-- | Convert an off-chain deposit period to its on-chain representation.
toChain :: DepositPeriod -> OnChain.DepositPeriod
toChain (DepositPeriod dt) = OnChain.depositPeriodFromDiffTime dt

-- | Convert an on-chain deposit period to its off-chain representation.
-- NOTE: Truncates to whole milliseconds.
fromChain :: OnChain.DepositPeriod -> DepositPeriod
fromChain = DepositPeriod . OnChain.depositPeriodToDiffTime
