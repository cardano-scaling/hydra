module Hydra.Tx.DepositPeriod.Types where

import Hydra.Prelude hiding (Show, show)

import Text.Show (Show (..))

-- | A non-negative duration used as the deposit validity window.
-- Nodes within the same Head must configure identical values. Use
-- 'Hydra.Tx.DepositPeriod.fromNominalDiffTime' to validate configured durations.
--
-- NOTE: Zero is allowed, unlike 'Hydra.Tx.ContestationPeriod': it means "no
-- margin", degenerate but workable, and it is what a sub-second period
-- truncates to. Sub-millisecond precision is not allowed by the smart constructor.
-- NOTE: 'FromJSON' is deliberately newtype-derived and therefore total, for the
-- same reason as 'Hydra.Tx.ContestationPeriod': it also decodes the persisted
-- event log. A configured period is validated at the configuration boundary
-- instead, see 'Hydra.Config.parseCardanoChainConfig'.
newtype DepositPeriod = DepositPeriod {toNominalDiffTime :: NominalDiffTime}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Read, Num, Real, ToJSON, FromJSON)

instance ToCBOR DepositPeriod where
  toCBOR = genericToCBOR

instance FromCBOR DepositPeriod where
  fromCBOR = genericFromCBOR

instance Show DepositPeriod where
  show (DepositPeriod dt) = show (round dt :: Integer) <> "s"
