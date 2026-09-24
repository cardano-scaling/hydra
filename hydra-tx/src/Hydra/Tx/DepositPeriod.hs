module Hydra.Tx.DepositPeriod where

import Hydra.Prelude hiding (Show, show)

import Hydra.Data.DepositPeriod qualified as OnChain
import Text.Show (Show (..))

-- | A non-negative duration used as the deposit validity window.
-- Nodes within the same Head must configure identical values. Use
-- 'fromNominalDiffTime' to create values of unknown sign.
--
-- NOTE: Zero is allowed, unlike 'Hydra.Tx.ContestationPeriod': it means "no
-- margin", degenerate but workable, and it is what a sub-second period
-- truncates to. Sub-millisecond precision is not, see 'fromNominalDiffTime'.
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

-- | Create a 'DepositPeriod' from a 'NominalDiffTime', accepting exactly the
-- durations that survive the on-chain encoding: a non-negative whole number of
-- milliseconds.
--
-- A negative duration would invert the deposit window. A finer-grained one would
-- be written to the datum as a different period than the one configured, and the
-- initiator would then not recognise its own head, consuming the seed and
-- stranding the head output with no way to abort.
fromNominalDiffTime :: MonadFail m => NominalDiffTime -> m DepositPeriod
fromNominalDiffTime dt
  | dt < 0 =
      fail $ "fromNominalDiffTime: deposit period < 0: " <> show dt
  | OnChain.depositPeriodToDiffTime (OnChain.depositPeriodFromDiffTime dt) /= dt =
      fail $ "fromNominalDiffTime: deposit period is not a whole number of milliseconds: " <> show dt
  | otherwise = pure $ DepositPeriod dt

-- | Convert an off-chain deposit period to its on-chain representation.
toChain :: DepositPeriod -> OnChain.DepositPeriod
toChain (DepositPeriod dt) = OnChain.depositPeriodFromDiffTime dt

-- | Convert an on-chain deposit period to its off-chain representation.
-- The on-chain representation is a signed number of milliseconds with no sign
-- constraint, so an observed datum can carry a negative value, which would
-- invert the deposit window; such a period is rejected.
-- NOTE: Truncates to whole milliseconds.
fromChain :: OnChain.DepositPeriod -> Either Text DepositPeriod
fromChain dp
  | dt >= 0 = Right $ DepositPeriod dt
  | otherwise = Left $ "deposit period is negative: " <> toText (show (toInteger (OnChain.milliseconds dp))) <> "ms"
 where
  dt = OnChain.depositPeriodToDiffTime dp
