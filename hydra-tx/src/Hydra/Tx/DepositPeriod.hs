module Hydra.Tx.DepositPeriod (module Hydra.Tx.DepositPeriod, module Hydra.Tx.DepositPeriod.Types) where

import Hydra.Prelude hiding (Show, show)

import Hydra.Data.DepositPeriod qualified as OnChain
import Hydra.Tx.DepositPeriod.Types
import Text.Show (Show (..))

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
