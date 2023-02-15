module Hydra.ContestationPeriod (
  module Hydra.API.ContestationPeriod,
  toChain,
  fromChain,
) where

import Hydra.Prelude hiding (Show, show)

import Data.Ratio ((%))
import Hydra.API.ContestationPeriod (ContestationPeriod (..), toNominalDiffTime)
import qualified Hydra.Data.ContestationPeriod as OnChain

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
