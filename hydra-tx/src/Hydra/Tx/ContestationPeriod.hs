module Hydra.Tx.ContestationPeriod (module Hydra.Tx.ContestationPeriod, module Hydra.Tx.ContestationPeriod.Types) where

import Hydra.Prelude hiding (Show, show)

import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Tx.ContestationPeriod.Types
import Text.Show (Show (..))

-- | Convert an off-chain contestation period to its on-chain representation.
toChain :: ContestationPeriod -> OnChain.ContestationPeriod
toChain (UnsafeContestationPeriod s) =
  OnChain.UnsafeContestationPeriod
    . fromIntegral
    $ s * 1000

-- | Convert an on-chain contestation period to its off-chain representation,
-- accepting exactly the values 'toChain' can produce: a positive whole number of
-- seconds.
--
-- The on-chain representation is a signed number of milliseconds and nothing
-- constrains it, so an observed datum can carry a value this type cannot hold.
-- Rejecting rather than rounding matters beyond the obvious underflow: the node
-- decides whether to join a head by comparing this value against its own
-- configuration, and then writes 'toChain' of it back into the closing datum,
-- which the head validator requires to equal what the opening datum held. A
-- period that only survives the trip approximately would pass the comparison and
-- then fail that equality forever, leaving a head that can never be closed.
fromChain :: OnChain.ContestationPeriod -> Either Text ContestationPeriod
fromChain cp
  | subSecond == 0 && seconds > 0 = Right . UnsafeContestationPeriod $ fromInteger seconds
  | otherwise =
      Left $
        "contestation period is not a positive whole number of seconds: "
          <> toText (show milliseconds)
          <> "ms"
 where
  milliseconds = toInteger (OnChain.milliseconds cp)

  (seconds, subSecond) = milliseconds `divMod` 1000
