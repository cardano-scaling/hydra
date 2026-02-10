-- | Module containing time conversion functions. These functions are assuming
-- that there is never going to be a different slot or epoch length and project
-- a slot/time accordingly.
--
-- See Hydra.Chain.Direct.TimeHandle for an alternative that uses the
-- cardano-node provided 'EraSummary' to do "correct" translation on time.
module Hydra.Ledger.Cardano.Time where

import Hydra.Prelude

import "cardano-slotting" Cardano.Slotting.Slot (SlotNo (..))
import "cardano-slotting" Cardano.Slotting.Time (RelativeTime (..), SlotLength, SystemStart, getSlotLength, getSystemStart, toRelativeTime)

-- | Convert a 'SlotNo' to a 'UTCTime' using given 'SystemStart' and
-- 'SlotLength'. This assumes the slot length never changes!
slotNoToUTCTime :: SystemStart -> SlotLength -> SlotNo -> UTCTime
slotNoToUTCTime systemStart slotLength slotNo =
  (numberOfSlots * slotDuration) `addUTCTime` startTime
 where
  startTime = getSystemStart systemStart

  slotDuration = getSlotLength slotLength

  numberOfSlots = fromIntegral $ unSlotNo slotNo

-- | Convert a 'UTCTime' to a 'SlotNo' using given 'SystemStart' and
-- 'SlotLength'. This assumes the slot length never changes! Also if the UTCTime
-- is before the systemStart it will truncate to slot 0.
slotNoFromUTCTime :: SystemStart -> SlotLength -> UTCTime -> SlotNo
slotNoFromUTCTime systemStart slotLength utcTime =
  SlotNo $ truncate (relativeTime / getSlotLength slotLength)
 where
  (RelativeTime relativeTime) =
    toRelativeTime systemStart utcTime
