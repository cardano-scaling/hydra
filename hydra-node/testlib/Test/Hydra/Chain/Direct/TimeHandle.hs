{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Chain.Direct.TimeHandle where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (getPositive)
import "cardano-slotting" Cardano.Slotting.Slot (SlotNo (SlotNo))
import "cardano-slotting" Cardano.Slotting.Time (SystemStart (..))
import "hydra-tx" Hydra.Ledger.Cardano.Evaluate (eraHistoryWithHorizonAt)
import "time" Data.Time (secondsToNominalDiffTime)
import "time" Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Hydra.Chain.Direct.TimeHandle

-- | Generate consistent values for 'SystemStart' and 'EraHistory' which has
-- a horizon at the returned SlotNo as well as some UTCTime before that
genTimeParams :: Gen TimeHandleParams
genTimeParams = do
  startSeconds <- getPositive <$> arbitrary
  let startTime = posixSecondsToUTCTime $ secondsToNominalDiffTime startSeconds
  uptimeSeconds <- getPositive <$> arbitrary
  -- it is ok to construct a slot from seconds here since on the devnet slot = 1s
  let currentSlotNo = SlotNo $ truncate $ uptimeSeconds + startSeconds
      horizonSlot = SlotNo $ truncate $ uptimeSeconds + safeZone
  pure $
    TimeHandleParams
      { systemStart = SystemStart startTime
      , eraHistory = eraHistoryWithHorizonAt horizonSlot
      , horizonSlot = horizonSlot
      , currentSlot = currentSlotNo
      }

instance Arbitrary TimeHandle where
  arbitrary = do
    TimeHandleParams{systemStart, eraHistory, currentSlot} <- genTimeParams
    pure $ mkTimeHandle currentSlot systemStart eraHistory
