module Hydra.Ledger.Cardano.TimeSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (classify, forAll, getPositive, (===))
import "base" Data.Fixed (Milli)
import "cardano-slotting" Cardano.Slotting.Slot (SlotNo (..))
import "cardano-slotting" Cardano.Slotting.Time (SlotLength, SystemStart (..), slotLengthFromMillisec)
import "hydra-tx" Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import "time" Data.Time.Clock.POSIX (posixSecondsToUTCTime)

spec :: Spec
spec = do
  prop "roundtrip slotNoToUTCTime and slotNoFromUTCTime" $
    forAll genSystemStart $ \systemStart ->
      forAll genSlotLength $ \slotLength ->
        forAll (SlotNo . getPositive <$> arbitrary) $ \slot ->
          slotNoFromUTCTime systemStart slotLength (slotNoToUTCTime systemStart slotLength slot) === slot

  prop "slotNoFromUTCTime works for any time" $
    forAll genSystemStart $ \systemStart ->
      forAll genSlotLength $ \slotLength ->
        forAll genUTCTime $ \t ->
          slotNoFromUTCTime systemStart slotLength t >= SlotNo 0
            & classify (t > getSystemStart systemStart) "after system start"
            & classify (t == getSystemStart systemStart) "equal to system start"
            & classify (t < getSystemStart systemStart) "before system start"

-- | Generate a UTCTime using millisecond precision time since epoch.
genUTCTime :: Gen UTCTime
genUTCTime =
  posixSecondsToUTCTime . realToFrac <$> (arbitrary :: Gen Milli)

genSlotLength :: Gen SlotLength
genSlotLength =
  slotLengthFromMillisec . getPositive <$> arbitrary

genSystemStart :: Gen SystemStart
genSystemStart =
  SystemStart <$> genUTCTime
