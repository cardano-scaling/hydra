module Hydra.Ledger.Cardano.TimeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Slotting.Time (SlotLength, SystemStart (..), slotLengthFromMillisec)
import Data.Fixed (Milli)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Test.QuickCheck (classify, forAll, getPositive, (===))

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
