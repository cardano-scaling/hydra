module Hydra.Chain.Direct.TimeHandleSpec where

import Hydra.Prelude hiding (label)

import Cardano.Ledger.Slot (SlotNo (SlotNo))
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..), mkTimeHandle)
import Hydra.Ledger.Cardano.Evaluate (eraHistoryWithHorizonAt)
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, counterexample, forAllBlind, property, (===))

spec :: Spec
spec = do
  prop "can roundtrip currentPointInTime" $
    forAllBlind arbitrary $ \TimeHandle{currentPointInTime, slotToUTCTime, slotFromUTCTime} ->
      let onLeft :: Text -> Property
          onLeft err = property False & counterexample ("Conversion failed: " <> toString err)
       in either onLeft id $ do
            (slot, _) <- currentPointInTime
            res <- slotFromUTCTime =<< slotToUTCTime slot
            pure $ res === slot

  it "should convert slot within latest/current era" $ do
    let currentSlotNo = SlotNo 13
        systemStart = SystemStart $ posixSecondsToUTCTime 0
        eraHistory = eraHistoryWithHorizonAt 15
        timeHandle = mkTimeHandle currentSlotNo systemStart eraHistory
        slotInside = SlotNo 14
        converted = slotToUTCTime timeHandle slotInside
        expected :: Either Text UTCTime
        expected = Right $ posixSecondsToUTCTime 14
    converted `shouldBe` expected
