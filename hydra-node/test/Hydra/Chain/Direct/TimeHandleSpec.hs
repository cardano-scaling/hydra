module Hydra.Chain.Direct.TimeHandleSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Slot (SlotNo (SlotNo))
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.Direct.TimeHandle (TimeHandle (..), TimeHandleParams (..), mkTimeHandle, newTimeHandleCache, slotFromUTCTimeWith, slotToUTCTimeWith)
import Test.Hydra.Chain.Direct.TimeHandle (genTimeParams)
import Test.Hydra.Ledger.Cardano.Fixtures (eraHistoryWithHorizonAt, eraHistoryWithoutHorizon)
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

  prop "slotToUTCTimeWith/slotFromUTCTimeWith roundtrip within the horizon" $
    forAllBlind genTimeParams $ \TimeHandleParams{systemStart, eraHistory, currentSlot} ->
      (slotFromUTCTimeWith systemStart eraHistory =<< slotToUTCTimeWith systemStart eraHistory currentSlot)
        === Right currentSlot

  prop "slotToUTCTimeWith fails past the horizon" $
    forAllBlind genTimeParams $ \TimeHandleParams{systemStart, eraHistory, horizonSlot} ->
      isLeft (slotToUTCTimeWith systemStart eraHistory (horizonSlot + 1))

  describe "newTimeHandleCache" $ do
    it "queries system start and era history only once" $ do
      systemStartQueries <- newIORef (0 :: Int)
      eraHistoryQueries <- newIORef (0 :: Int)
      now <- getCurrentTime
      getTimeHandle <-
        newTimeHandleCache
          (modifyIORef' systemStartQueries (+ 1) $> SystemStart now)
          (modifyIORef' eraHistoryQueries (+ 1) $> eraHistoryWithoutHorizon)
      replicateM_ 5 getTimeHandle
      readIORef systemStartQueries `shouldReturn` 1
      readIORef eraHistoryQueries `shouldReturn` 1

    it "refreshes era history when the wall clock is past the horizon" $ do
      eraHistoryQueries <- newIORef (0 :: Int)
      getTimeHandle <-
        newTimeHandleCache
          (pure . SystemStart $ posixSecondsToUTCTime 0)
          ( do
              n <- readIORef eraHistoryQueries
              modifyIORef' eraHistoryQueries (+ 1)
              pure $ if n == 0 then eraHistoryWithHorizonAt 1 else eraHistoryWithoutHorizon
          )
      TimeHandle{currentPointInTime} <- getTimeHandle
      readIORef eraHistoryQueries `shouldReturn` 2
      currentPointInTime `shouldSatisfy` isRight

    it "degrades gracefully when refreshing does not extend the horizon" $ do
      eraHistoryQueries <- newIORef (0 :: Int)
      getTimeHandle <-
        newTimeHandleCache
          (pure . SystemStart $ posixSecondsToUTCTime 0)
          (modifyIORef' eraHistoryQueries (+ 1) $> eraHistoryWithHorizonAt 1)
      TimeHandle{currentPointInTime, slotToUTCTime} <- getTimeHandle
      readIORef eraHistoryQueries `shouldReturn` 2
      currentPointInTime `shouldSatisfy` isLeft
      slotToUTCTime 0 `shouldBe` Right (posixSecondsToUTCTime 0)
