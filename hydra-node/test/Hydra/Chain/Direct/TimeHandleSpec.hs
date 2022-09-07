module Hydra.Chain.Direct.TimeHandleSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Test.QuickCheck (counterexample, forAllBlind, property, (===))

spec :: Spec
spec = do
  prop "can roundtrip currentPointInTime" $
    forAllBlind arbitrary $ \TimeHandle{currentPointInTime, slotToPOSIXTime, slotFromPOSIXTime} ->
      let failWith name = either (\err -> error (name <> " failed: " <> err)) id
       in do
            let (slot, pt) = failWith "currentPointInTime" currentPointInTime
            let posixTime = trace ("slot: " <> show slot <> ", pt: " <> show pt) failWith "slotToPOSIXTime" $ slotToPOSIXTime slot
            let resultSlot = trace ("posixTime: " <> show posixTime) failWith "slotFromPOSIXTime" $ slotFromPOSIXTime posixTime
            resultSlot === slot
