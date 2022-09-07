module Hydra.Chain.Direct.TimeHandleSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Chain.Direct.TimeHandle (TimeHandle (..))
import Test.QuickCheck (counterexample, forAllBlind, property, (===))

spec :: Spec
spec =
  prop "can roundtrip currentPointInTime" $
    forAllBlind arbitrary $ \TimeHandle{currentPointInTime, slotToUTCTime, slotFromUTCTime} ->
      let onLeft err = property False & counterexample ("Conversion failed: " <> toString err)
       in either onLeft id $ do
            (slot, _) <- currentPointInTime
            res <- slotFromUTCTime =<< slotToUTCTime slot
            pure $ res === slot
