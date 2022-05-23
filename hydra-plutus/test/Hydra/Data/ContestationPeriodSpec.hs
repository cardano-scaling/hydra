module Hydra.Data.ContestationPeriodSpec where

import Hydra.Prelude

import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Plutus.Orphans ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (tabulate, (===))
import Test.QuickCheck.Property (coverTable)

spec :: Spec
spec = describe "posixToUTCTime" $ do
  prop "is homomorphic w.r.t to Ord" $ \t1 t2 ->
    let ordering = compare t1 t2
     in ordering === compare (posixToUTCTime t1) (posixToUTCTime t2)
          & tabulate "Ord" (map show $ enumFrom LT)
          & coverTable "Cover" [("LT", 33), ("EQ", 33), ("GT", 33)]
