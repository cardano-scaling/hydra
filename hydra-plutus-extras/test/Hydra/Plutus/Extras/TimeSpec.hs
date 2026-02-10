module Hydra.Plutus.Extras.TimeSpec where

import "hydra-prelude" Hydra.Prelude
import "QuickCheck" Test.QuickCheck (Positive (Positive), collect, (===))
import "base" Data.Fixed (Milli)
import "hspec" Test.Hspec (Spec, describe)
import "hspec" Test.Hspec.QuickCheck (prop)
import "time" Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Hydra.Plutus.Extras.Time (posixFromUTCTime, posixToUTCTime)
import Hydra.Plutus.Orphans ()

spec :: Spec
spec = do
  describe "posixToUTCTime" $ do
    prop "is homomorphic w.r.t to Ord" $ \t1 t2 ->
      let ordering = compare t1 t2
       in ordering
            === compare (posixToUTCTime t1) (posixToUTCTime t2)
            & collect ordering

  prop "roundtrip posixToUTCTime . posixFromUTCTime" $ \(Positive t) ->
    posixFromUTCTime (posixToUTCTime t) === t

  prop "roundtrip posixFromUTCTime . posixToUTCTime (up to millisecond precision)" $ \(s :: Milli) ->
    let t = posixSecondsToUTCTime $ realToFrac s
     in posixToUTCTime (posixFromUTCTime t) === t
