module Hydra.Logging.AccountingSpec where

import Hydra.Prelude hiding (first, insert)
import Test.Hydra.Prelude hiding (pending)

import Data.Map.Strict qualified as Map
import Hydra.Logging.Monitoring.Accounting qualified as A

spec :: Spec
spec = do
  it "counts an input once and preserves its first processing time across retries" $ do
    let (first, _, s1) = A.request 10 3600 0 1 (42 :: Int) initial
        (retry, _, s2) = A.request 10 3600 0.5 1 42 s1
        (_, _, times, _) = A.confirmSnapshot 0.6 key1 [42] s2
    (first, retry, times) `shouldBe` (True, False, [0.6])

  it "counts externally duplicated messages but preserves their transaction's first time" $ do
    let (_, _, s1) = A.request 10 3600 0 1 (42 :: Int) initial
        (fresh, _, s2) = A.request 10 3600 0.5 2 42 s1
    fresh `shouldBe` True
    A.timers s2 `shouldBe` Map.singleton 42 0

  it "bounds both timer indexes and expires entries without more traffic" $ do
    let insert :: A.Accounting Text Int -> Int -> A.Accounting Text Int
        insert st i = let (_, _, next) = A.request 3 10 (fromIntegral i) (fromIntegral i) i st in next
        full = foldl' insert initial [1 .. 100]
        (removed, expired) = A.prune 3 10 111 full
    Map.keys (A.timers full) `shouldBe` [98, 99, 100]
    Map.size (A.orderedTimers full) `shouldBe` 3
    removed `shouldBe` 3
    A.timers expired `shouldBe` Map.empty
    A.orderedTimers expired `shouldBe` Map.empty

  it "removes invalid or dropped transactions from both indexes" $ do
    let (_, _, pending) = A.request 10 3600 0 1 (42 :: Int) initial
        next = A.forget 42 pending
    A.timers next `shouldBe` Map.empty
    A.orderedTimers next `shouldBe` Map.empty

  it "does not count repeated adoption or a version bump at the same number" $ do
    let (first, _, _, s1) = A.confirmSnapshot 1 key1 [42] initial
        (again, _, _, s2) = A.confirmSnapshot 2 key1 [42] s1
        (bumped, _, _, _) = A.confirmSnapshot 3 (A.SnapshotKey "head" 1 1) [42] s2
    (first, again, bumped) `shouldBe` (True, False, False)

  it "does not consume or time an unrelated in-flight snapshot when adopting an older one" $ do
    let pending = A.startSnapshot key2 (Just 1) [43] initial
        (_, duration, _, next) = A.confirmSnapshot 2 key1 [42] pending
    duration `shouldBe` Nothing
    A.inFlight next `shouldBe` Just (key2, Just 1, [43])

  it "does not match the same snapshot number in another head or version" $ do
    let pending = A.startSnapshot key1 (Just 1) [42] initial
        (_, duration, _, next) = A.confirmSnapshot 2 (A.SnapshotKey "other" 0 1) [42] pending
    duration `shouldBe` Nothing
    A.inFlight next `shouldBe` A.inFlight pending

  it "restores snapshot contents without inventing a duration" $ do
    let pending = A.startSnapshot key2 Nothing [43] initial{A.confirmed = Just key1}
        (advanced, duration, timings, next) = A.confirmSnapshot 500 key2 [43] pending
    (advanced, duration, timings) `shouldBe` (True, Nothing, [])
    A.inFlight next `shouldBe` Nothing

  it "clears pending state without forgetting the confirmed watermark" $ do
    let (_, _, pending) = A.request 10 3600 0 1 42 initial{A.confirmed = Just key1}
        cleared = A.clearPending $ A.startSnapshot key2 (Just 0) [42] pending
    A.confirmed cleared `shouldBe` Just key1
    A.inFlight cleared `shouldBe` Nothing
    Map.size (A.timers cleared) `shouldBe` 0
    Map.size (A.orderedTimers cleared) `shouldBe` 0
 where
  initial :: A.Accounting Text Int
  initial = A.empty
  key1 = A.SnapshotKey "head" 0 1
  key2 = A.SnapshotKey "head" 0 2
