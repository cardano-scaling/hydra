-- | Tests of the 'PersistentQueue'.
module Hydra.PersistentQueueSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Network.Etcd (newPersistentQueue, peekPersistentQueue, writePersistentQueue)
import Test.QuickCheck (counterexample, generate, ioProperty)

spec :: Spec
spec = do
  it "can be constructed" $ do
    capacity <- generate arbitrary
    withTempDir "persistent-queue" $ \dir -> do
      void $ newPersistentQueue @_ @Int dir capacity

  prop "is persistent with capacity" $ \(items :: [Int]) -> do
    let capacity = fromIntegral $ length items
    counterexample ("capacity: " <> show capacity) $
      ioProperty $
        withTempDir "persistent-queue" $ \dir -> do
          q <- newPersistentQueue dir capacity
          shouldNotBlock_ $ mapM (writePersistentQueue q) items
          -- This is expected to block as we reached capacity
          _ <- timeout 0.01 (writePersistentQueue q 123)
          -- A new queue should be initialized with all the elements
          q2 <- shouldNotBlock $ newPersistentQueue @_ @Int dir capacity
          let expected = maybe 123 head (nonEmpty items)
          peekPersistentQueue q2 `shouldReturn` expected

shouldNotBlock :: HasCallStack => IO a -> IO a
shouldNotBlock action = do
  timeout 0.1 action >>= \case
    Nothing -> failure "blocked unexpectedly"
    Just a -> pure a

shouldNotBlock_ :: HasCallStack => IO a -> IO ()
shouldNotBlock_ = shouldNotBlock . void
