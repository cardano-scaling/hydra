-- | Tests of the 'PersistentQueue'.
module Hydra.PersistentQueueSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (check, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadAsync (concurrently, wait, withAsync)
import Hydra.Logging (Envelope (message), nullTracer, traceInTVar)
import Hydra.Network.Etcd (EtcdLog (..), newPersistentQueue, peekPersistentQueue, popPersistentQueue, writePersistentQueue)
import System.Directory (createDirectory, listDirectory, removeFile)
import System.FilePath ((</>))
import Test.QuickCheck (counterexample, generate, ioProperty)
import Test.QuickCheck.Instances.Natural ()

spec :: Spec
spec = do
  it "can be constructed" $ do
    capacity <- generate arbitrary
    withTempDir "persistent-queue" $ \dir -> do
      void $ newPersistentQueue @_ @Int nullTracer dir capacity

  prop "is persistent with capacity" $ \(items :: [Int]) -> do
    let capacity = fromIntegral $ length items
    counterexample ("capacity: " <> show capacity) $
      ioProperty $
        withTempDir "persistent-queue" $ \dir -> do
          q <- newPersistentQueue nullTracer dir capacity
          shouldNotBlock_ $ mapM (writePersistentQueue nullTracer q) items
          -- This is expected to block as we reached capacity
          _ <- timeout 0.01 (writePersistentQueue nullTracer q 123)
          -- A new queue should be initialized with all the elements
          q2 <- shouldNotBlock $ newPersistentQueue @_ @Int nullTracer dir capacity
          let expected = maybe 123 head (nonEmpty items)
          peekPersistentQueue q2 `shouldReturn` expected

  it "pop unblocks a blocked writer when at capacity" $ do
    withTempDir "persistent-queue" $ \dir -> do
      traces <- newTVarIO []
      let tracer = traceInTVar traces "PersistentQueueSpec"
      q <- newPersistentQueue tracer dir 1
      writePersistentQueue tracer q (1 :: Int)
      withAsync (writePersistentQueue tracer q 2) $ \writer -> do
        -- The writer traces 'PersistentQueueFull' right before blocking on
        -- the full queue, so this doubles as the signal it reached that point.
        shouldNotBlock_ . atomically $ do
          entries <- readTVar traces
          check $ PersistentQueueFull `elem` (message <$> entries)
        popPersistentQueue tracer q
        shouldNotBlock_ $ wait writer
      peekPersistentQueue q `shouldReturn` 2

  it "does not deadlock when writing beyond capacity with a concurrent consumer" $ do
    withTempDir "persistent-queue" $ \dir -> do
      q <- newPersistentQueue nullTracer dir 10
      let items = [1 .. 100 :: Int]
      (_, received) <-
        shouldNotBlock $
          concurrently
            (forM_ items $ writePersistentQueue nullTracer q)
            ( forM items $ \_ -> do
                item <- peekPersistentQueue q
                popPersistentQueue nullTracer q
                pure item
            )
      received `shouldBe` items
      listDirectory dir `shouldReturn` []

  it "pop removes the head item from memory and disk" $ do
    withTempDir "persistent-queue" $ \dir -> do
      q <- newPersistentQueue nullTracer dir 10
      forM_ [1, 2, 3 :: Int] $ writePersistentQueue nullTracer q
      peekPersistentQueue q `shouldReturn` 1
      popPersistentQueue nullTracer q
      peekPersistentQueue q `shouldReturn` 2
      -- A popped item must not be reloaded on restart (it was already
      -- broadcast and would be duplicated)
      q2 <- shouldNotBlock $ newPersistentQueue @_ @Int nullTracer dir 10
      peekPersistentQueue q2 `shouldReturn` 2

  it "pop tolerates a missing backing file" $ do
    withTempDir "persistent-queue" $ \dir -> do
      q <- newPersistentQueue nullTracer dir 10
      writePersistentQueue nullTracer q (1 :: Int)
      files <- listDirectory dir
      forM_ files $ \f -> removeFile (dir </> f)
      popPersistentQueue nullTracer q

  it "pop survives and traces a failing backing file deletion" $ do
    withTempDir "persistent-queue" $ \dir -> do
      traces <- newTVarIO []
      let tracer = traceInTVar traces "PersistentQueueSpec"
      q <- newPersistentQueue tracer dir 10
      writePersistentQueue tracer q (1 :: Int)
      -- Replace the backing file with a same-named directory so removeFile
      -- fails with EISDIR instead of ENOENT (works even when running as root)
      [file] <- listDirectory dir
      removeFile (dir </> file)
      createDirectory (dir </> file)
      writePersistentQueue tracer q 2
      popPersistentQueue tracer q
      entries <- readTVarIO traces
      (message <$> entries) `shouldSatisfy` any isDeleteFailed
      -- The consumer keeps making progress
      peekPersistentQueue q `shouldReturn` 2

  it "traces PersistentQueueLoadFailed on corrupt items and starts empty" $ do
    withTempDir "persistent-queue" $ \dir -> do
      writeFileBS (dir </> "1") "not-valid-cbor"
      traces <- newTVarIO []
      let tracer = traceInTVar traces "PersistentQueueSpec"
      q <- newPersistentQueue @_ @Int tracer dir 10
      entries <- readTVarIO traces
      (message <$> entries) `shouldSatisfy` any isLoadFailed
      -- The queue starts empty but stays functional
      writePersistentQueue tracer q 42
      peekPersistentQueue q `shouldReturn` 42

isDeleteFailed :: EtcdLog -> Bool
isDeleteFailed = \case
  PersistentQueueDeleteFailed{} -> True
  _ -> False

isLoadFailed :: EtcdLog -> Bool
isLoadFailed = \case
  PersistentQueueLoadFailed{} -> True
  _ -> False

shouldNotBlock :: HasCallStack => IO a -> IO a
shouldNotBlock action = do
  -- Generous enough to survive heavy parallel load on CI; the goal is to
  -- detect a hung action, not benchmark responsiveness.
  timeout 5 action >>= \case
    Nothing -> failure "blocked unexpectedly"
    Just a -> pure a

shouldNotBlock_ :: HasCallStack => IO a -> IO ()
shouldNotBlock_ = shouldNotBlock . void
