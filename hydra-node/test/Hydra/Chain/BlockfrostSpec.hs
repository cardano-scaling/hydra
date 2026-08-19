module Hydra.Chain.BlockfrostSpec where

import Hydra.Prelude
import Test.Hspec

import Control.Concurrent.Class.MonadSTM (takeTMVar, writeTQueue)
import Control.Retry (RetryPolicyM, limitRetries)
import Control.Tracer (nullTracer)
import Hydra.Chain.Blockfrost (blockfrostSubmissionClient, retryOnBlockfrostError)
import Hydra.Chain.Blockfrost.Client (APIBlockfrostError (..), BlockfrostException (..), TxHash (..), isRetryable)
import Hydra.Chain.Direct.Handlers (CardanoChainLog)
import Hydra.Logging (Tracer)
import Test.Hydra.Prelude (failAfter)
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck (arbitrary, generate)

retry :: RetryPolicyM IO
retry = limitRetries 3

spec :: Spec
spec = do
  describe "isRetryable" $ do
    it "treats DecodeError as retryable" $ do
      isRetryable (DecodeError "some decode error") `shouldBe` True

    it "treats BlockfrostError as retryable" $ do
      isRetryable (BlockfrostError "some API error") `shouldBe` True

    it "treats BlockfrostRateLimited as retryable" $ do
      isRetryable BlockfrostRateLimited `shouldBe` True

  describe "retryOnBlockfrostError" $ do
    it "retries on transient APIBlockfrostError and eventually succeeds" $ do
      attemptsRef <- newIORef (0 :: Int)
      result <-
        retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) retry $ const $ do
          attempts <- readIORef attemptsRef
          writeIORef attemptsRef (attempts + 1)
          if attempts < 2
            then throwIO $ BlockfrostError "transient error"
            else pure ("success" :: Text)
      result `shouldBe` "success"
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 3

    it "gives up after max retries" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            modifyIORef attemptsRef (+ 1)
            throwIO $ BlockfrostError "persistent error"
      retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) retry (const action)
        `shouldThrow` \case
          BlockfrostError{} -> True
          _ -> False
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 4

    it "retries on HTTP error (BlockfrostError Text) and eventually succeeds" $ do
      attemptsRef <- newIORef (0 :: Int)
      result <-
        retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) retry $ const $ do
          attempts <- readIORef attemptsRef
          modifyIORef attemptsRef (+ 1)
          if attempts < 2
            then throwIO $ BlockfrostError "HTTP 403 Forbidden"
            else pure ("success" :: Text)
      result `shouldBe` "success"
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 3

    it "gives up after max retries on persistent HTTP error" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            modifyIORef attemptsRef (+ 1)
            throwIO $ BlockfrostError "HTTP 403 Forbidden"
      retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) retry (const action)
        `shouldThrow` \case
          BlockfrostError _ -> True
          _ -> False
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 4

    it "does not retry on BlockfrostClientError" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            modifyIORef attemptsRef (+ 1)
            throwIO $ BlockfrostClientError ByronAddressNotSupported
      retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) retry (const action)
        `shouldThrow` \case
          BlockfrostClientError{} -> True
          _ -> False
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 1

  describe "blockfrostSubmissionClient" $
    it "reports submission failures immediately and keeps serving the queue" $
      failAfter 5 $ do
        queue <- newLabelledTQueueIO "test-submission-queue"
        calls <- newIORef (0 :: Int)
        let submit _tx = do
              n <- atomicModifyIORef' calls $ \c -> (c + 1, c)
              pure $
                if n == 0
                  then Left "submission failed"
                  else Right (TxHash "deadbeef")
        tx1 <- generate arbitrary
        tx2 <- generate arbitrary
        withAsyncLabelled ("blockfrost-submit", blockfrostSubmissionClient (nullTracer :: Tracer IO CardanoChainLog) submit queue) $ \_ -> do
          res1 <- postViaQueue queue tx1
          res1 `shouldSatisfy` isJust
          res2 <- postViaQueue queue tx2
          res2 `shouldSatisfy` isNothing
 where
  postViaQueue :: forall m a b. MonadLabelledSTM m => TQueue m (a, TMVar m b) -> a -> m b
  postViaQueue queue tx = do
    response <- atomically $ do
      r <- newLabelledEmptyTMVar "test-response"
      writeTQueue queue (tx, r)
      pure r
    atomically $ takeTMVar response
