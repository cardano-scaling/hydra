module Hydra.Chain.BlockfrostSpec where

import Hydra.Prelude
import Test.Hspec

import Control.Tracer (nullTracer)
import Hydra.Chain.Blockfrost (
  APIBlockfrostError (..),
  isRetryable,
  retryOnBlockfrostError,
 )
import Hydra.Chain.Direct.Handlers (CardanoChainLog)
import Hydra.Logging (Tracer)

spec :: Spec
spec = do
  describe "isRetryable" $ do
    it "treats DecodeError as retryable" $ do
      isRetryable (DecodeError "some decode error") `shouldBe` True

    it "treats BlockfrostError as retryable" $ do
      isRetryable (BlockfrostError "some API error") `shouldBe` True

  describe "retryOnBlockfrostError" $ do
    it "retries on transient APIBlockfrostError and eventually succeeds" $ do
      attemptsRef <- newIORef (0 :: Int)
      result <-
        retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) 3 $ do
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
      retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) 3 action
        `shouldThrow` \case
          BlockfrostError{} -> True
          _ -> False
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 3
