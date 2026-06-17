module Hydra.Chain.BlockfrostSpec where

import Hydra.Prelude
import Test.Hspec

import Control.Tracer (nullTracer)
import Hydra.Chain.Blockfrost (
  APIBlockfrostError (..),
  isRetryable,
  retryOnBlockfrostError,
 )
import Hydra.Chain.Blockfrost.Client qualified as BlockfrostClient
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
        retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) 3 $ const $ do
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
      retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) 3 (const action)
        `shouldThrow` \case
          BlockfrostError{} -> True
          _ -> False
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 4

    it "retries on Client.APIBlockfrostError (HTTP 403) and eventually succeeds" $ do
      attemptsRef <- newIORef (0 :: Int)
      result <-
        retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) 3 $ const $ do
          attempts <- readIORef attemptsRef
          modifyIORef attemptsRef (+ 1)
          if attempts < 2
            then throwIO $ BlockfrostClient.BlockfrostError (BlockfrostClient.BlockfrostAPIError "HTTP 403 Forbidden")
            else pure ("success" :: Text)
      result `shouldBe` "success"
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 3

    it "gives up after max retries on persistent Client.APIBlockfrostError" $ do
      attemptsRef <- newIORef (0 :: Int)
      let action = do
            modifyIORef attemptsRef (+ 1)
            throwIO $ BlockfrostClient.BlockfrostError (BlockfrostClient.BlockfrostAPIError "HTTP 403 Forbidden")
      retryOnBlockfrostError (nullTracer :: Tracer IO CardanoChainLog) 3 (const action)
        `shouldThrow` \case
          BlockfrostClient.BlockfrostError (BlockfrostClient.BlockfrostAPIError _) -> True
          _ -> False
      finalAttempts <- readIORef attemptsRef
      finalAttempts `shouldBe` 4
