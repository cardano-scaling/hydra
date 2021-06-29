{-# LANGUAGE TypeApplications #-}

module Hydra.API.ServerSpec where

import Control.Exception (IOException)
import Control.Monad.Class.MonadSTM (
  check,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  tryReadTQueue,
  writeTQueue,
 )
import qualified Data.Aeson as Aeson
import Hydra.API.Server (withAPIServer)
import Hydra.HeadLogic (ServerOutput (..))
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.Network.Ports (withFreePort)
import Hydra.Prelude
import Network.WebSockets ( runClient, sendBinaryData )
import Network.WebSockets.Connection (receiveData)
import Test.Hspec
import Test.Util (failAfter, failure)

spec :: Spec
spec = describe "API Server" $ do
  it "sends sendOutput to all connected clients" $ do
    queue <- atomically newTQueue
    failAfter 5 $
      withFreePort $ \port ->
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) nullTracer noop $ \sendOutput -> do
          semaphore <- newTVarIO 0
          withAsync (concurrently_ (testClient port queue semaphore) (testClient port queue semaphore)) $ \_ -> do
            atomically $ readTVar semaphore >>= \n -> check (n == 2)
            let arbitraryMsg = ReadyToCommit []
            sendOutput arbitraryMsg

            atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [arbitraryMsg, arbitraryMsg]
            atomically (tryReadTQueue queue) `shouldReturn` Nothing

  it "sends an error when input cannot be decoded" $
    failAfter 5 $
      withFreePort $ \port -> sendsAnErrorWhenInputCannotBeDecoded port

sendsAnErrorWhenInputCannotBeDecoded :: Int -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) nullTracer noop $ \_sendOutput -> do
    tryClient `shouldReturn` InvalidInput invalidInput
 where
  tryClient :: IO (ServerOutput SimpleTx)
  tryClient =
    runClient
      "127.0.0.1"
      port
      "/"
      ( \con -> do
          sendBinaryData @Text con invalidInput
          msg <- receiveData con
          case Aeson.eitherDecode msg of
            Right resp -> pure resp
            Left{} -> failure $ "Failed to decode output " <> show msg
      )
      `catch` \(_ :: IOException) -> tryClient

  invalidInput = "not a valid message"

noop :: Applicative m => a -> m ()
noop = const $ pure ()

testClient :: HasCallStack => Int -> TQueue IO (ServerOutput SimpleTx) -> TVar IO Int -> IO ()
testClient port queue semaphore =
  failAfter 5 tryClient
 where
  tryClient =
    runClient
      "127.0.0.1"
      port
      "/"
      ( \cnx -> do
          atomically $ modifyTVar' semaphore (+ 1)
          msg <- receiveData cnx
          case Aeson.eitherDecode msg of
            Right resp -> atomically (writeTQueue queue resp)
            Left{} -> expectationFailure ("Failed to decode message " <> show msg)
      )
      `catch` \(_ :: IOException) -> tryClient
