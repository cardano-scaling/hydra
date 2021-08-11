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
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Test.Network.Ports (withFreePort)
import Hydra.Prelude
import Hydra.ServerOutput (ServerOutput (InvalidInput, ReadyToCommit))
import Test.Hydra.Prelude (failAfter, failure)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (cover)
import Test.QuickCheck.Monadic (monadicIO, monitor, run)

spec :: Spec
spec = describe "API Server" $ do
  it "sends sendOutput to all connected clients" $ do
    queue <- atomically newTQueue
    failAfter 5 $
      withFreePort $ \port ->
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) nullTracer noop $ \sendOutput -> do
          semaphore <- newTVarIO 0
          withAsync
            ( concurrently_
                (withClient port $ testClient queue semaphore)
                (withClient port $ testClient queue semaphore)
            )
            $ \_ -> do
              atomically $ readTVar semaphore >>= \n -> check (n == 2)
              let arbitraryMsg = ReadyToCommit []
              sendOutput arbitraryMsg

              atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [arbitraryMsg, arbitraryMsg]
              atomically (tryReadTQueue queue) `shouldReturn` Nothing

  prop "echoes history (past outputs) to client upon reconnection" $ \msgs -> monadicIO $ do
    monitor $ cover 1 (null msgs) "no message when reconnecting"
    monitor $ cover 1 (length msgs == 1) "only one message when reconnecting"
    monitor $ cover 1 (length msgs > 1) "more than one message when reconnecting"
    run . failAfter 5 $ do
      withFreePort $ \port ->
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) nullTracer noop $ \sendOutput -> do
          mapM_ sendOutput (msgs :: [ServerOutput SimpleTx])
          withClient port $ \conn -> do
            received <- replicateM (length msgs) (receiveData conn)
            case traverse Aeson.eitherDecode received of
              Right msgs' -> msgs' `shouldBe` msgs
              Left{} -> failure $ "Failed to decode messages " <> show msgs

  it "sends an error when input cannot be decoded" $
    failAfter 5 $
      withFreePort $ \port -> sendsAnErrorWhenInputCannotBeDecoded port

sendsAnErrorWhenInputCannotBeDecoded :: Int -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) nullTracer noop $ \_sendOutput -> do
    withClient port $ \con -> do
      sendBinaryData @Text con invalidInput
      msg <- receiveData con
      case Aeson.eitherDecode msg of
        Right resp -> resp `shouldBe` (InvalidInput :: ServerOutput SimpleTx)
        Left{} -> failure $ "Failed to decode output " <> show msg
 where
  invalidInput = "not a valid message"

testClient :: TQueue IO (ServerOutput SimpleTx) -> TVar IO Int -> Connection -> IO ()
testClient queue semaphore cnx = do
  atomically $ modifyTVar' semaphore (+ 1)
  msg <- receiveData cnx
  case Aeson.eitherDecode msg of
    Right resp -> atomically (writeTQueue queue resp)
    Left{} -> failure $ "Failed to decode message " <> show msg

noop :: Applicative m => a -> m ()
noop = const $ pure ()

withClient :: HasCallStack => Int -> (Connection -> IO ()) -> IO ()
withClient port action = do
  failAfter 5 retry
 where
  retry = runClient "127.0.0.1" port "/" action `catch` \(_ :: IOException) -> retry
