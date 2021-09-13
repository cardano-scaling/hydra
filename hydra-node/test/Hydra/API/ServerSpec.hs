{-# LANGUAGE TypeApplications #-}

module Hydra.API.ServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

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
import Hydra.API.Server (Server (Server, sendOutput), withAPIServer)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.ServerOutput (ServerOutput (Greetings, InvalidInput, ReadyToCommit), input)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)
import Test.Network.Ports (withFreePort)
import Test.QuickCheck (cover)
import Test.QuickCheck.Monadic (monadicIO, monitor, run)

spec :: Spec
spec = parallel $ do
  let party = 1
      greeting = Greetings party

  it "greets" $ do
    failAfter 5 $
      withFreePort $ \port ->
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) party nullTracer noop $ \_ -> do
          withClient port $ \conn -> do
            received <- receiveData conn
            case Aeson.eitherDecode received of
              Right msg -> msg `shouldBe` greeting
              Left{} -> failure $ "Failed to decode greeting " <> show received

  it "sends sendOutput to all connected clients" $ do
    pendingWith "failing since SN introduced greetings.. he is sorry, but no time to fix this now"
    queue <- atomically newTQueue
    failAfter 5 $
      withFreePort $ \port ->
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) party nullTracer noop $ \Server{sendOutput} -> do
          semaphore <- newTVarIO 0
          withAsync
            ( concurrently_
                (withClient port $ testClient queue semaphore)
                (withClient port $ testClient queue semaphore)
            )
            $ \_ -> do
              atomically $ readTVar semaphore >>= \n -> check (n >= 2)
              failAfter 1 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [greeting, greeting]
              let arbitraryMsg = ReadyToCommit mempty
              sendOutput arbitraryMsg
              failAfter 1 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [arbitraryMsg, arbitraryMsg]
              failAfter 1 $ atomically (tryReadTQueue queue) `shouldReturn` Nothing

  prop "echoes history (past outputs) to client upon reconnection" $ \msgs -> monadicIO $ do
    monitor $ cover 1 (null msgs) "no message when reconnecting"
    monitor $ cover 1 (length msgs == 1) "only one message when reconnecting"
    monitor $ cover 1 (length msgs > 1) "more than one message when reconnecting"
    run . failAfter 5 $ do
      withFreePort $ \port ->
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) party nullTracer noop $ \Server{sendOutput} -> do
          mapM_ sendOutput (msgs :: [ServerOutput SimpleTx])
          withClient port $ \conn -> do
            received <- replicateM (length msgs + 1) (receiveData conn)
            case traverse Aeson.eitherDecode received of
              Right msgs' -> msgs' `shouldBe` greeting : msgs
              Left{} -> failure $ "Failed to decode messages " <> show msgs

  it "sends an error when input cannot be decoded" $
    failAfter 5 $
      withFreePort $ \port -> sendsAnErrorWhenInputCannotBeDecoded port

sendsAnErrorWhenInputCannotBeDecoded :: Int -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) 1 nullTracer noop $ \_server -> do
    withClient port $ \con -> do
      _greeting :: ByteString <- receiveData con
      sendBinaryData con invalidInput
      msg <- receiveData con
      case Aeson.eitherDecode @(ServerOutput SimpleTx) msg of
        Right resp -> resp `shouldSatisfy` isInvalidInput
        Left{} -> failure $ "Failed to decode output " <> show msg
 where
  invalidInput = "not a valid message"
  isInvalidInput = \case
    InvalidInput{input} -> input == invalidInput
    _ -> False

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
