{-# LANGUAGE TypeApplications #-}

module Hydra.API.ServerSpec where

import Hydra.Prelude hiding (seq)
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
import Hydra.API.ServerOutput (ServerOutput (Greetings, InvalidInput), TimedServerOutput (..), input)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Persistence (PersistenceIncremental (..), createPersistenceIncremental)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)
import Test.Hydra.Fixture (alice)
import Test.Network.Ports (withFreePort)
import Test.QuickCheck (checkCoverage, cover, generate)
import Test.QuickCheck.Monadic (monadicIO, monitor, pick, run)

spec :: Spec
spec = parallel $ do
  it "greets" $ do
    failAfter 5 $
      withFreePort $ \port -> do
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice mockPersistence nullTracer noop $ \_ -> do
          withClient port defaultPath $ \conn -> do
            received <- receiveData conn
            case Aeson.eitherDecode received of
              Left{} -> failure $ "Failed to decode greeting " <> show received
              Right TimedServerOutput{output = msg} -> msg `shouldBe` greeting

  it "sends sendOutput to all connected clients" $ do
    queue <- atomically newTQueue
    showLogsOnFailure $ \tracer -> failAfter 5 $
      withFreePort $ \port -> do
        withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice mockPersistence tracer noop $ \Server{sendOutput} -> do
          semaphore <- newTVarIO 0
          withAsync
            ( concurrently_
                (withClient port defaultPath $ testClient queue semaphore)
                (withClient port defaultPath $ testClient queue semaphore)
            )
            $ \_ -> do
              waitForClients semaphore
              failAfter 1 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [greeting, greeting]
              arbitraryMsg <- generate arbitrary
              sendOutput arbitraryMsg
              failAfter 1 $ atomically (replicateM 2 (readTQueue queue)) `shouldReturn` [arbitraryMsg, arbitraryMsg]
              failAfter 1 $ atomically (tryReadTQueue queue) `shouldReturn` Nothing

  it "sends all sendOutput history to all connected clients after a restart" $ do
    showLogsOnFailure $ \tracer -> failAfter 5 $
      withTempDir "ServerSpec" $ \tmpDir -> do
        let persistentFile = tmpDir <> "/history"
        arbitraryMsg <- generate arbitrary

        persistence <- createPersistenceIncremental persistentFile
        withFreePort $ \port -> do
          withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice persistence tracer noop $ \Server{sendOutput} -> do
            sendOutput arbitraryMsg

        queue1 <- atomically newTQueue
        queue2 <- atomically newTQueue
        persistence' <- createPersistenceIncremental persistentFile
        withFreePort $ \port -> do
          withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice persistence' tracer noop $ \Server{sendOutput} -> do
            semaphore <- newTVarIO 0
            withAsync
              ( concurrently_
                  (withClient port defaultPath $ testClient queue1 semaphore)
                  (withClient port defaultPath $ testClient queue2 semaphore)
              )
              $ \_ -> do
                waitForClients semaphore
                failAfter 1 $ atomically (replicateM 3 (readTQueue queue1)) `shouldReturn` [greeting, arbitraryMsg, greeting]
                failAfter 1 $ atomically (replicateM 3 (readTQueue queue2)) `shouldReturn` [greeting, arbitraryMsg, greeting]
                sendOutput arbitraryMsg
                failAfter 1 $ atomically (replicateM 1 (readTQueue queue1)) `shouldReturn` [arbitraryMsg]
                failAfter 1 $ atomically (replicateM 1 (readTQueue queue2)) `shouldReturn` [arbitraryMsg]
                failAfter 1 $ atomically (tryReadTQueue queue1) `shouldReturn` Nothing

  it "echoes history (past outputs) to client upon reconnection" $
    checkCoverage . monadicIO $ do
      outputs <- pick arbitrary
      monitor $ cover 0.1 (null outputs) "no message when reconnecting"
      monitor $ cover 0.1 (length outputs == 1) "only one message when reconnecting"
      monitor $ cover 1 (length outputs > 1) "more than one message when reconnecting"
      run . failAfter 15 $ do
        withFreePort $ \port ->
          withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice mockPersistence nullTracer noop $ \Server{sendOutput} -> do
            mapM_ sendOutput outputs
            withClient port defaultPath $ \conn -> do
              received <- replicateM (length outputs + 1) (receiveData conn)
              case traverse Aeson.eitherDecode received of
                Left{} -> failure $ "Failed to decode messages:\n" <> show received
                Right timedOutputs -> do
                  (output <$> timedOutputs) `shouldBe` greeting : outputs

  it "doesn't echo history if client says no" $
    checkCoverage . monadicIO $ do
      outputs :: [ServerOutput SimpleTx] <- pick arbitrary
      monitor $ cover 0.1 (null outputs) "no message when reconnecting"
      monitor $ cover 0.1 (length outputs == 1) "only one message when reconnecting"
      monitor $ cover 1 (length outputs > 1) "more than one message when reconnecting"
      run . failAfter 15 $ do
        withFreePort $ \port ->
          withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice mockPersistence nullTracer noop $ \Server{sendOutput} -> do
            mapM_ sendOutput outputs
            -- start client that doesn't want to see the history
            withClient port defaultPath $ \conn -> do
              received <- replicateM 1 (receiveData conn)
              case traverse Aeson.eitherDecode received of
                Left{} -> failure $ "Failed to decode messages:\n" <> show received
                Right timedOutputs -> do
                  (output <$> timedOutputs) `shouldBe` [greeting]

  it "sequence numbers are continuous and strictly monotonically increasing" $
    monadicIO $ do
      outputs :: [ServerOutput SimpleTx] <- pick arbitrary
      run . failAfter 5 $ do
        withFreePort $ \port ->
          withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice mockPersistence nullTracer noop $ \Server{sendOutput} -> do
            mapM_ sendOutput outputs
            withClient port defaultPath $ \conn -> do
              received <- replicateM (length outputs + 1) (receiveData conn)
              case traverse Aeson.eitherDecode received of
                Left{} -> failure $ "Failed to decode messages:\n" <> show received
                Right (timedOutputs :: [TimedServerOutput SimpleTx]) ->
                  seq <$> timedOutputs `shouldSatisfy` strictlyMonotonic

  it "sends an error when input cannot be decoded" $
    failAfter 5 $
      withFreePort $ \port -> sendsAnErrorWhenInputCannotBeDecoded port

strictlyMonotonic :: [Natural] -> Bool
strictlyMonotonic = \case
  [] -> True
  [_] -> True
  (a : b : as) -> a + 1 == b && strictlyMonotonic (b : as)

sendsAnErrorWhenInputCannotBeDecoded :: Int -> Expectation
sendsAnErrorWhenInputCannotBeDecoded port = do
  withAPIServer @SimpleTx "127.0.0.1" (fromIntegral port) alice mockPersistence nullTracer noop $ \_server -> do
    withClient port defaultPath $ \con -> do
      _greeting :: ByteString <- receiveData con
      sendBinaryData con invalidInput
      msg <- receiveData con
      case Aeson.eitherDecode @(TimedServerOutput SimpleTx) msg of
        Left{} -> failure $ "Failed to decode output " <> show msg
        Right TimedServerOutput{output = resp} -> resp `shouldSatisfy` isInvalidInput
 where
  invalidInput = "not a valid message"
  isInvalidInput = \case
    InvalidInput{input} -> input == invalidInput
    _ -> False

greeting :: ServerOutput SimpleTx
greeting = Greetings alice

waitForClients :: (MonadSTM m, Ord a, Num a) => TVar m a -> m ()
waitForClients semaphore = atomically $ readTVar semaphore >>= \n -> check (n >= 2)

-- NOTE: this client runs indefinitely so it should be run within a context that won't
-- leak runaway threads
testClient :: TQueue IO (ServerOutput SimpleTx) -> TVar IO Int -> Connection -> IO ()
testClient queue semaphore cnx = do
  atomically $ modifyTVar' semaphore (+ 1)
  msg <- receiveData cnx
  case Aeson.eitherDecode msg of
    Left{} -> failure $ "Failed to decode message " <> show msg
    Right TimedServerOutput{output = resp} -> do
      atomically (writeTQueue queue resp)
      testClient queue semaphore cnx

noop :: Applicative m => a -> m ()
noop = const $ pure ()

defaultPath :: String
defaultPath = "/"

withClient :: HasCallStack => Int -> String -> (Connection -> IO ()) -> IO ()
withClient port path action = do
  failAfter 5 retry
 where
  retry = runClient "127.0.0.1" port path action `catch` \(_ :: IOException) -> retry

-- | Mocked persistence handle which just does nothing.
mockPersistence :: Applicative m => PersistenceIncremental a m
mockPersistence =
  PersistenceIncremental
    { append = \_ -> pure ()
    , loadAll = pure []
    }
