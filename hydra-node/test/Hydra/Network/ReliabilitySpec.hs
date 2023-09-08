{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (readTQueue, readTVarIO, writeTQueue), modifyTVar', newTQueueIO, newTVarIO, writeTVar)
import Control.Concurrent.Class.MonadSTM.TVar (modifyTVar)
import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence ((|>))
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Party (Party)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (Positive (Positive), collect, counterexample, generate)

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _cb action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (|> msg)}

      captureIncoming receivedMessages msg =
        atomically $ modifyTVar' receivedMessages (|> msg)

  msg <- runIO $ generate @String arbitrary

  it "forward received messages" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob]
            ( \incoming _ -> do
                incoming (Authenticated (Msg [1, 1] msg) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

  prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO mempty

          withReliability alice [alice] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
            mapM_ broadcast messages

          toList <$> readTVarIO sentMessages
     in List.head . messageId . payload <$> sentMsgs `shouldBe` [1 .. (length messages)]

  it "broadcasts messages to single connected peer" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty
          queue <- newTQueueIO

          let aliceNetwork _ action = do
                action (Network{broadcast = atomically . writeTQueue queue})

          let bobNetwork callback action = do
                withAsync
                  ( forever $ do
                      newMsg <- atomically $ readTQueue queue
                      callback newMsg
                  )
                  $ \_ ->
                    action (Network{broadcast = const $ pure ()})

          withReliability alice [alice, bob] aliceNetwork (const $ pure ()) $ \Network{broadcast} ->
            withReliability bob [alice, bob] bobNetwork (captureIncoming receivedMessages) $ \_ -> do
              broadcast msg
              threadDelay 1

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg]

  prop "drops already received messages" $ \(messages :: [Positive Int]) ->
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob]
            ( \incoming _ -> do
                forM_ messages $ \(Positive m) ->
                  incoming (Authenticated (Msg [0, m] m) bob)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages
        receivedMessagesInOrder =
          and (zipWith (==) receivedMsgs [1 ..])
     in receivedMessagesInOrder
          & counterexample (show receivedMsgs)
          & collect (length receivedMsgs)

  it "do not drop messages with same ids from different peers" $ do
    let receivedMsgs = runSimOrThrow $ do
          receivedMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob, carol]
            ( \incoming _ -> do
                incoming (Authenticated (Msg [0, 1, 0] msg) bob)
                incoming (Authenticated (Msg [0, 0, 1] msg) carol)
            )
            (captureIncoming receivedMessages)
            $ \_ ->
              pure ()

          toList <$> readTVarIO receivedMessages

    receivedMsgs `shouldBe` [msg, msg]

  it "sends unacknowledged messages" $ do
    let sentMsgs = runSimOrThrow $ do
          sentMessages <- newTVarIO mempty

          withReliability
            alice
            [alice, bob]
            ( \incoming action -> do
                action $ Network{broadcast = \_ -> atomically $ modifyTVar' sentMessages (|> msg)}
                incoming (Authenticated (Msg [0, 1] msg) bob)
            )
            noop
            $ \Network{broadcast} ->
              broadcast msg

          toList <$> readTVarIO sentMessages

    sentMsgs `shouldBe` [msg, msg]

data Msg msg = Msg
  { messageId :: [Int]
  , message :: msg
  }
  deriving (Eq, Show)

withReliability ::
  (MonadSTM m) =>
  Party ->
  [Party] ->
  NetworkComponent m (Authenticated (Msg msg)) a ->
  NetworkComponent m msg a
withReliability us allParties withRawNetwork callback action = do
  broadcastCounter <- newTVarIO $ replicate (length allParties) 0
  incomingCounter <- newTVarIO mempty
  withRawNetwork (dummyCallback incomingCounter) (dummyBroadcast broadcastCounter)
 where
  dummyBroadcast messageCounter Network{broadcast} =
    action $
      Network
        { broadcast = \msg -> do
            counter <- atomically $ do
              acks <- readTVar messageCounter
              let ourIndex = fromJust $ List.elemIndex us allParties
              let newAcks = zipWith (\ack i -> if i == ourIndex then ack + 1 else ack) acks [0 ..]
              writeTVar messageCounter newAcks
              readTVar messageCounter

            broadcast $ Authenticated (Msg counter msg) us
        }

  dummyCallback messageCounter (Authenticated (Msg acks msg) party) = do
    let partyIndex = fromJust $ List.elemIndex party allParties
    let n = acks List.!! partyIndex
    count <- fromMaybe 0 . Map.lookup party <$> readTVarIO messageCounter
    when (n == count + 1) $ do
      atomically $ modifyTVar messageCounter (Map.insert party n)
      callback msg

noop :: Monad m => b -> m ()
noop = const $ pure ()
