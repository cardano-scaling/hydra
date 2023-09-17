{-# OPTIONS_GHC -Wno-orphans #-}

-- | A `Network` layer that guarantees delivery of `msg` in order even in the
-- face of transient connection failures.
--
-- This layer implements an algorithm based on _vector clocks_ adapted from
-- reliable consistent broadcast algorithms with FIFO ordering as presented in
-- [Introduction to Reliable and Secure Distributed
-- Programming](https://www.distributedprogramming.net), ch. 3.9, by Cachin and
-- Guerraoui. Each node maintains a vector of monotonically increasing integer
-- indices denoting the index of the last message known (sent or received) from
-- each peer, where a peer is identified a `Party`, which is updated upon
-- sending and receiving messages, and is sent with each message.
--
-- The algorithm is simple:
--
--   * When a message is sent, the index of the current node's party is incremented,
--
--   * When a message is received:
--
--       * It is discarded if the index for the sender's party in the message is
--         not exactly one more than the latest known index,
--
--       * If our own party's index as broadcasted by the sender is lower than our
--         latest known index, we resend all the "missing" messages.
--
-- As shown by the signature of the `withReliability` function, this layer
-- relies on an authentication layer providing `Authenticated` messages in order
-- to securely identify senders.
--
-- NOTE: This layer does not guarantee resilience in the crash-recovery setting,
-- eg. if a process crashes and then later recovers. To provide this guarantee,
-- we should add _logging_ capability that persist sent and received messages
-- before communicating with the applicative layer.
module Hydra.Network.Reliability where

import Hydra.Prelude hiding (empty, fromList, length, replicate, zipWith)

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, readTVarIO, writeTQueue),
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  writeTVar,
 )
import Control.Tracer (Tracer)
import Data.Maybe (fromJust)
import Data.Vector (
  Vector,
  elemIndex,
  empty,
  fromList,
  generate,
  length,
  replicate,
  snoc,
  zipWith,
  (!),
  (!?),
 )
import Hydra.Logging (traceWith)
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Party (Party)
import Test.QuickCheck (getPositive, listOf)

data Msg msg = Msg
  { messageId :: Vector Int
  , message :: msg
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (ToCBOR msg) => ToCBOR (Msg msg) where
  toCBOR Msg{messageId, message} = toCBOR messageId <> toCBOR message

instance (FromCBOR msg) => FromCBOR (Msg msg) where
  fromCBOR = Msg <$> fromCBOR <*> fromCBOR

instance ToCBOR msg => SignableRepresentation (Msg msg) where
  getSignableRepresentation = serialize'

data ReliabilityException
  = -- | Signals that received acks from the peer is not of
    -- proper length
    ReliabilityReceivedAckedMalformed
  | -- | This should never happen. We should always be able to find a message by
    -- the given index.
    ReliabilityFailedToFindMsg String
  deriving (Eq, Show)

instance Exception ReliabilityException

withReliability ::
  forall m msg a.
  (MonadAsync m, MonadThrow (STM m), MonadThrow m) =>
  Tracer m ReliabilityLog ->
  Party ->
  Vector Party ->
  NetworkComponent m (Authenticated (Msg msg)) (Authenticated (Msg msg)) a ->
  NetworkComponent m (Authenticated msg) (Authenticated msg) a
withReliability tracer us allParties withRawNetwork callback action = do
  ackCounter <- newTVarIO $ replicate (length allParties) 0
  sentMessages <- newTVarIO empty
  resendQ <- newTQueueIO
  let resend = writeTQueue resendQ
  withRawNetwork (reliableCallback ackCounter sentMessages resend) $ \network@Network{broadcast} -> do
    withAsync (forever $ atomically (readTQueue resendQ) >>= broadcast) $ \_ ->
      reliableBroadcast ackCounter sentMessages network
 where
  reliableBroadcast ackCounter sentMessages Network{broadcast} =
    action $
      Network
        { broadcast = \(Authenticated msg _) -> do
            let ourIndex = fromJust $ elemIndex us allParties
            ackCounter' <- atomically $ do
              acks <- readTVar ackCounter
              let newAcks = constructAcks acks ourIndex
              writeTVar ackCounter newAcks
              modifyTVar' sentMessages (`snoc` msg)
              readTVar ackCounter

            traceWith tracer (BroadcastCounter ourIndex ackCounter')

            broadcast $ Authenticated (Msg ackCounter' msg) us
        }

  reliableCallback ackCounter sentMessages resend (Authenticated (Msg acks msg) party) = do
    if length acks /= length allParties
      then throwIO ReliabilityReceivedAckedMalformed
      else do
        let partyIndex = fromJust $ elemIndex party allParties
        let n = acks ! partyIndex
        existingAcks <- readTVarIO ackCounter
        let count = existingAcks ! partyIndex

        traceWith tracer (Receiving acks existingAcks partyIndex)

        -- handle message from party iff it's next in line
        when (n == count + 1) $ do
          let newAcks = constructAcks existingAcks partyIndex
          atomically $ writeTVar ackCounter newAcks
          callback (Authenticated msg party)

        -- FIXME: we are misusing the vector clocks we carry in our messagse. We should NOT
        -- resend messages when the message received message's clock is not behind ours, as
        -- this means some messages are still in flight and not handled by the peer. It's only
        -- when we receive an "old" message where both the message index and their view of our
        -- index are behind that we should resend.
        --
        -- read our local acks again since they might be updated above
        existingAcks' <- readTVarIO ackCounter
        -- resend messages if party did not acknowledge our latest idx
        let myIndex = fromJust $ elemIndex us allParties
        let acked = acks ! myIndex
        let latestMsgAck = existingAcks' ! myIndex
        when (acked < latestMsgAck && n <= count) $ do
          let missing = fromList [acked + 1 .. latestMsgAck]
          messages <- readTVarIO sentMessages
          forM_ missing $ \idx -> do
            case messages !? (idx - 1) of
              Nothing ->
                throwIO $
                  ReliabilityFailedToFindMsg $
                    "FIXME: this should never happen, there's no sent message at index "
                      <> show idx
                      <> ", messages length = "
                      <> show (length messages)
                      <> ", latest message ack: "
                      <> show latestMsgAck
                      <> ", acked: "
                      <> show acked
              Just missingMsg -> do
                let newAcks' = zipWith (\ack i -> if i == myIndex then idx else ack) existingAcks' partyIndexes
                traceWith tracer (Resending missing acks newAcks' partyIndex)
                atomically $ resend $ Authenticated (Msg newAcks' missingMsg) us

  partyIndexes = generate (length allParties) id

  constructAcks acks wantedIndex =
    zipWith (\ack i -> if i == wantedIndex then ack + 1 else ack) acks partyIndexes

data ReliabilityLog
  = Resending {missing :: Vector Int, acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | BroadcastCounter {partyIndex :: Int, localCounter :: Vector Int}
  | Receiving {acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary (Vector Int) where
  arbitrary = fromList <$> listOf (getPositive <$> arbitrary)

instance Arbitrary ReliabilityLog where
  arbitrary = genericArbitrary
