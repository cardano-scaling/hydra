{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A `Network` layer that guarantees delivery of `msg` in order even in the
-- face of transient connection failures.
--
-- This layer implements an algorithm based on _vector clocks_, loosely inspired from
-- /Reliable consistent broadcast/ algorithms with FIFO ordering as presented in
-- [Introduction to Reliable and Secure Distributed
-- Programming](https://www.distributedprogramming.net), ch. 3.9, by Cachin et al.
--
-- Each node maintains a vector of monotonically increasing integer
-- indices denoting the index of the last message known (sent or received) for
-- each peer, where a peer is identified a `Party`, which is updated upon
-- sending and receiving messages, and is sent with each message.
--
-- The basic algorithm is simple:
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
-- to securely identify senders, and also on `Heartbeat` messages in order to
-- provide some "liveness".
--
-- `Heartbeat` messages are critical in order to /signal/ peers our current view
-- of the world, because it could be the case that we don't have any network
-- message to send which leads to head being stalled. `Ping` messages are
-- treated specially, both when receiving and sending:
--
--   * When sending a `Ping`, we /don't increment/ our local message counter
--     before broadcasting it,
--
--   * Conversely, when receiving a `Ping`, we don't update the peer's message
--     counter but only take into account their view of /our/ counter in order
--     to compute whether or not to resend previous messages.
--
-- NOTE: This layer does not guarantee resilience in the crash-recovery setting,
-- eg. if a process crashes and then later recovers. It may work because `Ping`s
-- will trigger some resending to the peer which starts from scratch. To provide
-- this guarantee in full, we should add /logging/ capability that persist sent and
-- received messages before communicating with the applicative layer.
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
import qualified Data.IntMap as IMap
import qualified Data.Map.Strict as Map
import Data.Vector (
  Vector,
  elemIndex,
  fromList,
  generate,
  length,
  replicate,
  zipWith,
  (!),
 )
import Hydra.Logging (traceWith)
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), isPing)
import Hydra.Party (Party)
import Test.QuickCheck (getPositive, listOf)

data ReliableMsg msg = ReliableMsg
  { knownMessageIds :: Vector Int
  -- ^ Vector of highest known counter for each known party. Serves as announcement of
  -- which messages the sender of `ReliableMsg` has seen. The individual counters have
  -- nothing to do with the `message` also included in this.
  , message :: msg
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (ToCBOR msg) => ToCBOR (ReliableMsg msg) where
  toCBOR ReliableMsg{knownMessageIds, message} = toCBOR knownMessageIds <> toCBOR message

instance (FromCBOR msg) => FromCBOR (ReliableMsg msg) where
  fromCBOR = ReliableMsg <$> fromCBOR <*> fromCBOR

instance ToCBOR msg => SignableRepresentation (ReliableMsg msg) where
  getSignableRepresentation = serialize'

data ReliabilityException
  = -- | Signals that received acks from the peer is not of
    -- proper length
    ReliabilityReceivedAckedMalformed
  | -- | This should never happen. We should always be able to find a message by
    -- the given index.
    ReliabilityFailedToFindMsg String
  | -- | This should never happen. We should always be able to find a party
    -- index in a list of parties.
    ReliabilityMissingPartyIndex Party
  deriving (Eq, Show)

instance Exception ReliabilityException

data ReliabilityLog
  = Resending {missing :: Vector Int, acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | BroadcastCounter {partyIndex :: Int, localCounter :: Vector Int}
  | BroadcastPing {partyIndex :: Int, localCounter :: Vector Int}
  | Received {acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | Ignored {acknowledged :: Vector Int, localCounter :: Vector Int, partyIndex :: Int}
  | ClearedMessageQueue {messageQueueLength :: Int, deletedMessages :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary (Vector Int) where
  arbitrary = fromList <$> listOf (getPositive <$> arbitrary)

instance Arbitrary ReliabilityLog where
  arbitrary = genericArbitrary

-- | Middleware function to handle message counters tracking and resending logic.
--
-- '''NOTE''': There is some "abstraction leak" here, because the `withReliability`
-- layer is tied to a specific structure of other layers, eg. be between
-- `withHeartbeat` and `withAuthenticate` layers.
--
-- NOTE: better use of Vectors? We should perhaps use a `MVector` to be able to
-- mutate in-place and not need `zipWith`
withReliability ::
  (MonadThrow (STM m), MonadThrow m, MonadAsync m) =>
  -- | Tracer for logging messages.
  Tracer m ReliabilityLog ->
  -- | Our own party identifier.
  Party ->
  -- | Other parties' identifiers.
  [Party] ->
  -- | Underlying network component providing consuming and sending channels.
  NetworkComponent m (Authenticated (ReliableMsg (Heartbeat msg))) (ReliableMsg (Heartbeat msg)) a ->
  NetworkComponent m (Authenticated (Heartbeat msg)) (Heartbeat msg) a
withReliability tracer me otherParties withRawNetwork callback action = do
  ackCounter <- newTVarIO $ replicate (length allParties) 0
  sentMessages <- newTVarIO IMap.empty
  seenMessages <- newTVarIO $ Map.fromList $ (,0) <$> toList allParties
  resendQ <- newTQueueIO
  ourIndex <- findPartyIndex me
  let resend = writeTQueue resendQ
  withRawNetwork (reliableCallback ackCounter sentMessages seenMessages resend) $ \network@Network{broadcast} -> do
    withAsync (forever $ atomically (readTQueue resendQ) >>= broadcast) $ \_ ->
      reliableBroadcast ourIndex ackCounter sentMessages network
 where
  allParties = fromList $ sort $ me : otherParties

  reliableBroadcast ourIndex ackCounter sentMessages Network{broadcast} =
    action $
      Network
        { broadcast = \msg ->
            case msg of
              Data{} -> do
                ackCounter' <- atomically $ do
                  acks <- readTVar ackCounter
                  let newAcks = constructAcks acks ourIndex
                  writeTVar ackCounter newAcks
                  modifyTVar' sentMessages (insertNewMsg msg)
                  readTVar ackCounter

                traceWith tracer (BroadcastCounter ourIndex ackCounter')
                broadcast $ ReliableMsg ackCounter' msg
              Ping{} -> do
                acks <- readTVarIO ackCounter
                traceWith tracer (BroadcastPing ourIndex acks)
                broadcast $ ReliableMsg acks msg
        }

  reliableCallback ackCounter sentMessages seenMessages resend (Authenticated (ReliableMsg acks msg) party) = do
    if length acks /= length allParties
      then ignoreMalformedMessages
      else do
        partyIndex <- findPartyIndex party
        (shouldCallback, messageAckForParty, knownAckForParty, knownAcks) <- atomically $ do
          let messageAckForParty = acks ! partyIndex
          knownAcks <- readTVar ackCounter
          let knownAckForParty = knownAcks ! partyIndex

          -- handle message from party iff it's next in line OR if it's a Ping
          --
          -- The stream of messages from a peer is expected to look like:
          --
          -- @@
          -- Msg [.., 1, ..] (Data nid m1)
          -- Msg [.., 2, ..] (Data nid m1)
          -- Msg [.., 3, ..] (Data nid m1)
          -- Msg [.., 3, ..] (Ping nid)
          -- Msg [.., 3, ..] (Ping nid)
          -- Msg [.., 3, ..] (Ping nid)
          -- @@
          --
          -- Pings are observed only for the information it provides about the
          -- peer's view of our index
          if isPing msg
            then return (isPing msg, messageAckForParty, knownAckForParty, knownAcks)
            else
              if messageAckForParty == knownAckForParty + 1
                then do
                  let newAcks = constructAcks knownAcks partyIndex
                  writeTVar ackCounter newAcks
                  return (True, messageAckForParty, knownAckForParty, newAcks)
                else return (isPing msg, messageAckForParty, knownAckForParty, knownAcks)

        if shouldCallback
          then do
            callback (Authenticated msg party)
            traceWith tracer (Received acks knownAcks partyIndex)
          else traceWith tracer (Ignored acks knownAcks partyIndex)

        -- NOTE: We only check whether or not our peer is lagging behind to
        -- resend messages if the peer is quiescent, ie. it did not make any
        -- progress since the last message we got from it ; or if it is sending
        -- us "old" messages. This could happen if it is also resending
        -- messages, but then we might detect later it's not actually lagging
        -- and therefore we won't resend
        when (messageAckForParty <= knownAckForParty) $
          resendMessagesIfLagging resend partyIndex sentMessages knownAcks acks

        -- Update last message index sent by us and seen by some party
        updateSeenMessages seenMessages acks party
        -- Take the lowest number from seen messages by everyone and remove it from
        -- our sent messages.
        deleteSeenMessages sentMessages seenMessages

  ignoreMalformedMessages = pure ()

  constructAcks acks wantedIndex =
    zipWith (\ack i -> if i == wantedIndex then ack + 1 else ack) acks partyIndexes

  partyIndexes = generate (length allParties) id

  resendMessagesIfLagging resend partyIndex sentMessages knownAcks messageAcks = do
    myIndex <- findPartyIndex me
    let messageAckForUs = messageAcks ! myIndex
    let knownAckForUs = knownAcks ! myIndex

    -- We resend messages if our peer notified us that it's lagging behind our
    -- latest message sent
    when (messageAckForUs < knownAckForUs) $ do
      let missing = fromList [messageAckForUs + 1 .. knownAckForUs]
      messages <- readTVarIO sentMessages
      forM_ missing $ \idx -> do
        case messages IMap.!? idx of
          Nothing ->
            throwIO $
              ReliabilityFailedToFindMsg $
                "FIXME: this should never happen, there's no sent message at index "
                  <> show idx
                  <> ", messages length = "
                  <> show (IMap.size messages)
                  <> ", latest message ack: "
                  <> show knownAckForUs
                  <> ", acked: "
                  <> show messageAckForUs
          Just missingMsg -> do
            let newAcks' = zipWith (\ack i -> if i == myIndex then idx else ack) knownAcks partyIndexes
            traceWith tracer (Resending missing messageAcks newAcks' partyIndex)
            atomically $ resend $ ReliableMsg newAcks' missingMsg

  updateSeenMessages seenMessages acks party = do
    myIndex <- findPartyIndex me
    let messageAckForUs = acks ! myIndex
    atomically $ modifyTVar' seenMessages (Map.insert party messageAckForUs)

  deleteSeenMessages sentMessages seenMessages = do
    clearedMessages <- atomically $ do
      seenMessages' <- readTVar seenMessages
      let messageReceivedByEveryone =
            case sortBy (\(_, a) (_, b) -> compare a b) (Map.toList seenMessages') of
              [] -> 0 -- should not happen
              ((_, v) : _) -> v
      sentMessages' <- readTVar sentMessages
      if IMap.member messageReceivedByEveryone sentMessages'
        then do
          let updatedMap = IMap.delete messageReceivedByEveryone sentMessages'
          writeTVar sentMessages updatedMap
          pure $ Just (IMap.size updatedMap, messageReceivedByEveryone)
        else pure Nothing

    case clearedMessages of
      Nothing -> pure ()
      Just (messageQueueLength, deletedMessages) ->
        traceWith tracer (ClearedMessageQueue{messageQueueLength, deletedMessages})

  insertNewMsg msg m =
    case IMap.lookupMax m of
      Nothing -> IMap.insert 1 msg m
      Just (k, _) -> IMap.insert (k + 1) msg m

  -- find the index of a party in the list of parties or fail with 'ReliabilityMissingPartyIndex'
  findPartyIndex party =
    maybe (throwIO $ ReliabilityMissingPartyIndex party) pure $ elemIndex party allParties
