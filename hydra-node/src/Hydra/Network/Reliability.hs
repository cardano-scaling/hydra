{-# OPTIONS_GHC -Wno-orphans #-}

-- | A `Network` layer that provides resilience in the face of network
-- connectivity loss and (most) crashes.
--
-- This network layer takes care of 2 aspects that together improve the
-- reliability and operability of a Hydra cluster, making it more tolerant to
-- transient failures and therefore alleviating the need to prematurely close
-- heads:
--
-- 1. To safeguard against lost of connectivity, it keeps track of messages
-- indices and resend lost messages,
-- 2. To safeguard against crashes, it persists all sent messages and indices on disk.
--
-- == Messages ordering & resending
--
-- This layer implements an algorithm based on /vector clocks/, loosely inspired from
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
--       * If our own party's index, as broadcasted by the sender, is lower than
--         our latest known index, and the peer appears /quiescent/ we resend
--         all the "missing" messages.
--
-- As shown by the signature of the `withReliability` function, this layer
-- relies on an authentication layer providing `Authenticated` messages in order
-- to securely identify senders, and also on `Heartbeat` messages in order to
-- provide some "liveness".
--
-- `Heartbeat` messages are critical in order to /signal/ peers our current view
-- of the world, because it could be the case that we don't have any network
-- message to send which leads to head being stalled. `Ping` messages in
-- particular are used to denote the fact the sender is /quiescent/, ie. it's
-- not able to make any /observable/ progress. Those messages are treated
-- specially, both when receiving and sending:
--
--   * When sending a `Ping`, we /don't increment/ our local message counter
--     before broadcasting it,
--
--   * Conversely, when receiving a `Ping`, we don't update the peer's message
--     counter but only take into account their view of /our/ counter in order
--     to compute whether or not to resend previous messages.
--
-- == Messages persistence
--
-- The `MessagePersistence` handle defines an interface that's used to load and
-- persist both the sequence of messages `broadcast`ed, and the /vector clock/
-- representing our current view of the all peers' knowledge about
-- messages. Because it's hard to guarantee atomicity of IO operations on files,
-- we only save the indices when we /broadcast/ and use a local cache in the
-- `NetworkCallback`: The underlying layers can callback us concurrently,
-- eg. when each connection to peers is managed by a dedicated thread.
--
-- __NOTE__: We do not recover (at least) from one particular crash situation: When
-- the `withReliability` "delivers" a message to the calling code, it's usually
-- put in a queue wrapped into a `NetworkEvent` and then handled later
-- on. Should the node crashes at that particular point, it won't be resubmitted
-- the same message later because the message indices could have been updated
-- and written to on disk already.
--
-- __NOTE__: Messages sent are /currently/ kept indefinitely on disk because we
-- don't set any bound to how far from the past a peer would need to be resent
-- messages. In the case of very long-running head with a high frequency of
-- transaction submission, can lead to significant storage use. Should this
-- become a problem, this can be mitigated by closing and reopening a head.
module Hydra.Network.Reliability where

import Hydra.Prelude hiding (empty, fromList, length, replicate, zipWith)

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, writeTQueue),
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  writeTVar,
 )
import Control.Tracer (Tracer)
import Data.IntMap qualified as IMap
import Data.Sequence.Strict qualified as Seq
import Data.Vector (
  Vector,
  elemIndex,
  fromList,
  generate,
  length,
  replicate,
  zipWith,
  (!?),
 )
import Hydra.Logging (traceWith)
import Hydra.Network (Network (..), NetworkCallback (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), isPing)
import Hydra.Persistence (Persistence (..), PersistenceIncremental (..))
import Hydra.Tx (Party)
import Test.QuickCheck.Instances.Vector ()

data ReliableMsg msg = ReliableMsg
  { knownMessageIds :: Vector Int
  -- ^ Vector of highest known counter for each known party. Serves as announcement of
  -- which messages the sender of `ReliableMsg` has seen. The individual counters have
  -- nothing to do with the `message` also included in this.
  , payload :: msg
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance ToCBOR msg => ToCBOR (ReliableMsg msg) where
  toCBOR ReliableMsg{knownMessageIds, payload} = toCBOR knownMessageIds <> toCBOR payload

instance FromCBOR msg => FromCBOR (ReliableMsg msg) where
  fromCBOR = ReliableMsg <$> fromCBOR <*> fromCBOR

instance ToCBOR msg => SignableRepresentation (ReliableMsg msg) where
  getSignableRepresentation = serialize'

-- | Log entries specific to this network layer.
--
-- __NOTE__: Log items are documented in a YAML schema file which is not
-- currently public, but should be.
data ReliabilityLog
  = Resending {missing :: Vector Int, acknowledged :: Vector Int, localCounter :: Vector Int, theirIndex :: Int}
  | BroadcastCounter {ourIndex :: Int, localCounter :: Vector Int}
  | BroadcastPing {ourIndex :: Int, localCounter :: Vector Int}
  | Received {acknowledged :: Vector Int, localCounter :: Vector Int, theirIndex :: Int, ourIndex :: Int}
  | Ignored {acknowledged :: Vector Int, localCounter :: Vector Int, theirIndex :: Int, ourIndex :: Int}
  | ReliabilityFailedToFindMsg
      { missingMsgIndex :: Int
      , sentMessagesLength :: Int
      , knownAckForUs :: Int
      , messageAckForUs :: Int
      }
  | ReliabilityMissingPartyIndex {missingParty :: Party}
  | ReceivedMalformedAcks
      { fromParty :: Party
      , partyAcks :: Vector Int
      , numberOfParties :: Int
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance Arbitrary ReliabilityLog where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- | Handle for all persistence operations in the Reliability network layer.
-- This handle takes care of storing and retreiving vector clock and all
-- messages.
data MessagePersistence m msg = MessagePersistence
  { loadAcks :: m (Vector Int)
  , saveAcks :: Vector Int -> m ()
  , loadMessages :: m [Heartbeat msg]
  , appendMessage :: Heartbeat msg -> m ()
  }

-- | Create 'MessagePersistence' out of 'PersistenceIncremental' and
-- 'Persistence' handles. This handle loads and saves acks (vector clock data)
-- and can load and append network messages.
-- On start we construct empty ack vector from all parties in case nothing
-- is stored on disk.
-- NOTE: This handle is returned in the underlying context just for the sake of
-- convenience.
mkMessagePersistence ::
  (MonadThrow m, FromJSON msg, ToJSON msg) =>
  Int ->
  PersistenceIncremental (Heartbeat msg) m ->
  Persistence (Vector Int) m ->
  MessagePersistence m msg
mkMessagePersistence numberOfParties msgPersistence ackPersistence =
  MessagePersistence
    { loadAcks = do
        macks <- load ackPersistence
        case macks of
          Nothing -> pure $ replicate numberOfParties 0
          Just acks -> pure acks
    , saveAcks = \acks -> do
        save ackPersistence acks
    , loadMessages = do
        loadAll msgPersistence
    , appendMessage = \msg -> do
        append msgPersistence msg
    }

-- | Middleware function to handle message counters tracking and resending logic.
--
-- '''NOTE''': There is some "abstraction leak" here, because the `withReliability`
-- layer is tied to a specific structure of other layers, eg. be between
-- `withHeartbeat` and `withAuthenticate` layers.
--
-- NOTE: Make better use of Vectors? We should perhaps use a `MVector` to be able to
-- mutate in-place and not need `zipWith`
withReliability ::
  (MonadThrow (STM m), MonadThrow m, MonadAsync m) =>
  -- | Tracer for logging messages.
  Tracer m ReliabilityLog ->
  -- | Our persistence handle
  MessagePersistence m outbound ->
  -- | Our own party identifier.
  Party ->
  -- | Other parties' identifiers.
  [Party] ->
  -- | Underlying network component providing consuming and sending channels.
  NetworkComponent m (Authenticated (ReliableMsg (Heartbeat inbound))) (ReliableMsg (Heartbeat outbound)) a ->
  NetworkComponent m (Authenticated (Heartbeat inbound)) (Heartbeat outbound) a
withReliability tracer MessagePersistence{saveAcks, loadAcks, loadMessages} me otherParties withRawNetwork callback action = do
  acksCache <- loadAcks >>= newTVarIO
  sentMessages <- loadMessages >>= newTVarIO . Seq.fromList
  resendQ <- newTQueueIO
  let ourIndex = fromMaybe (error "This cannot happen because we constructed the list with our party inside.") (findPartyIndex me)
  let resend = writeTQueue resendQ
  withRawNetwork (reliableCallback acksCache sentMessages resend ourIndex) $ \network@Network{broadcast} -> do
    withAsync (forever $ atomically (readTQueue resendQ) >>= broadcast) $ \_ ->
      reliableBroadcast sentMessages ourIndex acksCache network
 where
  NetworkCallback{deliver} = callback

  allParties = fromList $ sort $ me : otherParties

  reliableBroadcast _sentMessages ourIndex acksCache Network{broadcast} =
    action $
      Network
        { broadcast = \msg ->
            case msg of
              Data{} -> do
                -- localCounter <- atomically $ cacheMessage msg >> incrementAckCounter
                -- saveAcks localCounter
                -- appendMessage msg
                localCounter <- atomically $ do
                  incrementAckCounter
                traceWith tracer BroadcastCounter{ourIndex, localCounter}
                broadcast $ ReliableMsg localCounter msg
              Ping{} -> do
                localCounter <- readTVarIO acksCache
                saveAcks localCounter
                traceWith tracer BroadcastPing{ourIndex, localCounter}
                broadcast $ ReliableMsg localCounter msg
        }
   where
    incrementAckCounter = do
      acks <- readTVar acksCache
      let newAcks = constructAcks acks ourIndex
      writeTVar acksCache newAcks
      pure newAcks

  -- cacheMessage msg =
  --   modifyTVar' sentMessages (|> msg)

  reliableCallback acksCache sentMessages resend ourIndex =
    NetworkCallback $ \(Authenticated (ReliableMsg acknowledged payload) party) -> do
      if length acknowledged /= length allParties
        then
          traceWith
            tracer
            ReceivedMalformedAcks
              { fromParty = party
              , partyAcks = acknowledged
              , numberOfParties = length allParties
              }
        else do
          eShouldCallbackWithKnownAcks <- atomically $ runMaybeT $ do
            loadedAcks <- lift $ readTVar acksCache
            partyIndex <- hoistMaybe $ findPartyIndex party
            messageAckForParty <- hoistMaybe (acknowledged !? partyIndex)
            knownAckForParty <- hoistMaybe $ loadedAcks !? partyIndex
            if
              | isPing payload ->
                  -- we do not update indices on Pings but we do propagate them
                  return (True, partyIndex, loadedAcks)
              | messageAckForParty == knownAckForParty + 1 -> do
                  -- we update indices for next in line messages and propagate them
                  let newAcks = constructAcks loadedAcks partyIndex
                  lift $ writeTVar acksCache newAcks
                  return (True, partyIndex, newAcks)
              | otherwise ->
                  -- other messages are dropped
                  return (False, partyIndex, loadedAcks)

          case eShouldCallbackWithKnownAcks of
            Just (shouldCallback, theirIndex, localCounter) -> do
              if shouldCallback
                then do
                  deliver Authenticated{payload, party}
                  traceWith tracer Received{acknowledged, localCounter, theirIndex, ourIndex}
                else traceWith tracer Ignored{acknowledged, localCounter, theirIndex, ourIndex}

              when (isPing payload) $
                resendMessagesIfLagging sentMessages resend theirIndex localCounter acknowledged ourIndex
            Nothing -> pure ()

  constructAcks acks wantedIndex =
    zipWith (\ack i -> if i == wantedIndex then ack + 1 else ack) acks partyIndexes

  partyIndexes = generate (length allParties) id

  resendMessagesIfLagging sentMessages resend theirIndex knownAcks acknowledged myIndex = do
    let mmessageAckForUs = acknowledged !? myIndex
    let mknownAckForUs = knownAcks !? myIndex
    case (mmessageAckForUs, mknownAckForUs) of
      (Just messageAckForUs, Just knownAckForUs) ->
        -- We resend messages if our peer notified us that it's lagging behind our
        -- latest message sent
        when (messageAckForUs < knownAckForUs) $ do
          let missing = fromList [messageAckForUs + 1 .. knownAckForUs]
          storedMessages <- readTVarIO sentMessages
          let messages = IMap.fromList (zip [1 ..] $ toList storedMessages)
          forM_ missing $ \idx -> do
            case messages IMap.!? idx of
              Nothing ->
                traceWith tracer $
                  ReliabilityFailedToFindMsg
                    { missingMsgIndex = idx
                    , sentMessagesLength = IMap.size messages
                    , knownAckForUs = knownAckForUs
                    , messageAckForUs = messageAckForUs
                    }
              Just missingMsg -> do
                let localCounter = zipWith (\ack i -> if i == myIndex then idx else ack) knownAcks partyIndexes
                traceWith tracer Resending{missing, acknowledged, localCounter, theirIndex}
                atomically $ resend $ ReliableMsg localCounter missingMsg
      _ -> pure ()

  -- Find the index of a party in the list of all parties.
  -- NOTE: This should never fail.
  findPartyIndex party =
    elemIndex party allParties
