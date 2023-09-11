module Hydra.Network.Reliability where

import Hydra.Prelude

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
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Sequence ((!?), (|>))
import Hydra.Logging (traceWith)
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Party (Party)

data Msg msg = Msg
  { messageId :: [Int]
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

withReliability ::
  forall m msg a.
  (MonadAsync m) =>
  Tracer m ReliabilityLog ->
  Party ->
  [Party] ->
  NetworkComponent m (Authenticated (Msg msg)) (Authenticated (Msg msg)) a ->
  NetworkComponent m (Authenticated msg) (Authenticated msg) a
withReliability tracer us allParties withRawNetwork callback action = do
  broadcastCounter <- newTVarIO $ replicate (length allParties) 0
  sentMessages <- newTVarIO mempty
  resendQ <- newTQueueIO
  let resend = writeTQueue resendQ
  withRawNetwork (reliableCallback broadcastCounter sentMessages resend) $ \network@Network{broadcast} -> do
    withAsync (forever $ atomically (readTQueue resendQ) >>= broadcast) $ \_ ->
      reliableBroadcast broadcastCounter sentMessages network
 where
  reliableBroadcast messageCounter sentMessages Network{broadcast} =
    action $
      Network
        { broadcast = \(Authenticated msg _) -> do
            counter <- atomically $ do
              acks <- readTVar messageCounter
              let ourIndex = fromJust $ List.elemIndex us allParties
              let newAcks = zipWith (\ack i -> if i == ourIndex then ack + 1 else ack) acks [0 ..]
              writeTVar messageCounter newAcks
              modifyTVar' sentMessages (|> msg)
              readTVar messageCounter

            traceWith tracer (BroadcastCounter counter)

            broadcast $ Authenticated (Msg counter msg) us
        }

  reliableCallback broadcastCounter sentMessages resend (Authenticated (Msg acks msg) party) = do
    let partyIndex = fromJust $ List.elemIndex party allParties
    let n = acks List.!! partyIndex
    counter <- readTVarIO broadcastCounter
    let count = (List.!! partyIndex) counter

    -- handle message from party iff it's next in line
    when (n == count + 1) $ do
      let newAcks = zipWith (\ack i -> if i == partyIndex then ack + 1 else ack) counter [0 ..]
      atomically $ writeTVar broadcastCounter newAcks
      callback (Authenticated msg party)

    -- resend messages if party did not acknowledge our latest idx
    let myIndex = fromJust $ List.elemIndex us allParties
    let acked = acks List.!! myIndex
    let latestMsg = (List.!! myIndex) counter
    when (acked < latestMsg) $ do
      let missing = [acked + 1 .. latestMsg]
      traceWith tracer (Resending missing acks counter party)
      atomically $ do
        messages <- readTVar sentMessages
        forM_ missing $ \idx -> do
          case messages !? (idx - 1) of
            Nothing ->
              error $
                "FIXME: this should never happen, there's no sent message at index "
                  <> show idx
                  <> ", messages length = "
                  <> show (length messages)
                  <> ", latest: "
                  <> show latestMsg
                  <> ", acked: "
                  <> show acked
            Just missingMsg -> do
              let counter' = zipWith (\ack i -> if i == myIndex then idx else ack) counter [0 ..]
              resend $ Authenticated (Msg counter' missingMsg) us

data ReliabilityLog
  = Resending {missing :: [Int], acknowledged :: [Int], localCounter :: [Int], party :: Party}
  | BroadcastCounter {localCounter :: [Int]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ReliabilityLog where
  arbitrary = genericArbitrary
