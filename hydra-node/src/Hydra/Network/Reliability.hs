module Hydra.Network.Reliability where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, readTVarIO, writeTQueue),
  newTQueueIO,
  newTVarIO,
  writeTVar,
 )
import Control.Concurrent.Class.MonadSTM.TVar (modifyTVar)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence ((!?))
import Hydra.Network (Network (..), NetworkComponent)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Party (Party)
import Cardano.Binary (serialize')

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
  (MonadAsync m) =>
  Party ->
  [Party] ->
  NetworkComponent m (Authenticated (Msg msg)) (Authenticated (Msg msg)) a ->
  NetworkComponent m (Authenticated msg) (Authenticated msg) a
withReliability us allParties withRawNetwork callback action = do
  broadcastCounter <- newTVarIO $ replicate (length allParties) 0
  incomingCounter <- newTVarIO mempty
  sentMessages <- newTVarIO mempty
  resendQ <- newTQueueIO
  let resend = atomically . writeTQueue resendQ
  withRawNetwork (reliableCallback broadcastCounter incomingCounter sentMessages resend) $ \network@Network{broadcast} -> do
    withAsync (forever $ atomically (readTQueue resendQ) >>= broadcast) $ \_ ->
      reliableBroadcast broadcastCounter network
 where
  reliableBroadcast messageCounter Network{broadcast} =
    action $
      Network
        { broadcast = \(Authenticated msg _) -> do
            counter <- atomically $ do
              acks <- readTVar messageCounter
              let ourIndex = fromJust $ List.elemIndex us allParties
              let newAcks = zipWith (\ack i -> if i == ourIndex then ack + 1 else ack) acks [0 ..]
              writeTVar messageCounter newAcks
              readTVar messageCounter

            broadcast $ Authenticated (Msg counter msg) us
        }

  reliableCallback broadcastCounter messageCounter sentMessages resend (Authenticated (Msg acks msg) party) = do
    let partyIndex = fromJust $ List.elemIndex party allParties
    let n = acks List.!! partyIndex
    count <- fromMaybe 0 . Map.lookup party <$> readTVarIO messageCounter

    -- handle message from party iff it's next in line
    when (n == count + 1) $ do
      atomically $ modifyTVar messageCounter (Map.insert party n)
      callback (Authenticated msg party)

    -- resend messages if party did not acknowledge our latest idx
    let myIndex = fromJust $ List.elemIndex us allParties
    let acked = acks List.!! myIndex
    counter <- readTVarIO broadcastCounter
    let latestMsg = (List.!! myIndex) counter
    when (acked < latestMsg) $ do
      let missing = [acked + 1 .. latestMsg]
      messages <- readTVarIO sentMessages
      forM_ missing $ \idx -> do
        let missingMsg = fromJust $ messages !? idx
        let counter' = zipWith (\ack i -> if i == myIndex then idx else ack) counter [0 ..]
        resend $ Authenticated (Msg counter' missingMsg) us
