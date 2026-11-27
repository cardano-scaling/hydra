module Hydra.Node.Input where

import Hydra.Prelude

import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Tx.Party (Party)

-- | Create a network input with the default retry limit for its message.
mkNetworkInput :: Party -> Message tx -> Input tx
mkNetworkInput sender msg =
  case msg of
    ReqTx{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    ReqDec{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    _ -> NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

-- | Maximum retries for network messages awaiting protocol progress.
defaultTTL :: TTL
defaultTTL = 6000

-- | Maximum retries for 'ReqTx' and 'ReqDec' messages.
defaultTxTTL :: TTL
defaultTxTTL = 5
