module Hydra.Node.Input where

import Hydra.Prelude

import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Tx.Party (Party)

-- | Create a network input with corresponding default ttl from given sender.
mkNetworkInput :: Party -> Message tx -> Input tx
mkNetworkInput sender msg =
  case msg of
    ReqTx{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    ReqDec{} -> NetworkInput defaultTxTTL $ ReceivedMessage{sender, msg}
    _ -> NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

-- | The maximum number of times to re-enqueue a network messages upon 'Wait'.
-- outcome.
defaultTTL :: TTL
defaultTTL = 6000

-- | The maximum number of times to re-enqueue 'ReqTx' and 'ReqDec' network
-- messages upon 'Wait'.
defaultTxTTL :: TTL
defaultTxTTL = 5
