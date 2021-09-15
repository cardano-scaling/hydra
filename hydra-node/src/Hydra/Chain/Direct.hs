-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct where

import Cardano.Binary (serialize)
import Hydra.Chain (Chain (..), ChainComponent, toOnChainTx)
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer)
import Hydra.Prelude
import Ouroboros.Network.Channel (Channel (Channel, send))

withDirectChain ::
  Tx tx =>
  IO (Channel IO LByteString) ->
  Tracer IO DirectChainLog ->
  ChainComponent tx IO ()
withDirectChain connect _tracer callback action = do
  chan <- connect
  action $ Chain{postTx = postTx chan}
 where
  postTx Channel{send} tx = do
    -- TODO(SN): convert 'postChainTx' to a Cardano tx and send it to the node
    send $ serialize ()
    now <- getCurrentTime
    callback (toOnChainTx now tx)

data DirectChainLog
