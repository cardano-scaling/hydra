-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct where

import Hydra.Chain (Chain (..), ChainComponent, OnChainTx, PostChainTx, toOnChainTx)
import Hydra.Logging (Tracer)
import Hydra.Prelude
import Ouroboros.Network.Channel (Channel)

withDirectChain ::
  IO (Channel IO LByteString) ->
  Tracer IO DirectChainLog ->
  ChainComponent tx IO ()
withDirectChain _connect _tracer callback action = do
  let chain =
        Chain
          { postTx = postTxToOnChainTx >=> callback
          }
  action chain
 where
  postTxToOnChainTx :: (MonadTime m) => PostChainTx tx -> m (OnChainTx tx)
  postTxToOnChainTx tx = do
    now <- getCurrentTime
    pure (toOnChainTx now tx)

data DirectChainLog
