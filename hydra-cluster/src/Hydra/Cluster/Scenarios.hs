module Hydra.Cluster.Scenarios where

import Hydra.Prelude

import CardanoClient (queryTip)
import CardanoNode (RunningNode (RunningNode))
import Hydra.Cardano.Api (NetworkId)

-- TODO: The 'RunningNode' should convey the networkId
singlePartyHeadFullLifeCycle :: NetworkId -> RunningNode -> IO ()
singlePartyHeadFullLifeCycle networkId (RunningNode _ nodeSocket) = do
  print =<< queryTip networkId nodeSocket
  threadDelay 3
  print =<< queryTip networkId nodeSocket
