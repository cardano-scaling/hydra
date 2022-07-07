module Hydra.Cluster.Scenarios where

import Hydra.Prelude

import CardanoClient (queryTip)
import CardanoNode (RunningNode (RunningNode))
import Hydra.Cardano.Api (NetworkId)
import qualified Hydra.Cardano.Api as Api
import Hydra.Cluster.Fixture (KnownNetwork (Testnet, VasilTestnet))

-- TODO: Determine networkId instead in 'withCardanoNodeOnKnownNetwork' and pass via RunningNode
knownNetworkId :: KnownNetwork -> NetworkId
knownNetworkId = \case
  Testnet -> Api.Testnet (Api.NetworkMagic 1097911063)
  VasilTestnet -> Api.Testnet (Api.NetworkMagic 9)

-- TODO: The 'RunningNode' should convey the networkId
singlePartyHeadFullLifeCycle :: NetworkId -> RunningNode -> IO ()
singlePartyHeadFullLifeCycle networkId (RunningNode _ nodeSocket) = do
  print =<< queryTip networkId nodeSocket
