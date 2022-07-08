module Hydra.Cluster.Scenarios where

import Hydra.Prelude

import CardanoClient (queryTip)
import CardanoNode (RunningNode (RunningNode))
import Data.Aeson ((.=))
import qualified Data.Set as Set
import Hydra.Cardano.Api (NetworkId)
import Hydra.Cluster.Faucet (Marked (Fuel), seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (Alice), alice, aliceSk)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Logging (Tracer)
import HydraNode (EndToEndLog, input, output, send, waitFor, withHydraNode)

-- TODO: The 'RunningNode' should convey the networkId
singlePartyHeadFullLifeCycle :: Tracer IO EndToEndLog -> FilePath -> NetworkId -> RunningNode -> IO ()
singlePartyHeadFullLifeCycle tracer workDir networkId node@(RunningNode _ nodeSocket) = do
  (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
  aliceChainConfig <- chainConfigFor Alice workDir nodeSocket []
  withHydraNode tracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    seedFromFaucet_ networkId node aliceCardanoVk 100_000_000 Fuel
    _tip <- queryTip networkId nodeSocket
    let contestationPeriod = 1 :: Natural
    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
    waitFor tracer 10 [n1] $
      output "ReadyToCommit" ["parties" .= Set.fromList [alice]]
