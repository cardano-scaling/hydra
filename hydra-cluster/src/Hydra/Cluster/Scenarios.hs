module Hydra.Cluster.Scenarios where

import Hydra.Prelude

import CardanoClient (queryTip)
import CardanoNode (RunningNode (RunningNode))
import Control.Lens ((^?))
import Data.Aeson (Value (Object), (.=))
import Data.Aeson.Lens (key, _Number)
import qualified Data.Set as Set
import Hydra.Cardano.Api (NetworkId)
import Hydra.Cluster.Faucet (Marked (Fuel), seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (Alice), alice, aliceSk)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Logging (Tracer)
import Hydra.Options (ChainConfig (startChainFrom))
import HydraNode (EndToEndLog, input, output, send, waitFor, waitMatch, withHydraNode)

-- TODO: The 'RunningNode' should convey the networkId
singlePartyHeadFullLifeCycle :: Tracer IO EndToEndLog -> FilePath -> NetworkId -> RunningNode -> IO ()
singlePartyHeadFullLifeCycle tracer workDir networkId node@(RunningNode _ nodeSocket) = do
  (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
  -- TODO: only re-fuel if needed
  seedFromFaucet_ networkId node aliceCardanoVk 100_000_000 Fuel

  -- Start hydra-node on chain tip
  tip <- queryTip networkId nodeSocket
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket []
      <&> \config -> config{startChainFrom = Just tip}
  withHydraNode tracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    let contestationPeriod = 1 :: Natural
    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
    waitFor tracer 600 [n1] $
      output "ReadyToCommit" ["parties" .= Set.fromList [alice]]
    -- Commit nothing for now
    send n1 $ input "Commit" ["utxo" .= Object mempty]
    waitFor tracer 600 [n1] $
      output "HeadIsOpen" ["utxo" .= Object mempty]

    send n1 $ input "Close" []
    remainingText <- waitMatch 600 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsClosed"
      v ^? key "remainingContestationPeriod" . _Number

    print remainingText
    -- TODO: parameterize waitFor accordingly here
    waitFor tracer 600 [n1] $
      output "ReadyToFanout" []
    send n1 $ input "Fanout" []
    waitFor tracer 600 [n1] $
      output "HeadIsFinalized" ["utxo" .= Object mempty]
