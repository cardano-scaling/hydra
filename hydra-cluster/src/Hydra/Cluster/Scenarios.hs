{-# LANGUAGE TypeApplications #-}

module Hydra.Cluster.Scenarios where

import Hydra.Prelude

import CardanoClient (queryTip)
import CardanoNode (RunningNode (RunningNode))
import Control.Lens ((^?))
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, _Number)
import qualified Data.Set as Set
import Hydra.Cardano.Api (Lovelace, NetworkId, selectLovelace)
import Hydra.Cluster.Faucet (Marked (Fuel), queryMarkedUTxO, seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (Alice), actorName, alice, aliceSk)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Ledger (IsTx (balance))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (networkId, startChainFrom)
import HydraNode (EndToEndLog (..), input, output, send, waitFor, waitMatch, withHydraNode)

-- TODO: The 'RunningNode' should convey the networkId
singlePartyHeadFullLifeCycle ::
  Tracer IO EndToEndLog ->
  FilePath ->
  NetworkId ->
  RunningNode ->
  IO ()
singlePartyHeadFullLifeCycle tracer workDir networkId node = do
  refuelIfNeeded tracer networkId node Alice 100_000_000
  -- Start hydra-node on chain tip
  tip <- queryTip networkId nodeSocket
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket []
      <&> \config -> config{networkId, startChainFrom = Just tip}
  withHydraNode tracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Initialize & open head
    let contestationPeriod = 1 :: Natural
    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
    waitFor tracer 600 [n1] $
      output "ReadyToCommit" ["parties" .= Set.fromList [alice]]
    -- Commit nothing for now
    send n1 $ input "Commit" ["utxo" .= object mempty]
    waitFor tracer 600 [n1] $
      output "HeadIsOpen" ["utxo" .= object mempty]
    -- Close head
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
      output "HeadIsFinalized" ["utxo" .= object mempty]
  traceRemainingFunds Alice
 where
  (RunningNode _ nodeSocket) = node

  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    (fuelUTxO, otherUTxO) <- queryMarkedUTxO networkId node actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, fuelUTxO, otherUTxO}

-- | Refuel given 'Actor' with given 'Lovelace' if current marked UTxO is below that amount.
refuelIfNeeded ::
  Tracer IO EndToEndLog ->
  NetworkId ->
  RunningNode ->
  Actor ->
  Lovelace ->
  IO ()
refuelIfNeeded tracer networkId node actor amount = do
  (actorVk, _) <- keysFor actor
  (fuelUTxO, otherUTxO) <- queryMarkedUTxO networkId node actorVk
  traceWith tracer $ StartingFunds{actor = actorName actor, fuelUTxO, otherUTxO}
  let fuelBalance = selectLovelace $ balance @Tx fuelUTxO
  when (fuelBalance < amount) $ do
    utxo <- seedFromFaucet networkId node actorVk amount Fuel
    traceWith tracer $ RefueledFunds{actor = actorName actor, refuelingAmount = amount, fuelUTxO = utxo}
