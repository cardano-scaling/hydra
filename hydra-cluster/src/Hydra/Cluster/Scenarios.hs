{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Cluster.Scenarios where

import Hydra.Prelude

import CardanoClient (queryTip)
import CardanoNode (RunningNode (..))
import Control.Lens ((^?))
import Data.Aeson (object, (.=))
import Data.Aeson.Lens (key, _JSON)
import qualified Data.Set as Set
import Hydra.Cardano.Api (Lovelace, TxId, selectLovelace)
import Hydra.Cluster.Faucet (Marked (Fuel), queryMarkedUTxO, seedFromFaucet)
import Hydra.Cluster.Fixture (Actor (Alice), actorName, alice, aliceSk)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Ledger (IsTx (balance))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (networkId, startChainFrom)
import HydraNode (EndToEndLog (..), input, output, send, waitFor, waitMatch, withHydraNode)

singlePartyHeadFullLifeCycle ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
singlePartyHeadFullLifeCycle tracer workDir node@RunningNode{networkId} hydraScriptsTxId = do
  refuelIfNeeded tracer node Alice 100_000_000
  -- Start hydra-node on chain tip
  tip <- queryTip networkId nodeSocket
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket []
      <&> \config -> config{networkId, startChainFrom = Just tip}
  withHydraNode tracer aliceChainConfig workDir 1 aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
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
    deadline <- waitMatch 600 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsClosed"
      v ^? key "contestationDeadline" . _JSON
    -- Expect to see ReadyToFanout within 3 seconds after deadline
    remainingTime <- diffUTCTime deadline <$> getCurrentTime
    waitFor tracer (truncate $ remainingTime + 3) [n1] $
      output "ReadyToFanout" []
    send n1 $ input "Fanout" []
    waitFor tracer 600 [n1] $
      output "HeadIsFinalized" ["utxo" .= object mempty]
  traceRemainingFunds Alice
 where
  RunningNode{nodeSocket} = node

  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    (fuelUTxO, otherUTxO) <- queryMarkedUTxO node actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, fuelUTxO, otherUTxO}

-- | Refuel given 'Actor' with given 'Lovelace' if current marked UTxO is below that amount.
refuelIfNeeded ::
  Tracer IO EndToEndLog ->
  RunningNode ->
  Actor ->
  Lovelace ->
  IO ()
refuelIfNeeded tracer node actor amount = do
  (actorVk, _) <- keysFor actor
  (fuelUTxO, otherUTxO) <- queryMarkedUTxO node actorVk
  traceWith tracer $ StartingFunds{actor = actorName actor, fuelUTxO, otherUTxO}
  let fuelBalance = selectLovelace $ balance @Tx fuelUTxO
  when (fuelBalance < amount) $ do
    utxo <- seedFromFaucet node actorVk amount Fuel
    traceWith tracer $ RefueledFunds{actor = actorName actor, refuelingAmount = amount, fuelUTxO = utxo}
