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
import Hydra.Cluster.Faucet (Marked (Fuel), queryMarkedUTxO, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Ledger (IsTx (balance))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (networkId, startChainFrom)
import HydraNode (EndToEndLog (..), input, output, send, waitFor, waitMatch, withHydraNode)

restartedNodeCanAbortOnPreviousTx :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> TxId -> IO ()
restartedNodeCanAbortOnPreviousTx tracer workDir cardanoNode hydraScriptsTxId = do
  let clients = [Alice, Bob]
  -- forM_ clients (\actor -> refuelIfNeeded tracer cardanoNode actor 100_000_000)
  [(aliceCardanoVk, _), (bobCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
  seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket [Bob]
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> \config -> config{networkId, startChainFrom = Nothing}

  bobChainConfig <-
    chainConfigFor Bob workDir nodeSocket [Alice]
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> \config -> config{networkId, startChainFrom = Nothing}

  withHydraNode tracer bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] hydraScriptsTxId $ \n1 -> do
    let contestationPeriod = 1 :: Natural
    withHydraNode tracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] hydraScriptsTxId $ \n2 -> do
      send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
      -- XXX: might need to tweak the wait time
      waitFor tracer 10 [n1, n2] $
        output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob]]

    -- n1 does a commit while n2 is down
    send n1 $ input "Commit" ["utxo" .= object mempty]
    waitFor tracer 10 [n1] $
      output "Committed" ["party" .= bob, "utxo" .= object mempty]

    -- n2 is back and can abort
    withHydraNode tracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] hydraScriptsTxId $ \n2 -> do
      waitFor tracer 10 [n2] $
        output "Committed" ["party" .= bob, "utxo" .= object mempty]

      send n2 $ input "Abort" []
      waitFor tracer 10 [n1, n2] $
        output "HeadIsAborted" ["utxo" .= object mempty]
 where
  RunningNode{nodeSocket, networkId} = cardanoNode

restartedNodeCanAbort :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> TxId -> IO ()
restartedNodeCanAbort tracer workDir cardanoNode hydraScriptsTxId = do
  refuelIfNeeded tracer cardanoNode Alice 100_000_000
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket []
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> \config -> config{networkId, startChainFrom = Nothing}

  withHydraNode tracer aliceChainConfig workDir 1 aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
    let contestationPeriod = 1 :: Natural
    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
    -- XXX: might need to tweak the wait time
    waitFor tracer 10 [n1] $
      output "ReadyToCommit" ["parties" .= Set.fromList [alice]]

  withHydraNode tracer aliceChainConfig workDir 1 aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
    send n1 $ input "Abort" []
    waitFor tracer 10 [n1] $
      output "HeadIsAborted" ["utxo" .= object mempty]
 where
  RunningNode{nodeSocket, networkId} = cardanoNode

-- | Step through the full life cycle of a Hydra Head with only a single
-- participant. This scenario is also used by the smoke test run via the
-- `hydra-cluster` executable.
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
    -- Expect to see ReadyToFanout within 60 seconds after deadline.
    -- XXX: We still would like to have a network-specific time here
    remainingTime <- diffUTCTime deadline <$> getCurrentTime
    waitFor tracer (truncate $ remainingTime + 60) [n1] $
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
    utxo <- seedFromFaucet node actorVk amount Fuel (contramap FromFaucet tracer)
    traceWith tracer $ RefueledFunds{actor = actorName actor, refuelingAmount = amount, fuelUTxO = utxo}
