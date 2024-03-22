{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (..),
  RunningNode (..),
  queryEpochNo,
  queryGenesisParameters,
  queryTip,
  queryTipSlotNo,
  submitTx,
  waitForUTxO,
 )
import CardanoNode (
  CardanoNodeArgs (..),
  forkIntoConwayInEpoch,
  setupCardanoDevnet,
  unsafeDecodeJsonFile,
  withCardanoNode,
  withCardanoNodeDevnet,
 )
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Lens ((^..), (^?))
import Data.Aeson (Result (..), Value (Null, Object, String), fromJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (AsJSON (_JSON), key, values, _Double, _JSON)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (secondsToDiffTime)
import Hydra.Cardano.Api hiding (Value, cardanoEra, queryGenesisParameters)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.State ()
import Hydra.Cluster.Faucet (
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Alice, Bob, Carol, Faucet),
  alice,
  aliceSk,
  aliceVk,
  bob,
  bobSk,
  bobVk,
  carol,
  carolSk,
  carolVk,
  cperiod,
 )
import Hydra.Cluster.Scenarios (
  EndToEndLog (..),
  canCloseWithLongContestationPeriod,
  canDecommit,
  canFanoutWithDecommitRecorded,
  canSubmitTransactionThroughAPI,
  headIsInitializingWith,
  initWithWrongKeys,
  refuelIfNeeded,
  restartedNodeCanAbort,
  restartedNodeCanObserveCommitTx,
  singlePartyCommitsFromExternal,
  singlePartyCommitsFromExternalTxBlueprint,
  singlePartyHeadFullLifeCycle,
  testPreventResumeReconfiguredPeer,
  threeNodesNoErrorsOnOpen,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor, modifyConfig)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, genUTxOFor, mkRangedTx, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options
import HydraNode (
  HydraClient (..),
  getMetrics,
  getSnapshotUTxO,
  input,
  output,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
  withHydraNode',
 )
import System.Directory (removeDirectoryRecursive)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO (
  hGetContents,
  hGetLine,
 )
import System.IO.Error (isEOFError)
import System.Process (waitForProcess)
import Test.QuickCheck (generate)
import Prelude qualified

allNodeIds :: [Int]
allNodeIds = [1 .. 3]

-- | Like 'withTempDir', but using a common template to archive logs more easily
-- on CI.
--
-- NOTE: The ci-nix.yaml workflow depends on this.
withClusterTempDir :: MonadIO m => (FilePath -> m a) -> m a
withClusterTempDir = withTempDir "hydra-cluster"

spec :: Spec
spec = around (showLogsOnFailure "EndToEndSpec") $ do
  it "End-to-end offline mode" $ \tracer -> do
    withClusterTempDir $ \tmpDir -> do
      (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
      (bobCardanoVk, _) <- keysFor Bob
      initialUTxO <- generate $ do
        a <- genUTxOFor aliceCardanoVk
        b <- genUTxOFor bobCardanoVk
        pure $ a <> b
      Aeson.encodeFile (tmpDir </> "utxo.json") initialUTxO
      let offlineConfig =
            Offline
              OfflineChainConfig
                { initialUTxOFile = tmpDir </> "utxo.json"
                , ledgerGenesisFile = Nothing
                }
      -- Start a hydra-node in offline mode and submit a transaction from alice to bob
      aliceToBob <- withHydraNode (contramap FromHydraNode tracer) offlineConfig tmpDir 1 aliceSk [] [1] $ \node -> do
        let Just (aliceSeedTxIn, aliceSeedTxOut) = UTxO.find (isVkTxOut aliceCardanoVk) initialUTxO
        let Right aliceToBob =
              mkSimpleTx
                (aliceSeedTxIn, aliceSeedTxOut)
                (mkVkAddress testNetworkId bobCardanoVk, txOutValue aliceSeedTxOut)
                aliceCardanoSk
        send node $ input "NewTx" ["transaction" .= aliceToBob]
        waitMatch 10 node $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        pure aliceToBob

      -- Restart a hydra-node in offline mode expect we can reverse the transaction (it retains state)
      withHydraNode (contramap FromHydraNode tracer) offlineConfig tmpDir 1 aliceSk [] [1] $ \node -> do
        let
          bobTxOut = toUTxOContext $ List.head (txOuts' aliceToBob)
          Right bobToAlice =
            mkSimpleTx
              (mkTxIn aliceToBob 0, bobTxOut)
              (mkVkAddress testNetworkId bobCardanoVk, txOutValue bobTxOut)
              aliceCardanoSk
        send node $ input "NewTx" ["transaction" .= bobToAlice]
        waitMatch 10 node $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"

  describe "End-to-end on Cardano devnet" $ do
    describe "single party hydra head" $ do
      it "full head life-cycle" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyHeadFullLifeCycle tracer tmpDir node
      it "can close with long deadline" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= canCloseWithLongContestationPeriod tracer tmpDir node
      it "can submit a timed tx" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= timedTx tmpDir tracer node
      it "commits from external with utxo" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyCommitsFromExternal tracer tmpDir node
      it "can submit a signed user transaction" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= canSubmitTransactionThroughAPI tracer tmpDir node
      it "commits from external with tx blueprint" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyCommitsFromExternalTxBlueprint tracer tmpDir node
      it "can decommit utxo" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= canDecommit tracer tmpDir node
      it "can fanout after decommit" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= canFanoutWithDecommitRecorded tracer tmpDir node

    describe "three hydra nodes scenario" $ do
      it "does not error when all nodes open the head concurrently" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              publishHydraScriptsAs node Faucet
                >>= threeNodesNoErrorsOnOpen tracer tmpDir node

      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              initAndClose tmpDir tracer 1 hydraScriptsTxId node

      it "inits a Head and closes it immediately" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            let clusterIx = 0
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
              aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
              bobKeys@(bobCardanoVk, _) <- generate genKeyPair
              carolKeys@(carolCardanoVk, _) <- generate genKeyPair

              let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
                  hydraKeys = [aliceSk, bobSk, carolSk]

              let firstNodeId = clusterIx * 3

              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              let contestationPeriod = UnsafeContestationPeriod 2
              let hydraTracer = contramap FromHydraNode tracer
              withHydraCluster hydraTracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
                let [n1, n2, n3] = toList nodes
                waitForNodesConnected hydraTracer 20 [n1, n2, n3]

                -- Funds to be used as fuel by Hydra protocol transactions
                seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
                seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
                seedFromFaucet_ node carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

                send n1 $ input "Init" []
                headId <-
                  waitForAllMatch 10 [n1, n2, n3] $
                    headIsInitializingWith (Set.fromList [alice, bob, carol])

                -- Get some UTXOs to commit to a head
                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
                committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)
                requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

                (bobExternalVk, bobExternalSk) <- generate genKeyPair
                committedUTxOByBob <- seedFromFaucet node bobExternalVk bobCommittedToHead (contramap FromFaucet tracer)
                requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk >>= submitTx node

                requestCommitTx n3 mempty >>= submitTx node

                let u0 = committedUTxOByAlice <> committedUTxOByBob

                waitFor hydraTracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= u0, "headId" .= headId]

                send n1 $ input "Close" []
                deadline <- waitMatch 3 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  guard $ v ^? key "headId" == Just (toJSON headId)
                  snapshotNumber <- v ^? key "snapshotNumber"
                  guard $ snapshotNumber == Aeson.Number 0
                  v ^? key "contestationDeadline" . _JSON

                -- Expect to see ReadyToFanout within 3 seconds after deadline
                remainingTime <- diffUTCTime deadline <$> getCurrentTime
                waitFor hydraTracer (remainingTime + 3) [n1] $
                  output "ReadyToFanout" ["headId" .= headId]

                send n1 $ input "Fanout" []
                waitFor hydraTracer 3 [n1] $
                  output "HeadIsFinalized" ["utxo" .= u0, "headId" .= headId]

    describe "restarting nodes" $ do
      it "can abort head after restart" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= restartedNodeCanAbort tracer tmpDir node

      it "can observe a commit tx after a restart, even when a tx happened while down" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= restartedNodeCanObserveCommitTx tracer tmpDir node

      it "prevent resuming a head after reconfiguring a peer" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= testPreventResumeReconfiguredPeer tracer tmpDir node

      it "can start chain from the past and replay on-chain events" $ \tracer ->
        withClusterTempDir $ \tmp ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            let contestationPeriod = UnsafeContestationPeriod 10
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] contestationPeriod
            let nodeId = 1
            let hydraTracer = contramap FromHydraNode tracer
            (tip, aliceHeadId) <- withHydraNode hydraTracer aliceChainConfig tmp nodeId aliceSk [] [1] $ \n1 -> do
              seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
              tip <- queryTip networkId nodeSocket
              send n1 $ input "Init" []
              headId <- waitForAllMatch 10 [n1] $ headIsInitializingWith (Set.fromList [alice])
              return (tip, headId)

            -- REVIEW: Do we want to keep this --start-chain-from feature or
            -- replace it with an event source load from persistence?

            -- NOTE: Need to clear persistence as we would load the state and
            -- not resynchronize from chain
            removeDirectoryRecursive $ tmp </> "state-" <> show nodeId

            let aliceChainConfig' = aliceChainConfig & modifyConfig (\cfg -> cfg{startChainFrom = Just tip})
            withHydraNode hydraTracer aliceChainConfig' tmp 1 aliceSk [] [1] $ \n1 -> do
              headId' <- waitForAllMatch 10 [n1] $ headIsInitializingWith (Set.fromList [alice])
              headId' `shouldBe` aliceHeadId

      it "close of an initial snapshot from re-initialized node is contested" $ \tracer ->
        withClusterTempDir $ \tmp ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet

            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            (bobCardanoVk, _bobCardanoSk) <- keysFor Bob

            seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
            seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

            tip <- queryTip networkId nodeSocket
            let startFromTip = modifyConfig $ \x -> x{startChainFrom = Just tip}
            let contestationPeriod = UnsafeContestationPeriod 10
            aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [Bob] contestationPeriod <&> startFromTip
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket hydraScriptsTxId [Alice] contestationPeriod <&> startFromTip

            let hydraTracer = contramap FromHydraNode tracer
            let aliceNodeId = 1
                bobNodeId = 2
                allNodesIds = [aliceNodeId, bobNodeId]
                withAliceNode :: (HydraClient -> IO a) -> IO a
                withAliceNode = withHydraNode hydraTracer aliceChainConfig tmp aliceNodeId aliceSk [bobVk] allNodesIds
                withBobNode :: (HydraClient -> IO a) -> IO a
                withBobNode = withHydraNode hydraTracer bobChainConfig tmp bobNodeId bobSk [aliceVk] allNodesIds

            withAliceNode $ \n1 -> do
              headId <- withBobNode $ \n2 -> do
                waitForNodesConnected hydraTracer 20 [n1, n2]

                send n1 $ input "Init" []
                headId <- waitForAllMatch 10 [n1, n2] $ headIsInitializingWith (Set.fromList [alice, bob])

                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
                committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)
                requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

                (bobExternalVk, _bobExternalSk) <- generate genKeyPair
                requestCommitTx n2 mempty >>= submitTx node

                waitFor hydraTracer 10 [n1, n2] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice, "headId" .= headId]

                -- Create an arbitrary transaction using some input.
                let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
                let Right tx =
                      mkSimpleTx
                        firstCommittedUTxO
                        (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
                        aliceExternalSk
                send n1 $ input "NewTx" ["transaction" .= tx]

                waitMatch 10 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "SnapshotConfirmed"
                  guard $ v ^? key "headId" == Just (toJSON headId)

                return headId
              -- NOTE: Need to clear state on disk to have bob close with
              -- initial snapshot
              removeDirectoryRecursive $ tmp </> "state-" <> show bobNodeId

              -- HACK: We do re-use network ports and for some reason Hydra
              -- network port is not available right away.
              threadDelay 1
              withBobNode $ \n2 -> do
                waitMatch 10 n2 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsOpen"
                  guard $ v ^? key "headId" == Just (toJSON headId)

                send n2 $ input "Close" []

                let isHeadClosedWith0 v = do
                      guard $ v ^? key "tag" == Just "HeadIsClosed"
                      guard $ v ^? key "headId" == Just (toJSON headId)
                      snapshotNumber <- v ^? key "snapshotNumber"
                      guard $ snapshotNumber == toJSON (0 :: Word)

                waitMatch 10 n1 isHeadClosedWith0
                waitMatch 10 n2 isHeadClosedWith0

                forM_ [n1, n2] $ \n ->
                  waitMatch 10 n $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsContested"
                    guard $ v ^? key "headId" == Just (toJSON headId)

    describe "two hydra heads scenario" $ do
      it "two heads on the same network do not conflict" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              concurrently_
                (initAndClose tmpDir tracer 0 hydraScriptsTxId node)
                (initAndClose tmpDir tracer 1 hydraScriptsTxId node)

      it "alice inits a Head with incorrect keys preventing bob from observing InitTx" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              publishHydraScriptsAs node Faucet
                >>= initWithWrongKeys tmpDir tracer node

      it "bob cannot abort alice's head" $ \tracer -> do
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
              (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
              (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
              let contestationPeriod = UnsafeContestationPeriod 10
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] contestationPeriod
              bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
              let hydraTracer = contramap FromHydraNode tracer
              withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] allNodeIds $ \n1 ->
                withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [aliceVk] allNodeIds $ \n2 -> do
                  -- Funds to be used as fuel by Hydra protocol transactions
                  seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
                  seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

                  send n1 $ input "Init" []
                  headIdAliceOnly <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

                  -- Bob opens and immediately aborts a Head with Alice, iow pulls Alice in
                  -- "his" Head
                  send n2 $ input "Init" []
                  headIdAliceAndBob <- waitMatch 10 n2 $ headIsInitializingWith (Set.fromList [alice, bob])

                  send n2 $ input "Abort" []
                  waitFor hydraTracer 10 [n2] $
                    output "HeadIsAborted" ["utxo" .= Object mempty, "headId" .= headIdAliceAndBob]

                  -- Alice should be able to continue working with her Head
                  requestCommitTx n1 mempty >>= submitTx node
                  waitFor hydraTracer 10 [n1] $
                    output "HeadIsOpen" ["utxo" .= Object mempty, "headId" .= headIdAliceOnly]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          (aliceCardanoVk, _) <- keysFor Alice
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            let hydraTracer = contramap FromHydraNode tracer
            let contestationPeriod = UnsafeContestationPeriod 10
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [Bob, Carol] contestationPeriod
            bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [Alice, Carol] contestationPeriod
            carolChainConfig <- chainConfigFor Carol tmpDir nodeSocket hydraScriptsTxId [Alice, Bob] contestationPeriod
            failAfter 20 $
              withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [aliceVk, carolVk] allNodeIds $ \n2 ->
                  withHydraNode hydraTracer carolChainConfig tmpDir 3 carolSk [aliceVk, bobVk] allNodeIds $ \n3 -> do
                    -- Funds to be used as fuel by Hydra protocol transactions
                    seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
                    waitForNodesConnected hydraTracer 20 [n1, n2, n3]
                    send n1 $ input "Init" []
                    void $ waitForAllMatch 3 [n1] $ headIsInitializingWith (Set.fromList [alice, bob, carol])
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_inputs" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "logs its command line arguments" $ \tracer -> do
        withClusterTempDir $ \dir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) dir $ \node@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            chainConfig <- chainConfigFor Alice dir nodeSocket hydraScriptsTxId [] (UnsafeContestationPeriod 1)
            withHydraNode' hydraTracer chainConfig dir 1 aliceSk [] [1] Nothing $ \stdOut _ _processHandle -> do
              waitForLog 10 stdOut "JSON object with key NodeOptions" $ \line ->
                line ^? key "message" . key "tag" == Just (Aeson.String "NodeOptions")

      it "logs to a logfile" $ \tracer -> do
        withClusterTempDir $ \dir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) dir $ \node@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            refuelIfNeeded tracer node Alice 100_000_000
            let contestationPeriod = UnsafeContestationPeriod 2
            aliceChainConfig <- chainConfigFor Alice dir nodeSocket hydraScriptsTxId [] contestationPeriod
            withHydraNode hydraTracer aliceChainConfig dir 1 aliceSk [] [1] $ \n1 -> do
              send n1 $ input "Init" []

            let logFilePath = dir </> "logs" </> "hydra-node-1.log"
            logfile <- readFileBS logFilePath
            BS.length logfile `shouldSatisfy` (> 0)

    describe "forking eras" $ do
      it "does report on unsupported era" $ \tracer -> do
        pendingWith "Currently supporting Conway era no future upcoming"
        withClusterTempDir $ \tmpDir -> do
          args <- setupCardanoDevnet tmpDir
          forkIntoConwayInEpoch tmpDir args 1
          withCardanoNode (contramap FromCardanoNode tracer) tmpDir args $ \node@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            chainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
            withHydraNode' hydraTracer chainConfig tmpDir 1 aliceSk [] [1] Nothing $ \out stdErr ph -> do
              -- Assert nominal startup
              waitForLog 5 out "missing NodeOptions" (Text.isInfixOf "NodeOptions")

              waitUntilEpoch tmpDir args node 1

              waitForProcess ph `shouldReturn` ExitFailure 1
              errorOutputs <- hGetContents stdErr
              errorOutputs `shouldContain` "Received blocks in unsupported era"
              errorOutputs `shouldContain` "upgrade your hydra-node"

      it "does report on unsupported era on startup" $ \tracer -> do
        pendingWith "Currently supporting Conway era no future upcoming"
        withClusterTempDir $ \tmpDir -> do
          args <- setupCardanoDevnet tmpDir
          forkIntoConwayInEpoch tmpDir args 1
          withCardanoNode (contramap FromCardanoNode tracer) tmpDir args $ \node@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            chainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod

            waitUntilEpoch tmpDir args node 2

            withHydraNode' hydraTracer chainConfig tmpDir 1 aliceSk [] [1] Nothing $ \_out stdErr ph -> do
              waitForProcess ph `shouldReturn` ExitFailure 1
              errorOutputs <- hGetContents stdErr
              errorOutputs `shouldContain` "Connected to cardano-node in unsupported era"
              errorOutputs `shouldContain` "upgrade your hydra-node"

      it "support new era" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          args <- setupCardanoDevnet tmpDir

          forkIntoConwayInEpoch tmpDir args 10
          withCardanoNode (contramap FromCardanoNode tracer) tmpDir args $
            \node@RunningNode{nodeSocket} -> do
              let lovelaceBalanceValue = 100_000_000
              -- Funds to be used as fuel by Hydra protocol transactions
              (aliceCardanoVk, _) <- keysFor Alice
              seedFromFaucet_ node aliceCardanoVk lovelaceBalanceValue (contramap FromFaucet tracer)
              -- Get some UTXOs to commit to a head
              (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
              committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)

              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              chainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod

              let hydraTracer = contramap FromHydraNode tracer
              withHydraNode hydraTracer chainConfig tmpDir 1 aliceSk [] [1] $ \n1 -> do
                send n1 $ input "Init" []
                headId <- waitForAllMatch 10 [n1] $ headIsInitializingWith (Set.fromList [alice])

                requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

                waitFor hydraTracer 3 [n1] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice, "headId" .= headId]

                waitUntilEpoch tmpDir args node 10

                send n1 $ input "Close" []
                waitMatch 3 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  guard $ v ^? key "headId" == Just (toJSON headId)
                  snapshotNumber <- v ^? key "snapshotNumber"
                  guard $ snapshotNumber == Aeson.Number 0

      it "support new era on restart" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          args <- setupCardanoDevnet tmpDir

          forkIntoConwayInEpoch tmpDir args 10
          withCardanoNode (contramap FromCardanoNode tracer) tmpDir args $
            \node@RunningNode{nodeSocket} -> do
              let lovelaceBalanceValue = 100_000_000
              -- Funds to be used as fuel by Hydra protocol transactions
              (aliceCardanoVk, _) <- keysFor Alice
              seedFromFaucet_ node aliceCardanoVk lovelaceBalanceValue (contramap FromFaucet tracer)
              -- Get some UTXOs to commit to a head
              (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
              committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)

              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              chainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod

              let hydraTracer = contramap FromHydraNode tracer
              headId <- withHydraNode hydraTracer chainConfig tmpDir 1 aliceSk [] [1] $ \n1 -> do
                send n1 $ input "Init" []
                headId <- waitForAllMatch 10 [n1] $ headIsInitializingWith (Set.fromList [alice])

                requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

                waitFor hydraTracer 3 [n1] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice, "headId" .= headId]

                pure headId

              waitUntilEpoch tmpDir args node 10

              withHydraNode hydraTracer chainConfig tmpDir 1 aliceSk [] [1] $ \n1 -> do
                send n1 $ input "Close" []
                waitMatch 3 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  guard $ v ^? key "headId" == Just (toJSON headId)
                  snapshotNumber <- v ^? key "snapshotNumber"
                  guard $ snapshotNumber == Aeson.Number 0

-- | Wait until given number of epoch. This uses the epoch and slot lengths from
-- the 'ShelleyGenesisFile' of the node args passed in.
waitUntilEpoch :: FilePath -> CardanoNodeArgs -> RunningNode -> Natural -> IO ()
waitUntilEpoch stateDirectory args RunningNode{networkId, nodeSocket} toEpochNo = do
  fromEpochNo :: Natural <- fromIntegral . unEpochNo <$> queryEpochNo networkId nodeSocket QueryTip
  toEpochNo `shouldSatisfy` (> fromEpochNo)
  shelleyGenesisFile :: Aeson.Value <- unsafeDecodeJsonFile (stateDirectory </> nodeShelleyGenesisFile args)
  let slotLength =
        fromMaybe (error "Field epochLength not found") $
          shelleyGenesisFile ^? key "slotLength" . _Double
      epochLength =
        fromMaybe (error "Field epochLength not found") $
          shelleyGenesisFile ^? key "epochLength" . _Double
  threadDelay . realToFrac $ fromIntegral (toEpochNo - fromEpochNo) * epochLength * slotLength

waitForLog :: DiffTime -> Handle -> Text -> (Text -> Bool) -> IO ()
waitForLog delay nodeOutput failureMessage predicate = do
  seenLogs <- newTVarIO []
  timeout delay (go seenLogs) >>= \case
    Just () -> pure ()
    Nothing -> failReason seenLogs $ "within " <> show delay
 where
  go seenLogs = do
    tryJust (guard . isEOFError) (fromString <$> hGetLine nodeOutput) >>= \case
      Left _ ->
        failReason seenLogs "before EOF"
      Right log -> do
        atomically (modifyTVar' seenLogs (log :))
        if predicate log
          then pure ()
          else go seenLogs

  failReason seenLogs reason = do
    logs <- readTVarIO seenLogs
    failure . toString $
      unlines $
        [ "waitForLog did not match a log line " <> reason
        , "looking for: " <> failureMessage
        , "seen logs:"
        ]
          <> logs

timedTx :: FilePath -> Tracer IO EndToEndLog -> RunningNode -> TxId -> IO ()
timedTx tmpDir tracer node@RunningNode{networkId, nodeSocket} hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice
  let contestationPeriod = UnsafeContestationPeriod 2
  aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] contestationPeriod
  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \n1 -> do
    waitForNodesConnected hydraTracer 20 [n1]
    let lovelaceBalanceValue = 100_000_000

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ node aliceCardanoVk lovelaceBalanceValue (contramap FromFaucet tracer)
    send n1 $ input "Init" []
    headId <-
      waitForAllMatch 10 [n1] $
        headIsInitializingWith (Set.fromList [alice])

    -- Get some UTXOs to commit to a head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)
    requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

    waitFor hydraTracer 3 [n1] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice, "headId" .= headId]

    -- Acquire a current point in time
    genesisParams <- queryGenesisParameters networkId nodeSocket QueryTip
    let slotLengthSec = protocolParamSlotLength genesisParams
    currentSlot <- queryTipSlotNo networkId nodeSocket

    -- Create an arbitrary transaction using some input.
    let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice

    -- Create a transaction which is only valid in 5 seconds
    let secondsToAwait = 5
        slotsToAwait = SlotNo . truncate $ fromInteger secondsToAwait / slotLengthSec
        futureSlot = currentSlot + slotsToAwait
        lovelaceToSend = lovelaceBalanceValue - 90_000_000

        -- TODO (later) use time in a script (as it is using POSIXTime)
        Right tx =
          mkRangedTx
            firstCommittedUTxO
            (inHeadAddress aliceExternalVk, lovelaceToValue lovelaceToSend)
            aliceExternalSk
            (Just $ TxValidityLowerBound futureSlot, Nothing)

    -- First submission: invalid
    send n1 $ input "NewTx" ["transaction" .= tx]
    waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "TxInvalid"

    -- Wait for the future chain slot and time
    threadDelay $ secondsToDiffTime secondsToAwait

    -- Second submission: now valid
    send n1 $ input "NewTx" ["transaction" .= tx]
    waitFor hydraTracer 3 [n1] $
      output "TxValid" ["transaction" .= tx, "headId" .= headId]

    confirmedTransactions <- waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      v ^? key "snapshot" . key "confirmedTransactions"
    confirmedTransactions ^.. values `shouldBe` [toJSON $ txId tx]

initAndClose :: FilePath -> Tracer IO EndToEndLog -> Int -> TxId -> RunningNode -> IO ()
initAndClose tmpDir tracer clusterIx hydraScriptsTxId node@RunningNode{nodeSocket} = do
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
  bobKeys@(bobCardanoVk, _) <- generate genKeyPair
  carolKeys@(carolCardanoVk, _) <- generate genKeyPair

  let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
      hydraKeys = [aliceSk, bobSk, carolSk]

  let firstNodeId = clusterIx * 3
  let contestationPeriod = UnsafeContestationPeriod 2
  let hydraTracer = contramap FromHydraNode tracer
  withHydraCluster hydraTracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
    let [n1, n2, n3] = toList nodes
    waitForNodesConnected hydraTracer 20 [n1, n2, n3]

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ node carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send n1 $ input "Init" []
    headId <-
      waitForAllMatch 10 [n1, n2, n3] $
        headIsInitializingWith (Set.fromList [alice, bob, carol])

    -- Get some UTXOs to commit to a head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)
    requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

    (bobExternalVk, bobExternalSk) <- generate genKeyPair
    committedUTxOByBob <- seedFromFaucet node bobExternalVk bobCommittedToHead (contramap FromFaucet tracer)
    requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk >>= submitTx node

    requestCommitTx n3 mempty >>= submitTx node

    waitFor hydraTracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob), "headId" .= headId]

    -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
    let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
    let Right tx =
          mkSimpleTx
            firstCommittedUTxO
            (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
            aliceExternalSk
    send n1 $ input "NewTx" ["transaction" .= tx]
    waitFor hydraTracer 10 [n1, n2, n3] $
      output "TxValid" ["transaction" .= tx, "headId" .= headId]

    -- The expected new utxo set is the created payment to bob,
    -- alice's remaining utxo in head and whatever bot has
    -- committed to the head
    let newUTxO =
          Map.fromList
            [
              ( TxIn (txId tx) (toEnum 0)
              , object
                  [ "address" .= String (serialiseAddress $ inHeadAddress bobExternalVk)
                  , "value" .= object ["lovelace" .= int paymentFromAliceToBob]
                  , "datum" .= Null
                  , "datumhash" .= Null
                  , "inlineDatum" .= Null
                  , "referenceScript" .= Null
                  ]
              )
            ,
              ( TxIn (txId tx) (toEnum 1)
              , object
                  [ "address" .= String (serialiseAddress $ inHeadAddress aliceExternalVk)
                  , "value" .= object ["lovelace" .= int (aliceCommittedToHead - paymentFromAliceToBob)]
                  , "datum" .= Null
                  , "datumhash" .= Null
                  , "inlineDatum" .= Null
                  , "referenceScript" .= Null
                  ]
              )
            ]
            <> fmap toJSON (Map.fromList (UTxO.pairs committedUTxOByBob))

    let expectedSnapshotNumber :: Int = 1

    waitMatch 10 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshotNumber <- v ^? key "snapshot" . key "snapshotNumber"
      guard $ snapshotNumber == toJSON expectedSnapshotNumber
      utxo <- v ^? key "snapshot" . key "utxo"
      guard $ utxo == toJSON newUTxO
      confirmedTransactions <- v ^? key "snapshot" . key "confirmedTransactions"
      guard $ confirmedTransactions == toJSON [txId tx]

    (toJSON <$> getSnapshotUTxO n1) `shouldReturn` toJSON newUTxO

    send n1 $ input "Close" []
    deadline <- waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsClosed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshotNumber <- v ^? key "snapshotNumber"
      guard $ snapshotNumber == toJSON expectedSnapshotNumber
      v ^? key "contestationDeadline" . _JSON

    -- Expect to see ReadyToFanout within 3 seconds after deadline
    remainingTime <- diffUTCTime deadline <$> getCurrentTime
    waitFor hydraTracer (remainingTime + 3) [n1] $
      output "ReadyToFanout" ["headId" .= headId]

    send n1 $ input "Fanout" []
    waitFor hydraTracer 3 [n1] $
      output "HeadIsFinalized" ["utxo" .= newUTxO, "headId" .= headId]

    case fromJSON $ toJSON newUTxO of
      Error err ->
        failure $ "newUTxO isn't valid JSON?: " <> err
      Data.Aeson.Success u ->
        failAfter 5 $ waitForUTxO node u

-- * Fixtures

aliceCommittedToHead :: Num a => a
aliceCommittedToHead = 20_000_000

bobCommittedToHead :: Num a => a
bobCommittedToHead = 5_000_000

paymentFromAliceToBob :: Num a => a
paymentFromAliceToBob = 1_000_000

someTxId :: IsString s => s
someTxId = "9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903"

inHeadAddress :: VerificationKey PaymentKey -> AddressInEra
inHeadAddress =
  mkVkAddress network
 where
  network = Testnet (NetworkMagic 14)

-- * Helpers

int :: Int -> Int
int = id

outputRef :: TxId -> Natural -> Value
outputRef tid tix =
  object
    [ "txId" .= tid
    , "index" .= tix
    ]
