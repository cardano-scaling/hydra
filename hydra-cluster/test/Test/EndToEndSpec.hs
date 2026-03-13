{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  waitForUTxO,
 )
import CardanoNode (
  EndToEndLog (..),
  withBackend,
  withCardanoNodeDevnet,
  withHydraScriptsAndBackendRunning,
 )
import Control.Lens ((^..), (^?))
import Data.Aeson (Result (..), Value (Null, Object, String), fromJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (AsJSON (_JSON), AsValue (_String), key, values, _JSON)
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Data.Time (secondsToDiffTime)
import Hydra.Cardano.Api hiding (Value, cardanoEra, queryGenesisParameters, txId)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Backend qualified as Backend
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
 )
import Hydra.Cluster.Scenarios (
  canCloseWithLongContestationPeriod,
  canDecommit,
  canDeposit,
  canDeposit2,
  canDepositPartially,
  canDepositReferenceScript,
  canDepositScriptBlueprint,
  canDepositTxBlueprint,
  canRecoverDeposit,
  canRecoverDepositInAnyState,
  canResumeOnMemberAlreadyBootstrapped,
  canSeePendingDeposits,
  canSideLoadSnapshot,
  canSubmitTransactionThroughAPI,
  ensureDepositScriptToTheRightHead,
  headIsFinalizedWith,
  headIsOpenWith,
  hydraNodeBaseUrl,
  initWithWrongKeys,
  nodeCanSupportMultipleEtcdClusters,
  nodeReObservesOnChainTxs,
  oneOfThreeNodesStopsForAWhile,
  persistenceCanLoadWithEmptyCommit,
  refuelIfNeeded,
  rejectDeposit,
  respendNTimes,
  restartedNodeCanAbort,
  restartedNodeCanObserveCommitTx,
  resumeFromLatestKnownPoint,
  singlePartyHeadFullLifeCycle,
  singlePartyUsesScriptOnL2,
  singlePartyUsesWithdrawZeroTrick,
  startWithWrongPeers,
  threeNodesNoErrorsOnOpen,
  threeNodesWithMirrorParty,
  waitsForChainInSyncAndSecure,
 )
import Hydra.Cluster.Util (Timing (..), chainConfigFor, depositTimeout, keysFor, mkTestTiming, modifyConfig)
import Hydra.Ledger.Cardano (mkRangedTx, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options
import Hydra.Tx.IsTx (txId)
import HydraNode (
  HydraClient (..),
  getMetrics,
  getSnapshotUTxO,
  input,
  output,
  prepareHydraNode,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
  withPreparedHydraNodeInSync,
 )
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Simple (getResponseBody, httpJSON)
import System.Directory (removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Test.Hydra.Cluster.Utils (chainPointToSlot)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor)
import Test.QuickCheck (Positive (..), generate)
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
  describe "Offline mode" $ do
    it "can process transactions in single participant offline head persistently" $ \tracer -> do
      withClusterTempDir $ \tmpDir -> do
        (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
        (bobCardanoVk, _) <- keysFor Bob
        initialUTxO <- generate $ do
          a <- genOneUTxOFor aliceCardanoVk
          b <- genOneUTxOFor bobCardanoVk
          pure $ a <> b
        Aeson.encodeFile (tmpDir </> "utxo.json") initialUTxO
        let offlineConfig =
              Offline
                OfflineChainConfig
                  { offlineHeadSeed = "test"
                  , initialUTxOFile = tmpDir </> "utxo.json"
                  , ledgerGenesisFile = Nothing
                  }
        let blockTime = 5
        -- Start a hydra-node in offline mode and submit a transaction from alice to bob
        aliceToBob <- withHydraNode (contramap FromHydraNode tracer) blockTime offlineConfig tmpDir 1 aliceSk [] [1] $ \node -> do
          let Just (aliceSeedTxIn, aliceSeedTxOut) = UTxO.find (isVkTxOut aliceCardanoVk) initialUTxO
          let Right aliceToBob =
                mkSimpleTx
                  (aliceSeedTxIn, aliceSeedTxOut)
                  (mkVkAddress testNetworkId bobCardanoVk, txOutValue aliceSeedTxOut)
                  aliceCardanoSk
          send node $ input "NewTx" ["transaction" .= aliceToBob]
          waitMatch 20 node $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          pure aliceToBob

        -- Restart a hydra-node in offline mode expect we can reverse the transaction (it retains state)
        withHydraNode (contramap FromHydraNode tracer) blockTime offlineConfig tmpDir 1 aliceSk [] [1] $ \node -> do
          let
            bobTxOut = toCtxUTxOTxOut $ List.head (txOuts' aliceToBob)
            Right bobToAlice =
              mkSimpleTx
                (mkTxIn aliceToBob 0, bobTxOut)
                (mkVkAddress testNetworkId bobCardanoVk, txOutValue bobTxOut)
                aliceCardanoSk
          send node $ input "NewTx" ["transaction" .= bobToAlice]
          waitMatch 10 node $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"

    it "rotates persistence on start up" $ \tracer -> do
      withClusterTempDir $ \tmpDir -> do
        (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
        initialUTxO <- generate $ genOneUTxOFor aliceCardanoVk
        Aeson.encodeFile (tmpDir </> "utxo.json") initialUTxO
        let offlineConfig =
              Offline
                OfflineChainConfig
                  { offlineHeadSeed = "test"
                  , initialUTxOFile = tmpDir </> "utxo.json"
                  , ledgerGenesisFile = Nothing
                  }
        let blockTime = 5
        -- Start a hydra-node in offline mode and submit several self-txs
        withHydraNode (contramap FromHydraNode tracer) blockTime offlineConfig tmpDir 1 aliceSk [] [] $ \node -> do
          -- Offline mode needs to confirm deposit of initialUTxO first.
          waitMatch 20 node $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          respendNTimes node aliceCardanoSk 0.01 200

        -- Measure restart time
        t0 <- getCurrentTime
        diff1 <- withHydraNode (contramap FromHydraNode tracer) blockTime offlineConfig tmpDir 1 aliceSk [] [] $ \_ -> do
          t1 <- getCurrentTime
          let diff = diffUTCTime t1 t0
          pure diff

        -- Measure restart after rotation
        options <- prepareHydraNode offlineConfig tmpDir 1 aliceSk [] [] id
        let options' = options{persistenceRotateAfter = Just (Positive 10)}
        t1 <- getCurrentTime
        diff2 <- withPreparedHydraNodeInSync (contramap FromHydraNode tracer) blockTime tmpDir 1 options' $ \_ -> do
          t2 <- getCurrentTime
          let diff = diffUTCTime t2 t1
          pure diff

        unless (diff2 < diff1 * 0.9) $
          failure $
            "Expected to start up 10% quicker than original " <> show diff1 <> ", but it took " <> show diff2

    it "supports multi-party networked heads" $ \tracer -> do
      withClusterTempDir $ \tmpDir -> do
        (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
        (bobCardanoVk, _) <- keysFor Bob
        initialUTxO <- generate $ do
          a <- genOneUTxOFor aliceCardanoVk
          b <- genOneUTxOFor bobCardanoVk
          pure $ a <> b
        Aeson.encodeFile (tmpDir </> "utxo.json") initialUTxO
        let offlineConfig =
              Offline
                OfflineChainConfig
                  { offlineHeadSeed = "test"
                  , initialUTxOFile = tmpDir </> "utxo.json"
                  , ledgerGenesisFile = Nothing
                  }
        let tr = contramap FromHydraNode tracer
        let blockTime = 5
        -- Start two hydra-nodes in offline mode and submit a transaction from alice to bob
        withHydraNode tr blockTime offlineConfig tmpDir 1 aliceSk [bobVk] [1, 2] $ \aliceNode -> do
          withHydraNode tr blockTime offlineConfig tmpDir 2 bobSk [aliceVk] [1, 2] $ \bobNode -> do
            waitForNodesConnected tr 20 $ aliceNode :| [bobNode]
            let Just (aliceSeedTxIn, aliceSeedTxOut) = UTxO.find (isVkTxOut aliceCardanoVk) initialUTxO
            let Right aliceToBob =
                  mkSimpleTx
                    (aliceSeedTxIn, aliceSeedTxOut)
                    (mkVkAddress testNetworkId bobCardanoVk, txOutValue aliceSeedTxOut)
                    aliceCardanoSk
            send aliceNode $ input "NewTx" ["transaction" .= aliceToBob]
            waitMatch 10 bobNode $ \v -> do
              guard $ v ^? key "tag" == Just "SnapshotConfirmed"

  describe "Cardano devnet" $ do
    describe "single party hydra head" $ do
      it "full head life-cycle" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            singlePartyHeadFullLifeCycle tracer tmpDir
      it "can close with long deadline" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canCloseWithLongContestationPeriod tracer tmpDir
      it "can deposit utxo" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDeposit tracer tmpDir
      it "can deposit with tx blueprint" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDepositTxBlueprint tracer tmpDir
      it "can decommit utxo" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDecommit tracer tmpDir
      it "reject commits with too low value" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            rejectDeposit tracer tmpDir
      it "can recover deposit" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canRecoverDeposit tracer tmpDir
      it "can recover deposit in any state" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canRecoverDepositInAnyState tracer tmpDir
      it "can see pending deposits" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canSeePendingDeposits tracer tmpDir
      it "deposit script with tx blueprint" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDepositScriptBlueprint tracer tmpDir
      it "deposit reference script" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDepositReferenceScript tracer tmpDir
      it "incrementally commit script with security checks" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            ensureDepositScriptToTheRightHead tracer tmpDir
      it "can deposit partial UTxO" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDepositPartially tracer tmpDir
      it "can submit a timed tx" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            timedTx tmpDir tracer
      it "can spend from a script on L2" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            singlePartyUsesScriptOnL2 tracer tmpDir
      it "can use withdraw zero on L2" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            singlePartyUsesWithdrawZeroTrick tracer tmpDir
      it "can submit a signed user transaction" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canSubmitTransactionThroughAPI tracer tmpDir
      it "persistence can load with empty commit" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            persistenceCanLoadWithEmptyCommit tracer tmpDir
      it "node re-observes on-chain txs" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            nodeReObservesOnChainTxs tracer tmpDir

    describe "two party hydra head" $ do
      it "can deposit and distribute funds" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canDeposit2 tracer tmpDir

    describe "three party hydra head" $ do
      it "can survive a bit of downtime of 1 in 3 nodes" $ \tracer ->
        withClusterTempDir $ \tmpDir ->
          withHydraScriptsAndBackendRunning tracer tmpDir $
            oneOfThreeNodesStopsForAWhile tracer tmpDir

      it "does not error when all nodes open the head concurrently" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir ->
            withHydraScriptsAndBackendRunning tracer tmpDir $
              threeNodesNoErrorsOnOpen tracer tmpDir

      it "node can support multiple etcd clusters" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir ->
            withHydraScriptsAndBackendRunning tracer tmpDir $
              nodeCanSupportMultipleEtcdClusters tracer tmpDir

      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir ->
            withHydraScriptsAndBackendRunning tracer tmpDir $
              initAndClose tmpDir tracer 1

      it "inits a Head and closes it immediately" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            let clusterIx = 0
            withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId -> do
              blockTime <- Backend.getBlockTime backend
              let nodeSocket' = case Backend.getOptions backend of
                    Direct DirectOptions{nodeSocket} -> nodeSocket
                    _ -> error "Unexpected Blockfrost backend"
              aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
              bobKeys@(bobCardanoVk, _) <- generate genKeyPair
              carolKeys@(carolCardanoVk, _) <- generate genKeyPair

              let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
                  hydraKeys = [aliceSk, bobSk, carolSk]

              let firstNodeId = clusterIx * 3

              let contestationPeriod = 2
              let hydraTracer = contramap FromHydraNode tracer

              let timing = Timing{blockTime, contestationPeriod, depositPeriod = truncate $ 3 * blockTime}
              withHydraCluster hydraTracer timing tmpDir nodeSocket' firstNodeId cardanoKeys hydraKeys hydraScriptsTxId $ \nodes -> do
                waitForNodesConnected hydraTracer 20 nodes
                let [n1, n2, n3] = toList nodes

                -- Funds to be used as fuel by Hydra protocol transactions
                seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
                seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
                seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

                -- Init head
                send n1 $ input "Init" []
                headId <-
                  waitForAllMatch 10 [n1, n2, n3] $ headIsOpenWith (Set.fromList [alice, bob, carol])

                -- Deposit UTxOs into the head
                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
                committedUTxOByAlice <- seedFromFaucet backend aliceExternalVk (lovelaceToValue aliceCommittedToHead) (contramap FromFaucet tracer)
                depositTxAlice <- requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk
                Backend.submitTransaction backend depositTxAlice

                (bobExternalVk, bobExternalSk) <- generate genKeyPair
                committedUTxOByBob <- seedFromFaucet backend bobExternalVk (lovelaceToValue bobCommittedToHead) (contramap FromFaucet tracer)
                depositTxBob <- requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk
                Backend.submitTransaction backend depositTxBob

                let u0 = committedUTxOByAlice <> committedUTxOByBob
                waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
                  output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxAlice]
                waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
                  output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxBob]

                send n1 $ input "Close" []
                deadline <- waitMatch 3 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  guard $ v ^? key "headId" == Just (toJSON headId)
                  v ^? key "contestationDeadline" . _JSON

                -- Expect to see ReadyToFanout within 3 seconds after deadline
                remainingTime <- diffUTCTime deadline <$> getCurrentTime
                waitFor hydraTracer (remainingTime + 3) [n1] $
                  output "ReadyToFanout" ["headId" .= headId]

                send n1 $ input "Fanout" []
                waitForAllMatch 10 [n1] $ headIsFinalizedWith headId u0

      it "Head can continue after TxInvalid" $ \tracer ->
        -- failAfter 60 $
        withClusterTempDir $ \tmpDir -> do
          let clusterIx = 0
          withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            let nodeSocket' = case Backend.getOptions backend of
                  Direct DirectOptions{nodeSocket} -> nodeSocket
                  _ -> error "Unexpected Blockfrost backend"
            aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
            bobKeys@(bobCardanoVk, _) <- generate genKeyPair
            carolKeys@(carolCardanoVk, _) <- generate genKeyPair

            let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
                hydraKeys = [aliceSk, bobSk, carolSk]

            let firstNodeId = clusterIx * 3

            let contestationPeriod = 2
            let hydraTracer = contramap FromHydraNode tracer

            let timing = Timing{blockTime, contestationPeriod, depositPeriod = truncate $ 3 * blockTime}
            withHydraCluster hydraTracer timing tmpDir nodeSocket' firstNodeId cardanoKeys hydraKeys hydraScriptsTxId $ \nodes -> do
              waitForNodesConnected hydraTracer 20 nodes
              let [n1, n2, n3] = toList nodes

              -- Funds to be used as fuel by Hydra protocol transactions
              seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
              seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
              seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

              send n1 $ input "Init" []
              headId <-
                waitForAllMatch 10 [n1, n2, n3] $ headIsOpenWith (Set.fromList [alice, bob, carol])

              -- Deposit UTxOs into the head
              (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
              committedUTxOByAlice <- seedFromFaucet backend aliceExternalVk (lovelaceToValue aliceCommittedToHead) (contramap FromFaucet tracer)
              depositTxAlice <- requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk
              Backend.submitTransaction backend depositTxAlice

              (bobExternalVk, bobExternalSk) <- generate genKeyPair
              committedUTxOByBob <- seedFromFaucet backend bobExternalVk (lovelaceToValue bobCommittedToHead) (contramap FromFaucet tracer)
              depositTxBob <- requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk
              Backend.submitTransaction backend depositTxBob

              waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
                output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxAlice]
              waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
                output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxBob]

              let firstCommittedUTxO = Prelude.head $ UTxO.toList committedUTxOByBob
              let Right tx =
                    mkSimpleTx
                      firstCommittedUTxO
                      (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
                      bobExternalSk

              let unsign (Tx body _) = Tx body []

              send n1 $ input "NewTx" ["transaction" .= unsign tx]

              validationError <- waitForAllMatch 10 [n1, n2, n3] $ \v -> do
                guard $ v ^? key "tag" == Just "TxInvalid"
                v ^? key "validationError" . key "reason" . _JSON

              validationError `shouldContain` "MissingVKeyWitnessesUTXOW"

              send n3 $ input "NewTx" ["transaction" .= tx]

              waitFor hydraTracer 20 [n1, n2, n3] $
                output "TxValid" ["transactionId" .= txId tx, "headId" .= headId]

              waitForAllMatch 20 [n1, n2, n3] $ \v -> do
                guard $ v ^? key "tag" == Just "SnapshotConfirmed"

              headUTxO :: UTxO <-
                parseUrlThrow ("GET " <> hydraNodeBaseUrl n1 <> "/snapshot/utxo")
                  >>= httpJSON
                  <&> getResponseBody

              send n1 $ input "Close" []

              deadline <- waitMatch 3 n1 $ \v -> do
                guard $ v ^? key "tag" == Just "HeadIsClosed"
                guard $ v ^? key "headId" == Just (toJSON headId)
                snapshotNumber <- v ^? key "snapshotNumber"
                guard $ snapshotNumber == Aeson.Number 1
                v ^? key "contestationDeadline" . _JSON

              -- Expect to see ReadyToFanout within 3 seconds after deadline
              remainingTime <- diffUTCTime deadline <$> getCurrentTime
              waitFor hydraTracer (remainingTime + 3) [n1] $
                output "ReadyToFanout" ["headId" .= headId]

              send n1 $ input "Fanout" []
              waitForAllMatch 10 [n1] $ headIsFinalizedWith headId headUTxO

      it "supports mirror party" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withHydraScriptsAndBackendRunning tracer tmpDir $
              threeNodesWithMirrorParty tracer tmpDir

      describe "Fanout maximum UTxOs" $ do
        -- This constant is set to the maximum number of UTxOs that can be
        -- fanned out in a single transaction. It is derived from the maximum
        -- transaction execution budget.
        --
        -- See <https://github.com/cardano-scaling/hydra/issues/1468> for work
        -- on addressing this.

        let ledgerSizeLimit = 47

        it "reaches the fan out limit" $ \tracer ->
          failAfter 60 $
            withClusterTempDir $ \tmpDir -> do
              withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId ->
                reachFanoutLimit ledgerSizeLimit tmpDir tracer hydraScriptsTxId backend

        it "doesn't reach the fan out limit by one" $ \tracer ->
          failAfter 60 $
            withClusterTempDir $ \tmpDir -> do
              withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId ->
                reachFanoutLimit (ledgerSizeLimit - 1) tmpDir tracer hydraScriptsTxId backend
                  `shouldThrow` \(e :: SomeException) -> "HeadIsFinalized" `isInfixOf` show e

    describe "restarting nodes" $ do
      it "resume from latest observed point" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withHydraScriptsAndBackendRunning tracer tmpDir $
            resumeFromLatestKnownPoint tracer tmpDir

      it "can abort head after restart" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withHydraScriptsAndBackendRunning tracer tmpDir $
            restartedNodeCanAbort tracer tmpDir

      it "can observe a commit tx after a restart, even when a tx happened while down" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withHydraScriptsAndBackendRunning tracer tmpDir $
            restartedNodeCanObserveCommitTx tracer tmpDir

      it "can start chain from the past and replay on-chain events" $ \tracer ->
        withClusterTempDir $ \tmp ->
          withHydraScriptsAndBackendRunning tracer tmp $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            let timing = mkTestTiming blockTime
            aliceChainConfig <- chainConfigFor Alice tmp backend hydraScriptsTxId [] timing
            let nodeId = 1
            let hydraTracer = contramap FromHydraNode tracer
            (tip, aliceHeadId) <- withHydraNode hydraTracer blockTime aliceChainConfig tmp nodeId aliceSk [] [1] $ \n1 -> do
              seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
              tip <- Backend.queryTip backend
              send n1 $ input "Init" []
              headId <- waitForAllMatch 10 [n1] $ headIsOpenWith (Set.fromList [alice])
              return (tip, headId)

            -- REVIEW: Do we want to keep this --start-chain-from feature or
            -- replace it with an event source load from persistence?

            -- NOTE: Need to clear persistence as we would load the state and
            -- not resynchronize from chain
            removeDirectoryRecursive $ tmp </> "state-" <> show nodeId

            let aliceChainConfig' = aliceChainConfig & modifyConfig (\cfg -> cfg{startChainFrom = Just tip})
            withHydraNode hydraTracer blockTime aliceChainConfig' tmp 1 aliceSk [] [1] $ \n1 -> do
              headId' <- waitForAllMatch 10 [n1] $ headIsOpenWith (Set.fromList [alice])
              headId' `shouldBe` aliceHeadId

      it "close of an initial snapshot from re-initialized node is contested" $ \tracer ->
        withClusterTempDir $ \tmp ->
          withHydraScriptsAndBackendRunning tracer tmp $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            (bobCardanoVk, _bobCardanoSk) <- keysFor Bob

            seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
            seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

            tip <- Backend.queryTip backend
            let startFromTip = modifyConfig $ \x -> x{startChainFrom = Just tip}
            let timing = mkTestTiming blockTime
            aliceChainConfig <- chainConfigFor Alice tmp backend hydraScriptsTxId [Bob] timing <&> startFromTip
            bobChainConfig <- chainConfigFor Bob tmp backend hydraScriptsTxId [Alice] timing <&> startFromTip

            let hydraTracer = contramap FromHydraNode tracer
            let aliceNodeId = 1
                bobNodeId = 2
                allNodesIds = [aliceNodeId, bobNodeId]
                withAliceNode :: (HydraClient -> IO a) -> IO a
                withAliceNode = withHydraNode hydraTracer blockTime aliceChainConfig tmp aliceNodeId aliceSk [bobVk] allNodesIds
                withBobNode :: (HydraClient -> IO a) -> IO a
                withBobNode = withHydraNode hydraTracer blockTime bobChainConfig tmp bobNodeId bobSk [aliceVk] allNodesIds

            withAliceNode $ \n1 -> do
              headId <- withBobNode $ \n2 -> do
                waitForNodesConnected hydraTracer 20 $ n1 :| [n2]
                send n1 $ input "Init" []
                headId <- waitForAllMatch 10 [n1, n2] $ headIsOpenWith (Set.fromList [alice, bob])

                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
                committedUTxOByAlice <- seedFromFaucet backend aliceExternalVk (lovelaceToValue aliceCommittedToHead) (contramap FromFaucet tracer)
                depositTxAlice <- requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk
                Backend.submitTransaction backend depositTxAlice
                waitFor hydraTracer (depositTimeout timing) [n1, n2] $
                  output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxAlice]

                (bobExternalVk, _bobExternalSk) <- generate genKeyPair

                -- Create an arbitrary transaction using some input.
                let firstCommittedUTxO = Prelude.head $ UTxO.toList committedUTxOByAlice
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

              -- NOTE: Clear persisted protocol state on disk to have bob
              -- re-discover the head and close with initial snapshot. We are
              -- not clearing the whole persistence dir as we would not be able
              -- to re-connect to the L2 network.
              removeFile $ tmp </> "state-" <> show bobNodeId </> "state"

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

      it "can side load snapshot" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withHydraScriptsAndBackendRunning tracer tmpDir $
            canSideLoadSnapshot tracer tmpDir

      it "can resume when member has already been bootstrapped" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \_ backend ->
            publishHydraScriptsAs backend Faucet
              >>= canResumeOnMemberAlreadyBootstrapped tracer tmpDir backend

      it "prevents network interactions until chain backend is in sync and secure." $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \_ backend ->
            publishHydraScriptsAs backend Faucet
              >>= waitsForChainInSyncAndSecure tracer tmpDir backend

    describe "two hydra heads scenario" $ do
      it "two heads on the same network do not conflict" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId ->
              concurrentlyLabelled_
                ("init-and-close-0", initAndClose tmpDir tracer 0 backend hydraScriptsTxId)
                ("init-and-close-1", initAndClose tmpDir tracer 1 backend hydraScriptsTxId)

      it "alice inits a Head with incorrect keys preventing bob from observing InitTx" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withHydraScriptsAndBackendRunning tracer tmpDir $
              initWithWrongKeys tmpDir tracer

      it "cluster id mismatch provides useful info in the logs" $ \tracer ->
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withHydraScriptsAndBackendRunning tracer tmpDir $
              startWithWrongPeers tmpDir tracer

      it "bob cannot close alice's head" $ \tracer -> do
        failAfter 60 $
          withClusterTempDir $ \tmpDir -> do
            withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId -> do
              blockTime <- Backend.getBlockTime backend
              (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
              (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
              let timing = mkTestTiming blockTime
              aliceChainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [] timing
              bobChainConfig <- chainConfigFor Bob tmpDir backend hydraScriptsTxId [Alice] timing
              let hydraTracer = contramap FromHydraNode tracer
              withHydraNode hydraTracer blockTime aliceChainConfig tmpDir 1 aliceSk [] allNodeIds $ \n1 ->
                withHydraNode hydraTracer blockTime bobChainConfig tmpDir 2 bobSk [aliceVk] allNodeIds $ \n2 -> do
                  -- Funds to be used as fuel by Hydra protocol transactions
                  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
                  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

                  send n1 $ input "Init" []
                  headIdAliceOnly <- waitMatch 10 n1 $ headIsOpenWith (Set.fromList [alice])

                  -- Bob opens and immediately closes a Head with Alice, iow pulls Alice in
                  -- "his" Head
                  send n2 $ input "Init" []
                  headIdAliceAndBob <- waitMatch 10 n2 $ headIsOpenWith (Set.fromList [alice, bob])

                  send n2 $ input "Close" []
                  void $ waitMatch 10 n2 $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsClosed"
                    guard $ v ^? key "headId" == Just (toJSON headIdAliceAndBob)

                  -- Alice should still have her head open (verify by closing)
                  send n1 $ input "Close" []
                  void $ waitMatch 10 n1 $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsClosed"
                    guard $ v ^? key "headId" == Just (toJSON headIdAliceOnly)

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withClusterTempDir $ \tmpDir -> do
          (aliceCardanoVk, _) <- keysFor Alice
          withHydraScriptsAndBackendRunning tracer tmpDir $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            let hydraTracer = contramap FromHydraNode tracer
            let timing = mkTestTiming blockTime
            aliceChainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [Bob, Carol] timing
            bobChainConfig <- chainConfigFor Bob tmpDir backend hydraScriptsTxId [Alice, Carol] timing
            carolChainConfig <- chainConfigFor Carol tmpDir backend hydraScriptsTxId [Alice, Bob] timing
            failAfter 20 $
              withHydraNode hydraTracer blockTime aliceChainConfig tmpDir 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode hydraTracer blockTime bobChainConfig tmpDir 2 bobSk [aliceVk, carolVk] allNodeIds $ \n2 ->
                  withHydraNode hydraTracer blockTime carolChainConfig tmpDir 3 carolSk [aliceVk, bobVk] allNodeIds $ \n3 -> do
                    -- Funds to be used as fuel by Hydra protocol transactions
                    seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
                    waitForNodesConnected hydraTracer 20 $ n1 :| [n2, n3]
                    send n1 $ input "Init" []
                    void $ waitForAllMatch 3 [n1] $ headIsOpenWith (Set.fromList [alice, bob, carol])
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_inputs" `BS.isInfixOf`)

    -- TODO: move to a HydraNodeSpec
    describe "withHydraNode" $ do
      it "detects crashes" $ \tracer -> do
        withClusterTempDir $ \dir -> do
          withBackend tracer dir $ \blockTime backend -> do
            let nodeSocket' = case Backend.getOptions backend of
                  Direct DirectOptions{nodeSocket} -> nodeSocket
                  _ -> error "Unexpected Blockfrost backend"
            -- NOTE: Deliberately broken configuration so we expect the node to not start.
            let chainConfig =
                  Cardano
                    defaultCardanoChainConfig
                      { cardanoSigningKey = "not-existing.sk"
                      , chainBackendOptions =
                          Direct
                            DirectOptions
                              { networkId = Hydra.Options.networkId defaultDirectOptions
                              , nodeSocket = nodeSocket'
                              }
                      }
            withHydraNode (contramap FromHydraNode tracer) blockTime chainConfig dir 1 aliceSk [] [1] (const $ pure ())
              `shouldThrow` \(e :: SomeException) ->
                "hydra-node" `isInfixOf` show e
                  && "not-existing.sk" `isInfixOf` show e

      it "stops gracefully" $ \tracer -> do
        withClusterTempDir $ \dir -> do
          withHydraScriptsAndBackendRunning tracer dir $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            let hydraTracer = contramap FromHydraNode tracer
            let timing = mkTestTiming blockTime
            aliceChainConfig <- chainConfigFor Alice dir backend hydraScriptsTxId [] timing

            -- XXX: Need to do something in 'action' otherwise always green?
            withHydraNode hydraTracer blockTime aliceChainConfig dir 1 aliceSk [] [1] $ \_ -> do
              threadDelay 0.1

      it "can be restarted" $ \tracer -> do
        withClusterTempDir $ \dir -> do
          withHydraScriptsAndBackendRunning tracer dir $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            let hydraTracer = contramap FromHydraNode tracer
            let timing = mkTestTiming blockTime
            aliceChainConfig <- chainConfigFor Alice dir backend hydraScriptsTxId [] timing

            -- XXX: Need to do something in 'action' otherwise always green?
            failAfter 10 $
              withHydraNode hydraTracer blockTime aliceChainConfig dir 1 aliceSk [] [1] $ \_ -> do
                threadDelay 0.1
            failAfter 10 $
              withHydraNode hydraTracer blockTime aliceChainConfig dir 1 aliceSk [] [1] $ \_ -> do
                threadDelay 0.1

      it "logs to a logfile" $ \tracer -> do
        withClusterTempDir $ \dir -> do
          withHydraScriptsAndBackendRunning tracer dir $ \backend hydraScriptsTxId -> do
            blockTime <- Backend.getBlockTime backend
            let hydraTracer = contramap FromHydraNode tracer
            refuelIfNeeded tracer backend Alice 100_000_000
            let timing = mkTestTiming blockTime
            aliceChainConfig <- chainConfigFor Alice dir backend hydraScriptsTxId [] timing
            withHydraNode hydraTracer blockTime aliceChainConfig dir 1 aliceSk [] [1] $ \n1 -> do
              send n1 $ input "Init" []

            let logFilePath = dir </> "logs" </> "hydra-node-1.log"
            logfile <- readFileBS logFilePath
            BS.length logfile `shouldSatisfy` (> 0)
            logfile `shouldSatisfy` BS.isInfixOf "NodeOptions"

timedTx :: ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> backend -> [TxId] -> IO ()
timedTx tmpDir tracer backend hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [] timing
  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer blockTime aliceChainConfig tmpDir 1 aliceSk [] [1] $ \n1 -> do
    let lovelaceBalanceValue = 100_000_000

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ backend aliceCardanoVk lovelaceBalanceValue (contramap FromFaucet tracer)
    send n1 $ input "Init" []
    headId <-
      waitForAllMatch 10 [n1] $
        headIsOpenWith (Set.fromList [alice])

    -- Deposit some UTxO into the head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    utxoToDeposit <- seedFromFaucet backend aliceExternalVk (lovelaceToValue aliceCommittedToHead) (contramap FromFaucet tracer)
    txDeposit <- requestCommitTx n1 utxoToDeposit <&> signTx aliceExternalSk
    Backend.submitTransaction backend txDeposit
    waitFor hydraTracer 5 [n1] $ output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId txDeposit]

    -- Acquire a current point in time
    slotLengthSec <- protocolParamSlotLength <$> Backend.queryGenesisParameters backend
    currentSlot <- chainPointToSlot <$> Backend.queryTip backend

    -- Create a transaction which is only valid in 5 seconds
    let secondsToAwait = 5
        slotsToAwait = SlotNo . truncate $ fromInteger secondsToAwait / slotLengthSec
        futureSlot = currentSlot + slotsToAwait
        lovelaceToSend = lovelaceBalanceValue - 90_000_000

        -- TODO (later) use time in a script (as it is using POSIXTime)
        Right tx =
          mkRangedTx
            (Prelude.head $ UTxO.toList utxoToDeposit)
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
      output "TxValid" ["transactionId" .= txId tx, "headId" .= headId]

    confirmedTransactions <- waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      v ^? key "snapshot" . key "confirmed"
    confirmedTransactions ^.. values `shouldBe` [toJSON tx]

initAndClose :: ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> Int -> backend -> [TxId] -> IO ()
initAndClose tmpDir tracer clusterIx backend hydraScriptsTxId = do
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
  bobKeys@(bobCardanoVk, _) <- generate genKeyPair
  carolKeys@(carolCardanoVk, _) <- generate genKeyPair

  let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
      hydraKeys = [aliceSk, bobSk, carolSk]

  let firstNodeId = clusterIx * 3
  let contestationPeriod = 2
  let hydraTracer = contramap FromHydraNode tracer
  let nodeSocket' = case Backend.getOptions backend of
        Direct DirectOptions{nodeSocket} -> nodeSocket
        _ -> error "Unexpected Blockfrost backend"
  blockTime <- Backend.getBlockTime backend
  let timing = Timing{blockTime, contestationPeriod, depositPeriod = truncate $ 3 * blockTime}
  withHydraCluster hydraTracer timing tmpDir nodeSocket' firstNodeId cardanoKeys hydraKeys hydraScriptsTxId $ \nodes -> do
    let [n1, n2, n3] = toList nodes
    waitForNodesConnected hydraTracer 20 $ n1 :| [n2, n3]

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send n1 $ input "Init" []
    headId <-
      waitForAllMatch 10 [n1, n2, n3] $
        headIsOpenWith (Set.fromList [alice, bob, carol])

    -- Deposit UTxOs into the head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    committedUTxOByAlice <- seedFromFaucet backend aliceExternalVk (lovelaceToValue aliceCommittedToHead) (contramap FromFaucet tracer)
    depositTxAlice <- requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk
    Backend.submitTransaction backend depositTxAlice

    (bobExternalVk, bobExternalSk) <- generate genKeyPair
    committedUTxOByBob <- seedFromFaucet backend bobExternalVk (lovelaceToValue bobCommittedToHead) (contramap FromFaucet tracer)
    depositTxBob <- requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk
    Backend.submitTransaction backend depositTxBob

    waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
      output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxAlice]
    waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
      output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxBob]

    -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
    let firstCommittedUTxO = Prelude.head $ UTxO.toList committedUTxOByAlice
    let Right tx =
          mkSimpleTx
            firstCommittedUTxO
            (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
            aliceExternalSk
    send n1 $ input "NewTx" ["transaction" .= tx]
    waitFor hydraTracer 10 [n1, n2, n3] $
      output "TxValid" ["transactionId" .= txId tx, "headId" .= headId]

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
                  , "inlineDatumRaw" .= Null
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
                  , "inlineDatumRaw" .= Null
                  , "referenceScript" .= Null
                  ]
              )
            ]
            <> fmap toJSON (Map.fromList (UTxO.toList committedUTxOByBob))

    -- After 2 deposits + 1 tx, snapshot number should be 3
    let expectedTxSnapshotNumber :: Int = 3

    waitMatch 10 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshotNumber <- v ^? key "snapshot" . key "number"
      guard $ snapshotNumber == toJSON expectedTxSnapshotNumber
      utxo <- v ^? key "snapshot" . key "utxo"
      guard $ utxo == toJSON newUTxO
      confirmedTransactions <- v ^? key "snapshot" . key "confirmed"
      guard $ confirmedTransactions == toJSON [tx]

    (toJSON <$> getSnapshotUTxO n1) `shouldReturn` toJSON newUTxO

    send n1 $ input "Close" []
    deadline <- waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsClosed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshotNumber <- v ^? key "snapshotNumber"
      guard $ snapshotNumber == toJSON expectedTxSnapshotNumber
      v ^? key "contestationDeadline" . _JSON

    -- Expect to see ReadyToFanout within 3 seconds after deadline
    remainingTime <- diffUTCTime deadline <$> getCurrentTime
    waitFor hydraTracer (remainingTime + 3) [n1] $
      output "ReadyToFanout" ["headId" .= headId]

    send n1 $ input "Fanout" []

    case fromJSON $ toJSON newUTxO of
      Error err ->
        failure $ "newUTxO isn't valid JSON?: " <> err
      Data.Aeson.Success u -> do
        waitForAllMatch 3 [n1] $ headIsFinalizedWith headId u
        failAfter 5 $ waitForUTxO backend u

reachFanoutLimit :: Integer -> ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> [TxId] -> backend -> IO ()
reachFanoutLimit ledgerSize tmpDir tracer hydraScriptsTxId backend = do
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair

  let contestationPeriod = 2
  let hydraTracer = contramap FromHydraNode tracer
  let nodeSocket' = case Backend.getOptions backend of
        Direct DirectOptions{nodeSocket} -> nodeSocket
        _ -> error "Unexpected Blockfrost backend"
  blockTime <- Backend.getBlockTime backend
  let timing = Timing{blockTime, contestationPeriod, depositPeriod = truncate $ 3 * blockTime}
  withHydraCluster hydraTracer timing tmpDir nodeSocket' 1 [aliceKeys] [aliceSk] hydraScriptsTxId $ \nodes -> do
    let [node] = toList nodes
    waitForNodesConnected hydraTracer 20 $ node :| []

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send node $ input "Init" []
    headId <-
      waitForAllMatch 10 [node] $
        headIsOpenWith (Set.fromList [alice])

    -- Deposit UTxOs into the head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    committedUTxOByAlice <- seedFromFaucet backend aliceExternalVk (lovelaceToValue (fromInteger $ ledgerSize * 1_000_000)) (contramap FromFaucet tracer)
    depositTxAlice <- requestCommitTx node committedUTxOByAlice <&> signTx aliceExternalSk
    Backend.submitTransaction backend depositTxAlice
    waitFor hydraTracer (depositTimeout timing) [node] $
      output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTxAlice]

    -- Create many transactions to reach the ledger limit
    let loop 0 _ res = return res
        loop n utxo _ = do
          let Right tx =
                mkSimpleTx
                  utxo
                  (inHeadAddress aliceExternalVk, lovelaceToValue 1_000_000)
                  aliceExternalSk
              Just out' = viaNonEmpty last (txOuts' tx)
              txId' = TxIn (txId tx) (toEnum 1)
              utxo' = (txId', toCtxUTxOTxOut out')

          send node $ input "NewTx" ["transaction" .= tx]
          waitFor hydraTracer 10 [node] $
            output "TxValid" ["transactionId" .= txId tx, "headId" .= headId]
          loop (n - 1 :: Integer) utxo' (Just tx)

    Just lastTx <- loop (ledgerSize - 1) (Prelude.head $ UTxO.toList committedUTxOByAlice) Nothing
    waitMatch 10 node $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshot <- v ^? key "snapshot"
      let
        txIds = snapshot ^.. key "confirmed" . values . key "txId" . _JSON
      guard $ txId lastTx `elem` (txIds :: [TxId])

    send node $ input "Close" []
    waitFor hydraTracer 10 [node] $
      output "ReadyToFanout" ["headId" .= headId]

    send node $ input "Fanout" []

    waitMatch 5 node $ \v -> do
      guard $ v ^? key "tag" == Just "PostTxOnChainFailed"
      failureReason <- v ^? key "postTxError" . key "failureReason" . _String
      -- Note: Now the failure is because our fee estimate is (correctly)
      -- too large.
      guard $ "ExUnitsTooBigUTxO" `isInfixOf` failureReason

-- * Fixtures

aliceCommittedToHead :: Num a => a
aliceCommittedToHead = 20_000_000

bobCommittedToHead :: Num a => a
bobCommittedToHead = 5_000_000

paymentFromAliceToBob :: Num a => a
paymentFromAliceToBob = 1_000_000

inHeadAddress :: VerificationKey PaymentKey -> AddressInEra
inHeadAddress =
  mkVkAddress network
 where
  network = Testnet (NetworkMagic 14)

-- * Helpers

int :: Int -> Int
int = id
