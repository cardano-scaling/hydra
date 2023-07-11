{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (QueryPoint (..), queryGenesisParameters, queryTip, queryTipSlotNo, submitTx, waitForUTxO)
import CardanoNode (RunningNode (..), generateCardanoKey, withCardanoNodeDevnet)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Lens ((^..), (^?))
import Data.Aeson (Result (..), Value (Null, Object, String), fromJSON, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, values, _JSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Time (secondsToDiffTime)
import Hydra.Cardano.Api (
  AddressInEra,
  GenesisParameters (..),
  Key (SigningKey),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  SlotNo (..),
  TxId,
  TxIn (..),
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  serialiseAddress,
  signTx,
  writeFileTextEnvelope,
  pattern TxValidityLowerBound,
 )
import qualified Hydra.Chain.Direct as ChainState
import Hydra.Chain.Direct.State ()
import Hydra.Cluster.Faucet (
  Marked (Fuel, Normal),
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
  headIsInitializingWith,
  restartedNodeCanAbort,
  restartedNodeCanObserveCommitTx,
  singlePartyCannotCommitExternallyWalletUtxo,
  singlePartyCommitsFromExternalScript,
  singlePartyCommitsUsingFuel,
  singlePartyHeadFullLifeCycle,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.HeadLogic (HeadStateEvent (..))
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, mkRangedTx, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options
import Hydra.Party (deriveParty)
import HydraNode (
  EndToEndLog (..),
  HydraClient,
  externalCommit,
  getMetrics,
  input,
  output,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
 )
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.IO.Error (isEOFError)
import System.Process (CreateProcess (..), StdStream (..), proc, withCreateProcess)
import System.Timeout (timeout)
import Test.QuickCheck (generate, suchThat)
import qualified Prelude

allNodeIds :: [Int]
allNodeIds = [1 .. 3]

-- | Like 'withTempDir', busing a common prefix to keep hydra-cluster logs more
-- easily on CI.
--
-- NOTE: The ci-nix.yaml workflow depends on this.
withClusterTempDir :: MonadIO m => String -> (FilePath -> m a) -> m a
withClusterTempDir name =
  withTempDir ("hydra-cluster-e2e-" <> name)

spec :: Spec
spec = around showLogsOnFailure $
  describe "End-to-end on Cardano devnet" $ do
    describe "single party hydra head" $ do
      it "full head life-cycle" $ \tracer -> do
        withClusterTempDir "single-full-life-cycle" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyHeadFullLifeCycle tracer tmpDir node
      it "can close with long deadline" $ \tracer -> do
        withClusterTempDir "close-long-deadline" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= canCloseWithLongContestationPeriod tracer tmpDir node
      it "can submit a timed tx" $ \tracer -> do
        withClusterTempDir "timed-tx" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= timedTx tmpDir tracer node
      it "commits using fuel" $ \tracer -> do
        withClusterTempDir "commits-using-fuel" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyCommitsUsingFuel tracer tmpDir node
      it "commits from external with script utxo" $ \tracer -> do
        withClusterTempDir "single-commits-script-from-external" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyCommitsFromExternalScript tracer tmpDir node
      it "can't commit externally with internal wallet utxo" $ \tracer -> do
        withClusterTempDir "commit-internal-wallet-utxo" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyCannotCommitExternallyWalletUtxo tracer tmpDir node

    describe "three hydra nodes scenario" $ do
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withClusterTempDir "three-full-life-cycle" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              initAndClose tmpDir tracer 1 hydraScriptsTxId node

      it "inits a Head and closes it immediately" $ \tracer ->
        failAfter 60 $
          withClusterTempDir "three-init-close-immediately" $ \tmpDir -> do
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
              withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
                let [n1, n2, n3] = toList nodes
                waitForNodesConnected tracer [n1, n2, n3]

                -- Funds to be used as fuel by Hydra protocol transactions
                seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
                seedFromFaucet_ node bobCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
                seedFromFaucet_ node carolCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

                send n1 $ input "Init" []
                headId <-
                  waitForAllMatch 10 [n1, n2, n3] $
                    headIsInitializingWith (Set.fromList [alice, bob, carol])

                -- Get some UTXOs to commit to a head
                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
                committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
                externalCommit n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

                (bobExternalVk, bobExternalSk) <- generate genKeyPair
                committedUTxOByBob <- seedFromFaucet node bobExternalVk bobCommittedToHead Normal (contramap FromFaucet tracer)
                externalCommit n2 committedUTxOByBob <&> signTx bobExternalSk >>= submitTx node

                externalCommit n3 mempty >>= submitTx node

                let u0 = committedUTxOByAlice <> committedUTxOByBob

                waitFor tracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= u0, "headId" .= headId]

                send n1 $ input "Close" []
                deadline <- waitMatch 3 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  guard $ v ^? key "headId" == Just (toJSON headId)
                  snapshotNumber <- v ^? key "snapshotNumber"
                  guard $ snapshotNumber == Aeson.Number 0
                  v ^? key "contestationDeadline" . _JSON

                -- Expect to see ReadyToFanout within 3 seconds after deadline
                remainingTime <- diffUTCTime deadline <$> getCurrentTime
                waitFor tracer (remainingTime + 3) [n1] $
                  output "ReadyToFanout" ["headId" .= headId]

                send n1 $ input "Fanout" []
                waitFor tracer 3 [n1] $
                  output "HeadIsFinalized" ["utxo" .= u0, "headId" .= headId]

    describe "restarting nodes" $ do
      it "can abort head after restart" $ \tracer -> do
        withClusterTempDir "abort-after-restart" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= restartedNodeCanAbort tracer tmpDir node

      it "can observe a commit tx after a restart, even when a tx happened while down" $ \tracer -> do
        withClusterTempDir "commit-after-restart" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= restartedNodeCanObserveCommitTx tracer tmpDir node

      it "can start chain from the past and replay on-chain events" $ \tracer ->
        withClusterTempDir "replay-chain-events" $ \tmp ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            let contestationPeriod = UnsafeContestationPeriod 10
            aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] contestationPeriod
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            let nodeId = 1
            (tip, aliceHeadId) <- withHydraNode tracer aliceChainConfig tmp nodeId aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
              seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
              tip <- queryTip networkId nodeSocket
              send n1 $ input "Init" []
              headId <- waitForAllMatch 10 [n1] $ headIsInitializingWith (Set.fromList [alice])
              return (tip, headId)

            -- REVIEW: Do we want to keep this --start-chain-from feature or
            -- replace it with an event source load from persistence?

            -- NOTE: Need to clear persistence as we would load the state and
            -- not resynchronize from chain
            removeDirectoryRecursive $ tmp </> "state-" <> show nodeId

            let aliceChainConfig' =
                  aliceChainConfig
                    { startChainFrom = Just tip
                    }
            withHydraNode tracer aliceChainConfig' tmp 1 aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
              headId' <- waitForAllMatch 10 [n1] $ headIsInitializingWith (Set.fromList [alice])
              headId' `shouldBe` aliceHeadId

      it "close of an initial snapshot from re-initialized node is contested" $ \tracer ->
        withClusterTempDir "contest-after-restart" $ \tmp ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet

            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            (bobCardanoVk, _bobCardanoSk) <- keysFor Bob

            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
            seedFromFaucet_ node bobCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

            tip <- queryTip networkId nodeSocket
            let startFromTip x = x{startChainFrom = Just tip}
            let contestationPeriod = UnsafeContestationPeriod 10
            aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Bob] contestationPeriod <&> startFromTip
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice] contestationPeriod <&> startFromTip

            let aliceNodeId = 1
                bobNodeId = 2
                allNodesIds = [aliceNodeId, bobNodeId]
                withAliceNode :: (HydraClient -> IO a) -> IO a
                withAliceNode = withHydraNode tracer aliceChainConfig tmp aliceNodeId aliceSk [bobVk] allNodesIds hydraScriptsTxId
                withBobNode :: (HydraClient -> IO a) -> IO a
                withBobNode = withHydraNode tracer bobChainConfig tmp bobNodeId bobSk [aliceVk] allNodesIds hydraScriptsTxId

            withAliceNode $ \n1 -> do
              headId <- withBobNode $ \n2 -> do
                waitForNodesConnected tracer [n1, n2]

                send n1 $ input "Init" []
                headId <- waitForAllMatch 10 [n1, n2] $ headIsInitializingWith (Set.fromList [alice, bob])

                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
                committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
                externalCommit n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

                (bobExternalVk, _bobExternalSk) <- generate genKeyPair
                externalCommit n2 mempty >>= submitTx node

                waitFor tracer 10 [n1, n2] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice, "headId" .= headId]

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

                waitFor tracer 10 [n1, n2] $ output "HeadIsContested" ["snapshotNumber" .= (1 :: Word), "headId" .= headId]

    describe "two hydra heads scenario" $ do
      it "two heads on the same network do not conflict" $ \tracer ->
        failAfter 60 $
          withClusterTempDir "two-heads-no-conflict" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              concurrently_
                (initAndClose tmpDir tracer 0 hydraScriptsTxId node)
                (initAndClose tmpDir tracer 1 hydraScriptsTxId node)

      it "bob cannot abort alice's head" $ \tracer -> do
        failAfter 60 $
          withClusterTempDir "two-heads-cant-abort" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
              (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
              (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
              let contestationPeriod = UnsafeContestationPeriod 10
              aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket [] contestationPeriod
              bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket [Alice] contestationPeriod
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              withHydraNode tracer aliceChainConfig tmpDir 1 aliceSk [] allNodeIds hydraScriptsTxId $ \n1 ->
                withHydraNode tracer bobChainConfig tmpDir 2 bobSk [aliceVk] allNodeIds hydraScriptsTxId $ \n2 -> do
                  -- Funds to be used as fuel by Hydra protocol transactions
                  seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
                  seedFromFaucet_ node bobCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

                  send n1 $ input "Init" []
                  headIdAliceOnly <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

                  -- Bob opens and immediately aborts a Head with Alice, iow pulls Alice in
                  -- "his" Head
                  send n2 $ input "Init" []
                  headIdAliceAndBob <- waitMatch 10 n2 $ headIsInitializingWith (Set.fromList [alice, bob])

                  send n2 $ input "Abort" []
                  waitFor tracer 10 [n2] $
                    output "HeadIsAborted" ["utxo" .= Object mempty, "headId" .= headIdAliceAndBob]

                  -- Alice should be able to continue working with her Head
                  externalCommit n1 mempty >>= submitTx node
                  waitFor tracer 10 [n1] $
                    output "HeadIsOpen" ["utxo" .= Object mempty, "headId" .= headIdAliceOnly]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withClusterTempDir "prometheus-metrics" $ \tmpDir -> do
          (aliceCardanoVk, _) <- keysFor Alice
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            let contestationPeriod = UnsafeContestationPeriod 10
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket [Bob, Carol] contestationPeriod
            bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket [Alice, Carol] contestationPeriod
            carolChainConfig <- chainConfigFor Carol tmpDir nodeSocket [Alice, Bob] contestationPeriod
            failAfter 20 $
              withHydraNode tracer aliceChainConfig tmpDir 1 aliceSk [bobVk, carolVk] allNodeIds hydraScriptsTxId $ \n1 ->
                withHydraNode tracer bobChainConfig tmpDir 2 bobSk [aliceVk, carolVk] allNodeIds hydraScriptsTxId $ \n2 ->
                  withHydraNode tracer carolChainConfig tmpDir 3 carolSk [aliceVk, bobVk] allNodeIds hydraScriptsTxId $ \n3 -> do
                    -- Funds to be used as fuel by Hydra protocol transactions
                    seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
                    waitForNodesConnected tracer [n1, n2, n3]
                    send n1 $ input "Init" []
                    void $ waitForAllMatch 3 [n1] $ headIsInitializingWith (Set.fromList [alice, bob, carol])
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_events" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "logs its command line arguments" $ \tracer -> do
        withClusterTempDir "logs-options" $ \dir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) dir $ \RunningNode{nodeSocket} -> do
            let hydraSK = dir </> "hydra.sk"
            let cardanoSK = dir </> "cardano.sk"
            hydraSKey :: SigningKey HydraKey <- generate arbitrary
            (_, cardanoSKey) <- generateCardanoKey
            void $ writeFileTextEnvelope hydraSK Nothing hydraSKey
            void $ writeFileTextEnvelope cardanoSK Nothing cardanoSKey
            withCreateProcess (proc "hydra-node" ["-n", "hydra-node-1", "--testnet-magic", "42", "--hydra-signing-key", hydraSK, "--cardano-signing-key", cardanoSK, "--node-socket", nodeSocket]){std_out = CreatePipe} $
              \_ (Just nodeStdout) _ _ ->
                waitForLog 10 nodeStdout "JSON object with key NodeOptions" $ \line ->
                  line ^? key "message" . key "tag" == Just (Aeson.String "NodeOptions")

      it "detects misconfiguration" $ \tracer -> do
        withClusterTempDir "detect-misconfiguration" $ \dir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) dir $ \node@RunningNode{nodeSocket} -> do
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            let persistenceDir = dir </> "persistence"
            let cardanoSK = dir </> "cardano.sk"
            let hydraSK = dir </> "hydra.sk"

            (_, cardanoSKey) <- generateCardanoKey
            hydraSKey :: SigningKey HydraKey <- generate arbitrary

            void $ writeFileTextEnvelope hydraSK Nothing hydraSKey
            void $ writeFileTextEnvelope cardanoSK Nothing cardanoSKey

            let alice = deriveParty hydraSKey
                -- we do not care about the chain state for this test
                newChainState = ChainState.initialChainState
            headId <- generate arbitrary
            contestationPeriod <- generate arbitrary
            initialSnapshot <- generate arbitrary
            -- given a list of random events that leads to some Open state
            let events =
                  fromStrict . BS8.unlines $
                    toStrict . Aeson.encode
                      <$> [ HeadInitialized{headId, contestationPeriod, parties = [alice], newChainState}
                          , TxCommitted{remainingParties = Set.empty, newCommitted = Map.empty, newChainState}
                          , HeadOpened{initialSnapshot, u0 = mempty, newChainState}
                          ]
            -- persist the random events
            void $ do
              createDirectoryIfMissing True persistenceDir
              BSL.writeFile (persistenceDir </> "state") events

            -- generate invalid node args to trigger the error
            -- by using a different contestationPeriod
            contestationPeriod' <- generate $ arbitrary `suchThat` (/= contestationPeriod)
            let nodeArgs =
                  toArgs
                    defaultRunOptions
                      { chainConfig =
                          defaultChainConfig
                            { cardanoSigningKey = cardanoSK
                            , nodeSocket
                            , contestationPeriod = contestationPeriod'
                            }
                      , hydraSigningKey = hydraSK
                      , hydraScriptsTxId
                      , persistenceDir
                      , ledgerConfig =
                          defaultLedgerConfig
                            { cardanoLedgerProtocolParametersFile = "config/protocol-parameters.json"
                            }
                      }

            -- expecting misconfiguration because nodeArgs will missmatch
            -- with the node head-state recovered from persisted events.
            withCreateProcess (proc "hydra-node" nodeArgs){std_out = CreatePipe, std_err = CreatePipe} $
              \_ (Just nodeStdout) (Just nodeStdErr) _ -> do
                -- we should be able to observe the log
                waitForLog 10 nodeStdout "Detect Misconfiguration log" $ \outline ->
                  outline ^? key "message" . key "tag" == Just (Aeson.String "Misconfiguration")

                -- node should exit with appropriate exception
                waitForLog 10 nodeStdErr "Detect ParamMismatchError" $ \errline ->
                  let allLogLines = lines errline
                      expectedLog =
                        "hydra-node: ParamMismatchError \"Loaded state does not match given command line options. Please check the state in: " <> pack persistenceDir <> " against provided command line options.\""
                   in expectedLog `elem` allLogLines

waitForLog :: NominalDiffTime -> Handle -> Text -> (Text -> Bool) -> IO ()
waitForLog delay nodeOutput failureMessage predicate = do
  seenLogs <- newTVarIO []
  timeout (truncate delay * 1_000_000) (go seenLogs) >>= \case
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
  let aliceSk = generateSigningKey "alice-timed"
  let alice = deriveParty aliceSk
  let contestationPeriod = UnsafeContestationPeriod 2
  aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket [] contestationPeriod
  withHydraNode tracer aliceChainConfig tmpDir 1 aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
    waitForNodesConnected tracer [n1]
    let lovelaceBalanceValue = 100_000_000

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ node aliceCardanoVk lovelaceBalanceValue Fuel (contramap FromFaucet tracer)
    send n1 $ input "Init" []
    headId <-
      waitForAllMatch 10 [n1] $
        headIsInitializingWith (Set.fromList [alice])

    -- Get some UTXOs to commit to a head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
    externalCommit n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

    waitFor tracer 3 [n1] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice, "headId" .= headId]

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
    waitFor tracer 3 [n1] $
      output "TxValid" ["transaction" .= tx, "headId" .= headId]

    confirmedTransactions <- waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      v ^? key "snapshot" . key "confirmedTransactions"
    confirmedTransactions ^.. values `shouldBe` [toJSON $ txId tx]

initAndClose :: FilePath -> Tracer IO EndToEndLog -> Int -> TxId -> RunningNode -> IO ()
initAndClose tmpDir tracer clusterIx hydraScriptsTxId node@RunningNode{nodeSocket, networkId} = do
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
  bobKeys@(bobCardanoVk, _) <- generate genKeyPair
  carolKeys@(carolCardanoVk, _) <- generate genKeyPair

  let aliceSk = generateSigningKey ("alice-" <> show clusterIx)
  let bobSk = generateSigningKey ("bob-" <> show clusterIx)
  let carolSk = generateSigningKey ("carol-" <> show clusterIx)

  let alice = deriveParty aliceSk
  let bob = deriveParty bobSk
  let carol = deriveParty carolSk

  let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
      hydraKeys = [aliceSk, bobSk, carolSk]

  let firstNodeId = clusterIx * 3
  let contestationPeriod = UnsafeContestationPeriod 2
  withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
    let [n1, n2, n3] = toList nodes
    waitForNodesConnected tracer [n1, n2, n3]

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
    seedFromFaucet_ node bobCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
    seedFromFaucet_ node carolCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

    send n1 $ input "Init" []
    headId <-
      waitForAllMatch 10 [n1, n2, n3] $
        headIsInitializingWith (Set.fromList [alice, bob, carol])

    -- Get some UTXOs to commit to a head
    (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
    committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
    externalCommit n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

    (bobExternalVk, bobExternalSk) <- generate genKeyPair
    committedUTxOByBob <- seedFromFaucet node bobExternalVk bobCommittedToHead Normal (contramap FromFaucet tracer)
    externalCommit n2 committedUTxOByBob <&> signTx bobExternalSk >>= submitTx node

    externalCommit n3 mempty >>= submitTx node

    waitFor tracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob), "headId" .= headId]

    -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
    let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
    let Right tx =
          mkSimpleTx
            firstCommittedUTxO
            (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
            aliceExternalSk
    send n1 $ input "NewTx" ["transaction" .= tx]
    waitFor tracer 10 [n1, n2, n3] $
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

    let expectedSnapshot =
          object
            [ "snapshotNumber" .= int expectedSnapshotNumber
            , "utxo" .= newUTxO
            , "confirmedTransactions" .= [txId tx]
            ]
        expectedSnapshotNumber = 1

    waitMatch 10 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshot <- v ^? key "snapshot"
      guard $ snapshot == expectedSnapshot

    send n1 $ input "GetUTxO" []
    waitFor tracer 10 [n1] $ output "GetUTxOResponse" ["utxo" .= newUTxO, "headId" .= headId]

    send n1 $ input "Close" []
    deadline <- waitMatch 3 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsClosed"
      guard $ v ^? key "headId" == Just (toJSON headId)
      snapshotNumber <- v ^? key "snapshotNumber"
      guard $ snapshotNumber == toJSON expectedSnapshotNumber
      v ^? key "contestationDeadline" . _JSON

    -- Expect to see ReadyToFanout within 3 seconds after deadline
    remainingTime <- diffUTCTime deadline <$> getCurrentTime
    waitFor tracer (remainingTime + 3) [n1] $
      output "ReadyToFanout" ["headId" .= headId]

    send n1 $ input "Fanout" []
    waitFor tracer 3 [n1] $
      output "HeadIsFinalized" ["utxo" .= newUTxO, "headId" .= headId]

    case fromJSON $ toJSON newUTxO of
      Error err ->
        failure $ "newUTxO isn't valid JSON?: " <> err
      Data.Aeson.Success u ->
        failAfter 5 $ waitForUTxO networkId nodeSocket u

--
-- Fixtures
--

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

--
-- Helpers
--

int :: Int -> Int
int = id

outputRef :: TxId -> Natural -> Value
outputRef tid tix =
  object
    [ "txId" .= tid
    , "index" .= tix
    ]
