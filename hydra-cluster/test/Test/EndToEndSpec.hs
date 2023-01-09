{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (queryTip, waitForUTxO)
import CardanoNode (RunningNode (..), withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson (Result (..), Value (Null, Object, String), fromJSON, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _JSON)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  AddressInEra,
  Key (SigningKey),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  TxId,
  TxIn (..),
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  serialiseAddress,
  writeFileTextEnvelope,
 )
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
  headIsInitializedWith,
  restartedNodeCanAbort,
  restartedNodeCanObserveCommitTx,
  singlePartyHeadFullLifeCycle,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options (ChainConfig (startChainFrom))
import Hydra.Party (deriveParty)
import HydraNode (
  CreateProcess (std_out),
  EndToEndLog (..),
  StdStream (CreatePipe),
  getMetrics,
  input,
  output,
  proc,
  readCreateProcess,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
 )
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (hGetLine)
import System.Process (withCreateProcess)
import Test.QuickCheck (generate)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import qualified Prelude

allNodeIds :: [Int]
allNodeIds = [1 .. 3]

spec :: Spec
spec = around showLogsOnFailure $ do
  describe "End-to-end on Cardano devnet" $ do
    describe "single party hydra head" $ do
      it "full head life-cycle" $ \tracer -> do
        withTempDir "hydra-cluster-end-to-end" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= singlePartyHeadFullLifeCycle tracer tmpDir node
      it "can close with long deadline" $ \tracer -> do
        withTempDir "hydra-cluster-end-to-end" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= canCloseWithLongContestationPeriod tracer tmpDir node

    describe "three hydra nodes scenario" $ do
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              initAndClose tracer 1 hydraScriptsTxId node

      it "inits a Head and closes it immediately " $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
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
                waitForAllMatch 10 [n1, n2, n3] $
                  headIsInitializedWith (Set.fromList [alice, bob, carol])

                -- Get some UTXOs to commit to a head
                committedUTxOByAlice <- seedFromFaucet node aliceCardanoVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
                committedUTxOByBob <- seedFromFaucet node bobCardanoVk bobCommittedToHead Normal (contramap FromFaucet tracer)
                send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
                send n2 $ input "Commit" ["utxo" .= committedUTxOByBob]
                send n3 $ input "Commit" ["utxo" .= Object mempty]

                let u0 = committedUTxOByAlice <> committedUTxOByBob

                waitFor tracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= u0]

                send n1 $ input "Close" []
                deadline <- waitMatch 3 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsClosed"
                  snapshotNumber <- v ^? key "snapshotNumber"
                  guard $ snapshotNumber == Aeson.Number 0
                  v ^? key "contestationDeadline" . _JSON

                -- Expect to see ReadyToFanout within 3 seconds after deadline
                remainingTime <- diffUTCTime deadline <$> getCurrentTime
                waitFor tracer (truncate $ remainingTime + 3) [n1] $
                  output "ReadyToFanout" []

                send n1 $ input "Fanout" []
                waitFor tracer 3 [n1] $
                  output "HeadIsFinalized" ["utxo" .= u0]

    describe "restarting nodes" $ do
      it "can abort head after restart" $ \tracer -> do
        withTempDir "hydra-cluster-end-to-end" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= restartedNodeCanAbort tracer tmpDir node

      it "can observe a commit tx after a restart, even when a tx happened while down" $ \tracer -> do
        withTempDir "hydra-cluster-end-to-end" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node ->
            publishHydraScriptsAs node Faucet
              >>= restartedNodeCanObserveCommitTx tracer tmpDir node

      it "can start chain from the past and replay on-chain events" $ \tracer ->
        withTempDir "end-to-end-chain-observer" $ \tmp ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            let contestationPeriod = UnsafeContestationPeriod 10
            aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] contestationPeriod
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet
            let nodeId = 1
            tip <- withHydraNode tracer aliceChainConfig tmp nodeId aliceSk [] [1] hydraScriptsTxId $ \n1 -> do
              seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
              tip <- queryTip networkId nodeSocket
              send n1 $ input "Init" []
              waitForAllMatch 10 [n1] $ headIsInitializedWith (Set.fromList [alice])
              return tip

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
              waitForAllMatch 10 [n1] $ headIsInitializedWith (Set.fromList [alice])

      it "close of an initial snapshot from re-initialized node is contested" $ \tracer ->
        withTempDir "end-to-end-chain-observer" $ \tmp ->
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
            hydraScriptsTxId <- publishHydraScriptsAs node Faucet

            (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
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
                withAliceNode = withHydraNode tracer aliceChainConfig tmp aliceNodeId aliceSk [bobVk] allNodesIds hydraScriptsTxId
                withBobNode = withHydraNode tracer bobChainConfig tmp bobNodeId bobSk [aliceVk] allNodesIds hydraScriptsTxId

            withAliceNode $ \n1 -> do
              withBobNode $ \n2 -> do
                waitForNodesConnected tracer [n1, n2]

                send n1 $ input "Init" []
                waitForAllMatch 10 [n1, n2] $ headIsInitializedWith (Set.fromList [alice, bob])

                committedUTxOByAlice <- seedFromFaucet node aliceCardanoVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
                send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
                send n2 $ input "Commit" ["utxo" .= Object mempty]
                waitFor tracer 10 [n1, n2] $ output "HeadIsOpen" ["utxo" .= committedUTxOByAlice]

                -- Create an arbitrary transaction using some input.
                let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
                let Right tx =
                      mkSimpleTx
                        firstCommittedUTxO
                        (inHeadAddress bobCardanoVk, lovelaceToValue paymentFromAliceToBob)
                        aliceCardanoSk
                send n1 $ input "NewTx" ["transaction" .= tx]

                waitMatch 10 n1 $ \v -> do
                  guard $ v ^? key "tag" == Just "SnapshotConfirmed"

              -- NOTE: Need to clear state on disk to have bob close with
              -- initial snapshot
              removeDirectoryRecursive $ tmp </> "state-" <> show bobNodeId

              withBobNode $ \n2 -> do
                waitMatch 10 n2 $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsOpen"

                send n2 $ input "Close" []

                let isHeadClosedWith0 v = do
                      guard $ v ^? key "tag" == Just "HeadIsClosed"
                      snapshotNumber <- v ^? key "snapshotNumber"
                      guard $ snapshotNumber == toJSON (0 :: Word)
                waitMatch 10 n1 isHeadClosedWith0
                waitMatch 10 n2 isHeadClosedWith0

                waitFor tracer 10 [n1, n2] $ output "HeadIsContested" ["snapshotNumber" .= (1 :: Word)]

    describe "two hydra heads scenario" $ do
      it "two heads on the same network do not conflict" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node -> do
              hydraScriptsTxId <- publishHydraScriptsAs node Faucet
              concurrently_
                (initAndClose tracer 0 hydraScriptsTxId node)
                (initAndClose tracer 1 hydraScriptsTxId node)

      it "bob cannot abort alice's head" $ \tracer -> do
        failAfter 60 $
          withTempDir "end-to-end-two-heads" $ \tmpDir -> do
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
                  waitForAllMatch 10 [n1] $ headIsInitializedWith (Set.fromList [alice])

                  -- Bob opens and immediately aborts a Head with Alice, iow pulls Alice in
                  -- "his" Head
                  send n2 $ input "Init" []
                  waitForAllMatch 10 [n2] $ headIsInitializedWith (Set.fromList [alice, bob])

                  send n2 $ input "Abort" []
                  waitFor tracer 10 [n2] $
                    output "HeadIsAborted" ["utxo" .= Object mempty]

                  -- Alice should be able to continue working with her Head
                  send n1 $ input "Commit" ["utxo" .= Object mempty]
                  waitFor tracer 10 [n1] $
                    output "HeadIsOpen" ["utxo" .= Object mempty]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withTempDir "end-to-end-prometheus-metrics" $ \tmpDir -> do
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
                    waitForAllMatch 3 [n1] $ headIsInitializedWith (Set.fromList [alice, bob, carol])
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_events" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "display proper semantic version given it is passed --version argument" $ \_ ->
        failAfter 5 $ do
          version <- readCreateProcess (proc "hydra-node" ["--version"]) ""
          version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

      it "logs its command line arguments" $ \_ -> do
        failAfter 60 $
          withTempDir "temp-dir-to-check-hydra-logs" $ \dir -> do
            let hydraSK = dir </> "hydra.sk"
            hydraSKey :: SigningKey HydraKey <- generate arbitrary
            void $ writeFileTextEnvelope hydraSK Nothing hydraSKey
            withCreateProcess (proc "hydra-node" ["-n", "hydra-node-1", "--hydra-signing-key", hydraSK]){std_out = CreatePipe} $
              \_ (Just nodeOutput) _ _ -> do
                out <- hGetLine nodeOutput
                out ^? key "message" . key "tag" `shouldBe` Just (Aeson.String "NodeOptions")

initAndClose :: Tracer IO EndToEndLog -> Int -> TxId -> RunningNode -> IO ()
initAndClose tracer clusterIx hydraScriptsTxId node@RunningNode{nodeSocket, networkId} = do
  withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
    aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- generate genKeyPair
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
      waitForAllMatch 10 [n1, n2, n3] $
        headIsInitializedWith (Set.fromList [alice, bob, carol])

      -- Get some UTXOs to commit to a head
      committedUTxOByAlice <- seedFromFaucet node aliceCardanoVk aliceCommittedToHead Normal (contramap FromFaucet tracer)
      committedUTxOByBob <- seedFromFaucet node bobCardanoVk bobCommittedToHead Normal (contramap FromFaucet tracer)
      send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
      send n2 $ input "Commit" ["utxo" .= committedUTxOByBob]
      send n3 $ input "Commit" ["utxo" .= Object mempty]
      waitFor tracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]

      -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
      let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
      let Right tx =
            mkSimpleTx
              firstCommittedUTxO
              (inHeadAddress bobCardanoVk, lovelaceToValue paymentFromAliceToBob)
              aliceCardanoSk
      send n1 $ input "NewTx" ["transaction" .= tx]
      waitFor tracer 10 [n1, n2, n3] $
        output "TxSeen" ["transaction" .= tx]

      -- The expected new utxo set is the created payment to bob,
      -- alice's remaining utxo in head and whatever bot has
      -- committed to the head
      let newUTxO =
            Map.fromList
              [
                ( TxIn (txId tx) (toEnum 0)
                , object
                    [ "address" .= String (serialiseAddress $ inHeadAddress bobCardanoVk)
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
                    [ "address" .= String (serialiseAddress $ inHeadAddress aliceCardanoVk)
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
              , "confirmedTransactions" .= [tx]
              ]
          expectedSnapshotNumber = 1

      waitMatch 10 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        snapshot <- v ^? key "snapshot"
        guard $ snapshot == expectedSnapshot

      send n1 $ input "GetUTxO" []
      waitFor tracer 10 [n1] $ output "GetUTxOResponse" ["utxo" .= newUTxO]

      send n1 $ input "Close" []
      deadline <- waitMatch 3 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        snapshotNumber <- v ^? key "snapshotNumber"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber
        v ^? key "contestationDeadline" . _JSON

      -- Expect to see ReadyToFanout within 3 seconds after deadline
      remainingTime <- diffUTCTime deadline <$> getCurrentTime
      waitFor tracer (truncate $ remainingTime + 3) [n1] $
        output "ReadyToFanout" []

      send n1 $ input "Fanout" []
      waitFor tracer 3 [n1] $
        output "HeadIsFinalized" ["utxo" .= newUTxO]

      case fromJSON $ toJSON newUTxO of
        Error err ->
          failure $ "newUTxO isn't valid JSON?: " <> err
        Success u ->
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
