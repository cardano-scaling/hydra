{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Hydra.Cluster.HydraClientSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  RunningNode (..),
  submitTx,
 )
import CardanoNode (
  withCardanoNodeDevnet,
 )
import Control.Lens ((^..), (^?))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (AsJSON (_JSON), key, values, _JSON)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hydra.Cardano.Api hiding (Value, cardanoEra, queryGenesisParameters)
import Hydra.Chain.Direct.State ()
import Hydra.Cluster.Faucet (
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Faucet),
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
 )
import Hydra.Cluster.Scenarios (
  EndToEndLog (..),
  headIsInitializingWith,
 )
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Tx (IsTx (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import HydraNode (HydraClient (..), input, output, requestCommitTx, send, waitFor, waitForAllMatch, waitForNodesConnected, waitMatch, withConnectionToNodeHost, withHydraCluster)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)
import Prelude qualified

spec :: Spec
spec = around (showLogsOnFailure "HydraClientSpec") $ do
  describe "HydraClient on Cardano devnet" $ do
    describe "hydra-client" $ do
      fit "should filter confirmed UTxO by provided addressed" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterConfirmedUTxOByAddressScenario tracer tmpDir

filterConfirmedUTxOByAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterConfirmedUTxOByAddressScenario tracer tmpDir = do
  withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
    aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
    bobKeys@(bobCardanoVk, _) <- generate genKeyPair
    carolKeys@(carolCardanoVk, _) <- generate genKeyPair

    let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
        hydraKeys = [aliceSk, bobSk, carolSk]

    let firstNodeId = 1
    hydraScriptsTxId <- publishHydraScriptsAs node Faucet
    let contestationPeriod = UnsafeContestationPeriod 2
    let hydraTracer = contramap FromHydraNode tracer
    withHydraCluster hydraTracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
      let [n1, n2, n3] = toList nodes
      waitForNodesConnected hydraTracer 20 $ n1 :| [n2, n3]

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

      -- Create an arbitrary transaction using some input.
      -- XXX: This makes a scenario where bob has more than 1 output, alice a small one and carol none.
      let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
      let Right tx =
            mkSimpleTx
              firstCommittedUTxO
              (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
              aliceExternalSk
      send n1 $ input "NewTx" ["transaction" .= tx]

      waitFor hydraTracer 10 [n1, n2, n3] $
        output "TxValid" ["transactionId" .= txId tx, "headId" .= headId]

      -- 1/ query alice address from alice node
      runScenario hydraTracer n1 (textAddrOf aliceExternalVk) $ \con -> do
        confirmedSnapshot <- waitMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          Just v
        traceShowM $ "Scenario-1: " <> textAddrOf aliceExternalVk
        traceShowM confirmedSnapshot

      -- 2/ query bob address from bob node
      runScenario hydraTracer n2 (textAddrOf bobExternalVk) $ \con -> do
        confirmedSnapshot <- waitMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          Just v
        traceShowM $ "Scenario-2: " <> textAddrOf bobExternalVk
        traceShowM confirmedSnapshot

      -- 3/ query bob address from alice node
      runScenario hydraTracer n1 (textAddrOf bobExternalVk) $ \con -> do
        confirmedSnapshot <- waitMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          Just v
        traceShowM $ "Scenario-3: " <> textAddrOf bobExternalVk
        traceShowM confirmedSnapshot

      -- 4/ query random address
      (randomVk, _) <- generate genKeyPair
      runScenario hydraTracer n1 (textAddrOf randomVk) $ \con -> do
        confirmedSnapshot <- waitMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          Just v
        traceShowM $ "Scenario-4: " <> textAddrOf randomVk
        traceShowM confirmedSnapshot

      -- 5/ query wrong address
      runScenario hydraTracer n3 "pepe" $ \con -> do
        confirmedSnapshot <- waitMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          Just v
        traceShowM $ "Scenario-5: " <> "pepe"
        traceShowM confirmedSnapshot

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
 where
  unwrapAddress :: AddressInEra -> Text
  unwrapAddress = \case
    ShelleyAddressInEra addr -> serialiseToBech32 addr
    ByronAddressInEra{} -> error "Byron."

  textAddrOf vk = unwrapAddress (mkVkAddress @Era testNetworkId vk)

  queryAddress addr = "/?history=yes&address=" <> addr

  runScenario hydraTracer hnode addr action = do
    withConnectionToNodeHost
      hydraTracer
      (HydraNode.hydraNodeId hnode)
      (HydraNode.apiHost hnode)
      (Just $ Text.unpack (queryAddress addr))
      action

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
