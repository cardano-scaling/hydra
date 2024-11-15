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
import Control.Lens ((^?))
import Data.Aeson ((.=))
import Data.Aeson.Lens (key)
import Data.Aeson.Types (parseMaybe)
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
import HydraNode (HydraClient (..), HydraNodeLog, input, output, requestCommitTx, send, waitFor, waitForAllMatch, waitForNodesConnected, waitMatch, waitNoMatch, withConnectionToNodeHost, withHydraCluster)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)
import Prelude qualified

spec :: Spec
spec = around (showLogsOnFailure "HydraClientSpec") $ do
  describe "HydraClient on Cardano devnet" $ do
    describe "hydra-client" $ do
      fit "should filter TxValid by provided address" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterTxValidByAddressScenario tracer tmpDir
      fit "should filter out TxValid when given a random address" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterTxValidByRandomAddressScenario tracer tmpDir
      fit "should filter out TxValid when given a wrong address" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterTxValidByWrongAddressScenario tracer tmpDir

filterTxValidByAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterTxValidByAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \node nodes hydraTracer -> do
    (expectedTxId, (aliceExternalVk, bobExternalVk)) <- prepareScenario node nodes tracer
    let [n1, n2, n3] = toList nodes

    -- 1/ query alice address from alice node -> Does see the tx
    runScenario hydraTracer n1 (textAddrOf aliceExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == expectedTxId

    -- 2/ query bob address from bob node -> Does see the tx
    runScenario hydraTracer n2 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == expectedTxId

    -- 3/ query bob address from carol node -> Does see the tx
    runScenario hydraTracer n3 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == expectedTxId

filterTxValidByRandomAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterTxValidByRandomAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \node nodes hydraTracer -> do
    (expectedTxId, _) <- prepareScenario node nodes tracer
    let [n1, _, _] = toList nodes

    (randomVk, _) <- generate genKeyPair
    runScenario hydraTracer n1 (textAddrOf randomVk) $ \con -> do
      waitNoMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == expectedTxId

filterTxValidByWrongAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterTxValidByWrongAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \node nodes hydraTracer -> do
    (expectedTxId, _) <- prepareScenario node nodes tracer
    let [_, _, n3] = toList nodes

    runScenario hydraTracer n3 "invalid" $ \con -> do
      waitNoMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == expectedTxId

-- * Helpers
unwrapAddress :: AddressInEra -> Text
unwrapAddress = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "Byron."

textAddrOf :: VerificationKey PaymentKey -> Text
textAddrOf vk = unwrapAddress (mkVkAddress @Era testNetworkId vk)

queryAddress :: Text -> Text
queryAddress addr = "/?history=yes&address=" <> addr

runScenario ::
  Tracer IO HydraNodeLog ->
  HydraClient ->
  Text ->
  (HydraClient -> IO a) ->
  IO a
runScenario hydraTracer hnode addr action = do
  withConnectionToNodeHost
    hydraTracer
    (HydraNode.hydraNodeId hnode)
    (HydraNode.apiHost hnode)
    (Just $ Text.unpack (queryAddress addr))
    action

scenarioSetup ::
  Tracer IO EndToEndLog ->
  FilePath ->
  (RunningNode -> NonEmpty HydraClient -> Tracer IO HydraNodeLog -> IO a) ->
  IO a
scenarioSetup tracer tmpDir action = do
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

      action node nodes hydraTracer

prepareScenario ::
  RunningNode ->
  NonEmpty HydraClient ->
  Tracer IO EndToEndLog ->
  IO (TxId, (VerificationKey PaymentKey, VerificationKey PaymentKey))
prepareScenario node nodes tracer = do
  let [n1, n2, n3] = toList nodes
  let hydraTracer = contramap FromHydraNode tracer

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
  -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
  let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
  let Right tx =
        mkSimpleTx
          firstCommittedUTxO
          (inHeadAddress bobExternalVk, lovelaceToValue paymentFromAliceToBob)
          aliceExternalSk
  send n1 $ input "NewTx" ["transaction" .= tx]
  waitFor hydraTracer 10 [n1, n2, n3] $
    output "TxValid" ["transactionId" .= txId tx, "headId" .= headId, "transaction" .= tx]
  pure (txId tx, (aliceExternalVk, bobExternalVk))

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
