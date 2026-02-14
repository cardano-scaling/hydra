{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Hydra.Cluster.HydraClientSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (
  withCardanoNodeDevnet,
 )
import Control.Lens ((^?))
import Data.Aeson ((.=))
import Data.Aeson.Lens (key)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hydra.Cardano.Api hiding (Value, cardanoEra, queryGenesisParameters, txId)
import Hydra.Chain.Backend (ChainBackend)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Direct (DirectBackend (..))
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
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options (ChainBackendOptions (..), DirectOptions (..))
import Hydra.Tx (HeadId, IsTx (..))
import HydraNode (
  HydraClient (..),
  HydraNodeLog,
  getSnapshotUTxO,
  input,
  output,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  waitNoMatch,
  withConnectionToNodeHost,
  withHydraCluster,
 )
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)
import Prelude qualified

spec :: Spec
spec = around (showLogsOnFailure "HydraClientSpec") $ do
  it "should filter SnapshotConfirmed by provided address" $ \tracer -> do
    failAfter 60 $
      withTempDir "hydra-client" $ \tmpDir ->
        filterSnapshotConfirmedByAddressScenario tracer tmpDir
  it "should filter out SnapshotConfirmed when given a random address" $ \tracer -> do
    failAfter 60 $
      withTempDir "hydra-client" $ \tmpDir ->
        filterSnapshotConfirmedByRandomAddressScenario tracer tmpDir
  it "should filter out SnapshotConfirmed when given a wrong address" $ \tracer -> do
    failAfter 60 $
      withTempDir "hydra-client" $ \tmpDir ->
        filterSnapshotConfirmedByWrongAddressScenario tracer tmpDir

filterSnapshotConfirmedByAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterSnapshotConfirmedByAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \_ backend nodes hydraTracer -> do
    (expectedSnapshotNumber, initialTxId, headId, (aliceExternalVk, _), (bobExternalVk, bobExternalSk)) <-
      prepareScenario backend nodes tracer
    let [n1, n2, _] = toList nodes

    -- 1/ query alice address from alice node -> Does see the tx
    runScenario hydraTracer n1 (textAddrOf aliceExternalVk) $ \con -> do
      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        guard $ v ^? key "transactionId" == Just (toJSON initialTxId)

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

    -- 2/ query bob address from bob node -> Does see the tx
    runScenario hydraTracer n2 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        guard $ v ^? key "transactionId" == Just (toJSON initialTxId)

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

    -- 3/ query bob address from alice node -> Does see the tx
    runScenario hydraTracer n1 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        guard $ v ^? key "transactionId" == Just (toJSON initialTxId)

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

    -- 4/ query alice address from alice node -> Does not see the bob-self tx
    newExpectedSnapshotNumber <-
      runScenario hydraTracer n1 (textAddrOf aliceExternalVk) $ \con -> do
        -- XXX: perform a new tx while the connection query by address is open.
        utxo <- getSnapshotUTxO n1
        newTx <- sendTransferTx nodes utxo bobExternalSk bobExternalVk
        waitFor hydraTracer 10 (toList nodes) $
          output "TxValid" ["transactionId" .= txId newTx, "headId" .= headId]

        let newExpectedSnapshotNumber = expectedSnapshotNumber + 1
        waitMatch 10 n1 $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "headId" == Just (toJSON headId)
          snapshotNumber <- v ^? key "snapshot" . key "number"
          guard $ snapshotNumber == toJSON newExpectedSnapshotNumber

        waitNoMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "headId" == Just (toJSON headId)
          snapshotNumber <- v ^? key "snapshot" . key "number"
          guard $ snapshotNumber == toJSON newExpectedSnapshotNumber

        pure newExpectedSnapshotNumber

    -- 5/ query bob address from alice node -> Does see both tx from history.
    runScenario hydraTracer n1 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        guard $ v ^? key "transactionId" == Just (toJSON initialTxId)

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON newExpectedSnapshotNumber

    -- 6/ query bob address from alice node -> Does see new bob-self tx
    runScenario hydraTracer n1 (textAddrOf bobExternalVk) $ \con -> do
      -- XXX: perform a new tx while the connection query by address is open.
      utxo <- getSnapshotUTxO n1
      newTx <- sendTransferTx nodes utxo bobExternalSk bobExternalVk
      waitFor hydraTracer 10 (toList nodes) $
        output "TxValid" ["transactionId" .= txId newTx, "headId" .= headId]

      let newExpectedSnapshotNumber' = newExpectedSnapshotNumber + 1
      waitMatch 10 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON newExpectedSnapshotNumber'

      -- XXX: the connection does observe the new tx
      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        guard $ v ^? key "transactionId" == Just (toJSON initialTxId)

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON newExpectedSnapshotNumber'

filterSnapshotConfirmedByRandomAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterSnapshotConfirmedByRandomAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \_ node nodes hydraTracer -> do
    (expectedSnapshotNumber, _, headId, _, _) <- prepareScenario node nodes tracer
    let [n1, _, _] = toList nodes

    (randomVk, _) <- generate genKeyPair
    runScenario hydraTracer n1 (textAddrOf randomVk) $ \con -> do
      waitNoMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

filterSnapshotConfirmedByWrongAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterSnapshotConfirmedByWrongAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \_ node nodes hydraTracer -> do
    (expectedSnapshotNumber, _, headId, _, _) <- prepareScenario node nodes tracer
    let [_, _, n3] = toList nodes

    runScenario hydraTracer n3 "invalid" $ \con -> do
      waitNoMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

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
  (NominalDiffTime -> DirectBackend -> NonEmpty HydraClient -> Tracer IO HydraNodeLog -> IO a) ->
  IO a
scenarioSetup tracer tmpDir action = do
  withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \blockTime backend -> do
    let nodeSocket' = case Backend.getOptions backend of
          Direct DirectOptions{nodeSocket} -> nodeSocket
          _ -> error "Unexpected Blockfrost backend"
    aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
    bobKeys@(bobCardanoVk, _) <- generate genKeyPair
    carolKeys@(carolCardanoVk, _) <- generate genKeyPair

    let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
        hydraKeys = [aliceSk, bobSk, carolSk]

    let firstNodeId = 1
    hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
    let contestationPeriod = 2
    let hydraTracer = contramap FromHydraNode tracer

    withHydraCluster hydraTracer backend tmpDir nodeSocket' firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \nodes -> do
      let [n1, n2, n3] = toList nodes
      waitForNodesConnected hydraTracer 20 $ n1 :| [n2, n3]

      -- Funds to be used as fuel by Hydra protocol transactions
      seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
      seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
      seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)
      action blockTime backend nodes hydraTracer

prepareScenario ::
  ChainBackend backend =>
  backend ->
  NonEmpty HydraClient ->
  Tracer IO EndToEndLog ->
  IO (Int, TxId, HeadId, (VerificationKey PaymentKey, SigningKey PaymentKey), (VerificationKey PaymentKey, SigningKey PaymentKey))
prepareScenario backend nodes tracer = do
  let [n1, n2, n3] = toList nodes
  let hydraTracer = contramap FromHydraNode tracer

  send n1 $ input "Init" []
  headId <-
    waitForAllMatch 10 [n1, n2, n3] $
      headIsInitializingWith (Set.fromList [alice, bob, carol])

  -- Get some UTXOs to commit to a head
  aliceKeys@(aliceExternalVk, aliceExternalSk) <- generate genKeyPair
  committedUTxOByAlice <- seedFromFaucet backend aliceExternalVk (lovelaceToValue aliceCommittedToHead) (contramap FromFaucet tracer)
  requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= Backend.submitTransaction backend

  bobKeys@(bobExternalVk, bobExternalSk) <- generate genKeyPair
  committedUTxOByBob <- seedFromFaucet backend bobExternalVk (lovelaceToValue bobCommittedToHead) (contramap FromFaucet tracer)
  requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk >>= Backend.submitTransaction backend

  requestCommitTx n3 mempty >>= Backend.submitTransaction backend

  let u0 = committedUTxOByAlice <> committedUTxOByBob

  waitFor hydraTracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= u0, "headId" .= headId]

  -- Create an arbitrary transaction using some input to have history.
  tx <- sendTx nodes committedUTxOByAlice aliceExternalSk bobExternalVk paymentFromAliceToBob
  waitFor hydraTracer 10 (toList nodes) $
    output "TxValid" ["transactionId" .= txId tx, "headId" .= headId]

  let expectedSnapshotNumber :: Int = 1

  waitMatch 10 n1 $ \v -> do
    guard $ v ^? key "tag" == Just "SnapshotConfirmed"
    guard $ v ^? key "headId" == Just (toJSON headId)
    snapshotNumber <- v ^? key "snapshot" . key "number"
    guard $ snapshotNumber == toJSON expectedSnapshotNumber

  pure (expectedSnapshotNumber, txId tx, headId, aliceKeys, bobKeys)

-- NOTE: this is partial and will fail if we are not able to generate a payment
sendTx :: NonEmpty HydraClient -> UTxO -> SigningKey PaymentKey -> VerificationKey PaymentKey -> Lovelace -> IO Tx
sendTx nodes senderUTxO sender receiver amount = do
  let utxo = Prelude.head $ UTxO.toList senderUTxO
  let Right tx =
        mkSimpleTx
          utxo
          (inHeadAddress receiver, lovelaceToValue amount)
          sender
  send (head nodes) $ input "NewTx" ["transaction" .= tx]
  pure tx

sendTransferTx :: NonEmpty HydraClient -> UTxO -> SigningKey PaymentKey -> VerificationKey PaymentKey -> IO Tx
sendTransferTx nodes utxo sender receiver = do
  tx <- mkTransferTx testNetworkId utxo sender receiver
  send (head nodes) $ input "NewTx" ["transaction" .= tx]
  pure tx

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
