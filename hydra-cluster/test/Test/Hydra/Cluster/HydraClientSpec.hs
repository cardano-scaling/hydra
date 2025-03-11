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
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Tx (HeadId, IsTx (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Tx.DepositDeadline (DepositDeadline (UnsafeDepositDeadline))
import HydraNode (HydraClient (..), HydraNodeLog, getSnapshotUTxO, input, output, requestCommitTx, send, waitFor, waitForAllMatch, waitForNodesConnected, waitMatch, waitNoMatch, withConnectionToNodeHost, withHydraCluster)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)
import Prelude qualified

spec :: Spec
spec = around (showLogsOnFailure "HydraClientSpec") $ do
  describe "HydraClient on Cardano devnet" $ do
    describe "hydra-client" $ do
      it "should filter TxValid by provided address" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterTxValidByAddressScenario tracer tmpDir
      it "should filter out TxValid when given a random address" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterTxValidByRandomAddressScenario tracer tmpDir
      it "should filter out TxValid when given a wrong address" $ \tracer -> do
        failAfter 60 $
          withTempDir "hydra-client" $ \tmpDir ->
            filterTxValidByWrongAddressScenario tracer tmpDir

filterTxValidByAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterTxValidByAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \node nodes hydraTracer -> do
    (expectedSnapshotNumber, initialTxId, headId, (aliceExternalVk, _), (bobExternalVk, bobExternalSk)) <-
      prepareScenario node nodes tracer
    let [n1, n2, _] = toList nodes

    -- 1/ query alice address from alice node -> Does see the tx
    runScenario hydraTracer n1 (textAddrOf aliceExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == initialTxId

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

    -- 2/ query bob address from bob node -> Does see the tx
    runScenario hydraTracer n2 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == initialTxId

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

    -- 3/ query bob address from alice node -> Does see the tx
    runScenario hydraTracer n1 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == initialTxId

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

    -- 4/ query alice address from alice node -> Does not see the bob-self tx
    (newTxId, newExpectedSnapshotNumber) <-
      runScenario hydraTracer n1 (textAddrOf aliceExternalVk) $ \con -> do
        -- XXX: perform a new tx while the connection query by address is open.
        utxo <- getSnapshotUTxO n1
        newTx <- sendTransferTx nodes utxo bobExternalSk bobExternalVk
        waitFor hydraTracer 10 (toList nodes) $
          output "TxValid" ["transactionId" .= txId newTx, "headId" .= headId, "transaction" .= newTx]

        let newExpectedSnapshotNumber = expectedSnapshotNumber + 1
        waitMatch 10 n1 $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "headId" == Just (toJSON headId)
          snapshotNumber <- v ^? key "snapshot" . key "number"
          guard $ snapshotNumber == toJSON newExpectedSnapshotNumber

        -- XXX: the connection does not observe the new tx
        waitNoMatch 3 con $ \v -> do
          guard $ v ^? key "tag" == Just "TxValid"
          tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
          guard $ txId tx == txId newTx

        waitNoMatch 10 con $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "headId" == Just (toJSON headId)
          snapshotNumber <- v ^? key "snapshot" . key "number"
          guard $ snapshotNumber == toJSON newExpectedSnapshotNumber

        pure (txId newTx, newExpectedSnapshotNumber)

    -- 5/ query bob address from alice node -> Does see both tx from history.
    runScenario hydraTracer n1 (textAddrOf bobExternalVk) $ \con -> do
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == initialTxId

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == newTxId

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
        output "TxValid" ["transactionId" .= txId newTx, "headId" .= headId, "transaction" .= newTx]

      let newExpectedSnapshotNumber' = newExpectedSnapshotNumber + 1
      waitMatch 10 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON newExpectedSnapshotNumber'

      -- XXX: the connection does observe the new tx
      waitMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == txId newTx

      waitMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON newExpectedSnapshotNumber'

filterTxValidByRandomAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterTxValidByRandomAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \node nodes hydraTracer -> do
    (expectedSnapshotNumber, initialTxId, headId, _, _) <- prepareScenario node nodes tracer
    let [n1, _, _] = toList nodes

    (randomVk, _) <- generate genKeyPair
    runScenario hydraTracer n1 (textAddrOf randomVk) $ \con -> do
      waitNoMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == initialTxId

      waitNoMatch 10 con $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        snapshotNumber <- v ^? key "snapshot" . key "number"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

filterTxValidByWrongAddressScenario :: Tracer IO EndToEndLog -> FilePath -> IO ()
filterTxValidByWrongAddressScenario tracer tmpDir = do
  scenarioSetup tracer tmpDir $ \node nodes hydraTracer -> do
    (expectedSnapshotNumber, initialTxId, headId, _, _) <- prepareScenario node nodes tracer
    let [_, _, n3] = toList nodes

    runScenario hydraTracer n3 "invalid" $ \con -> do
      waitNoMatch 3 con $ \v -> do
        guard $ v ^? key "tag" == Just "TxValid"
        tx :: Tx <- v ^? key "transaction" >>= parseMaybe parseJSON
        guard $ txId tx == initialTxId

      waitNoMatch 10 con $ \v -> do
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
    let depositDeadline = UnsafeDepositDeadline 200
    let hydraTracer = contramap FromHydraNode tracer
    withHydraCluster hydraTracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod depositDeadline $ \nodes -> do
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
  IO (Int, TxId, HeadId, (VerificationKey PaymentKey, SigningKey PaymentKey), (VerificationKey PaymentKey, SigningKey PaymentKey))
prepareScenario node nodes tracer = do
  let [n1, n2, n3] = toList nodes
  let hydraTracer = contramap FromHydraNode tracer

  send n1 $ input "Init" []
  headId <-
    waitForAllMatch 10 [n1, n2, n3] $
      headIsInitializingWith (Set.fromList [alice, bob, carol])

  -- Get some UTXOs to commit to a head
  aliceKeys@(aliceExternalVk, aliceExternalSk) <- generate genKeyPair
  committedUTxOByAlice <- seedFromFaucet node aliceExternalVk aliceCommittedToHead (contramap FromFaucet tracer)
  requestCommitTx n1 committedUTxOByAlice <&> signTx aliceExternalSk >>= submitTx node

  bobKeys@(bobExternalVk, bobExternalSk) <- generate genKeyPair
  committedUTxOByBob <- seedFromFaucet node bobExternalVk bobCommittedToHead (contramap FromFaucet tracer)
  requestCommitTx n2 committedUTxOByBob <&> signTx bobExternalSk >>= submitTx node

  requestCommitTx n3 mempty >>= submitTx node

  let u0 = committedUTxOByAlice <> committedUTxOByBob

  waitFor hydraTracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= u0, "headId" .= headId]

  -- Create an arbitrary transaction using some input to have history.
  tx <- sendTx nodes committedUTxOByAlice aliceExternalSk bobExternalVk paymentFromAliceToBob
  waitFor hydraTracer 10 (toList nodes) $
    output "TxValid" ["transactionId" .= txId tx, "headId" .= headId, "transaction" .= tx]

  let expectedSnapshotNumber :: Int = 1

  waitMatch 10 n1 $ \v -> do
    guard $ v ^? key "tag" == Just "SnapshotConfirmed"
    guard $ v ^? key "headId" == Just (toJSON headId)
    snapshotNumber <- v ^? key "snapshot" . key "number"
    guard $ snapshotNumber == toJSON expectedSnapshotNumber

  pure (expectedSnapshotNumber, txId tx, headId, aliceKeys, bobKeys)

-- NOTE(AB): this is partial and will fail if we are not able to generate a payment
sendTx :: NonEmpty HydraClient -> UTxO' (TxOut CtxUTxO) -> SigningKey PaymentKey -> VerificationKey PaymentKey -> Lovelace -> IO Tx
sendTx nodes senderUTxO sender receiver amount = do
  let utxo = Prelude.head $ UTxO.pairs senderUTxO
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
