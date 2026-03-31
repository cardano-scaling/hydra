{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.Cluster.Scenarios (
  module Hydra.Cluster.Scenarios,
  EndToEndLog (..),
) where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (HydraTestnet (..))

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity)
import Cardano.Ledger.Api (RewardAccount (..), Withdrawals (..), collateralInputsTxBodyL, hashScript, scriptTxWitsL, totalCollateralTxBodyL, withdrawalsTxBodyL)
import Cardano.Ledger.Api.PParams (AlonzoEraPParams, PParams, getLanguageView)
import Cardano.Ledger.Api.Tx (AsIx (..), EraTx, Redeemers (..), bodyTxL, datsTxWitsL, rdmrsTxWitsL, witsTxL)
import Cardano.Ledger.Api.Tx qualified as Ledger
import Cardano.Ledger.Api.Tx.Body (AlonzoEraTxBody, scriptIntegrityHashTxBodyL)
import Cardano.Ledger.Api.Tx.Wits (AlonzoEraTxWits, ConwayPlutusPurpose (ConwayRewarding))
import Cardano.Ledger.BaseTypes (Network (Testnet), StrictMaybe (..))
import Cardano.Ledger.Credential (Credential (ScriptHashObj))
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import CardanoClient (
  QueryPoint (QueryTip),
  SubmitTransactionException,
  waitForUTxO,
 )
import CardanoNode (EndToEndLog (..))
import Control.Concurrent.Async (concurrently, mapConcurrently_)
import Control.Lens ((.~), (?~), (^.), (^..), (^?))
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey, key, values, _JSON, _String)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (isInfixOf)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.API.HTTPServer (
  DraftCommitTxResponse (..),
  TransactionSubmitted (..),
 )
import Hydra.API.ServerOutput (HeadStatus (..))
import Hydra.Cardano.Api (
  Coin (..),
  Era,
  File (File),
  Key (SigningKey),
  KeyWitnessInCtx (..),
  LedgerProtocolParameters (..),
  PaymentKey,
  Tx,
  TxId (..),
  TxOutDatum,
  UTxO,
  addTxIns,
  addTxInsCollateral,
  addTxOut,
  addTxOuts,
  createAndValidateTransactionBody,
  defaultTxBodyContent,
  fromCtxUTxOTxOut,
  fromLedgerTx,
  getTxBody,
  getTxId,
  getVerificationKey,
  lovelaceToValue,
  makeSignedTransaction,
  mkScriptAddress,
  mkScriptDatum,
  mkScriptWitness,
  mkTxIn,
  mkTxOutAutoBalance,
  mkTxOutDatumHash,
  mkVkAddress,
  modifyTxOutValue,
  scriptWitnessInCtx,
  selectLovelace,
  setTxProtocolParams,
  signTx,
  toLedgerData,
  toLedgerExUnits,
  toLedgerScript,
  toLedgerTx,
  toLedgerTxIn,
  toScriptData,
  txOutValue,
  txOuts',
  utxoFromTx,
  writeFileTextEnvelope,
  pattern BuildTxWith,
  pattern KeyWitness,
  pattern PlutusScriptWitness,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Cardano.Api qualified as CAPI
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.Backend (ChainBackend, buildTransaction, buildTransactionWithPParams, buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.Cluster.Faucet (createOutputAtAddress, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk, carolVk)
import Hydra.Cluster.Util (Timing (..), chainConfigFor, chainConfigFor', depositTimeout, keysFor, mkTestTiming, mkTestTiming', modifyConfig, setNetworkId)
import Hydra.Contract.Dummy (dummyRewardingScript, dummyValidatorScript)
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx, unsafeBuildTransaction)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node.State (SyncedStatus (..))
import Hydra.Node.UnsyncedPeriod (defaultUnsyncedPeriodFor, unsyncedPeriodToNominalDiffTime)
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), ChainConfig (..), DirectOptions (..), RunOptions (..), startChainFrom)
import Hydra.Tx (HeadId (..), IsTx (balance), Party, txId)
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Deposit (constructDepositUTxO)
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import HydraNode (
  HydraClient (..),
  getProtocolParameters,
  getSnapshotConfirmed,
  getSnapshotUTxO,
  input,
  output,
  postDecommit,
  prepareHydraNode,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitForNodesDisconnected,
  waitForNodesSynced,
  waitMatch,
  withHydraCluster,
  withHydraNode,
  withHydraNodeCatchingUp,
  withPreparedHydraNode,
  withUnsyncedHydraNode,
 )
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Conduit qualified as L
import Network.HTTP.Req (
  HttpException (VanillaHttpException),
  JsonResponse,
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  defaultHttpConfig,
  http,
  port,
  req,
  responseBody,
  runReq,
  (/:),
 )
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Hydra.Ledger.Cardano.Fixtures (maxTxExecutionUnits)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genDatum, genKeyPair, genTxOutWithReferenceScript)
import Test.QuickCheck (Positive, elements, generate)

oneOfThreeNodesStopsForAWhile :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
oneOfThreeNodesStopsForAWhile tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob, Carol]
  [(aliceCardanoVk, aliceCardanoSk), (bobCardanoVk, _), (carolCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)
  networkId <- Backend.queryNetworkId backend
  blockTime <- Backend.getBlockTime backend

  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob, Carol] timing
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice, Carol] timing <&> setNetworkId networkId

  carolChainConfig <-
    chainConfigFor Carol workDir backend hydraScriptsTxId [Alice, Bob] timing
      <&> setNetworkId networkId
  withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    aliceUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)
    withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withHydraNode hydraTracer blockTime carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- Init & open head
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) [n1, n2, n3] $ headIsOpenWith (Set.fromList [alice, bob, carol])

        -- Alice deposits something
        depositTx <- requestCommitTx n1 aliceUTxO
        Backend.submitTransaction backend depositTx
        waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
          output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

        -- Perform a simple transaction from alice to herself
        utxo <- getSnapshotUTxO n1
        tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
        send n1 $ input "NewTx" ["transaction" .= tx]

        -- Everyone confirms it (snapshot 2 = after deposit snapshot 1 + this tx)
        waitForAllMatch (200 * blockTime) [n1, n2, n3] $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "snapshot" . key "number" == Just (toJSON (2 :: Integer))

      -- Carol disconnects and the others observe it
      -- waitForAllMatch (100 * blockTime) [n1, n2] $ \v -> do
      --   guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- Alice never-the-less submits a transaction
      utxo <- getSnapshotUTxO n1
      tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
      send n1 $ input "NewTx" ["transaction" .= tx]

      -- Carol reconnects, and then the snapshot can be confirmed
      withHydraNode hydraTracer blockTime carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- Note: We can't use `waitForAlMatch` here as it expects them to
        -- emit the exact same datatype; but Carol will be behind in sequence
        -- numbers as she was offline.
        flip mapConcurrently_ [n1, n2, n3] $ \n ->
          waitMatch (200 * blockTime) n $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $ v ^? key "snapshot" . key "number" == Just (toJSON (3 :: Integer))
            -- Just check that everyone signed it.
            let sigs = v ^.. key "signatures" . key "multiSignature" . values
            guard $ length sigs == 3
 where
  hydraTracer = contramap FromHydraNode tracer

restartedNodeCanObserveCommitTx :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
restartedNodeCanObserveCommitTx tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob]
  [(aliceCardanoVk, _), (bobCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

  networkId <- Backend.queryNetworkId backend
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer blockTime bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] $ \n1 -> do
    headId <- withHydraNode hydraTracer blockTime aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] $ \n2 -> do
      send n1 $ input "Init" []
      -- XXX: might need to tweak the wait time
      waitForAllMatch 10 [n1, n2] $ headIsOpenWith (Set.fromList [alice, bob])

    -- n1 does a deposit while n2 is down
    depositUTxO <- seedFromFaucet backend bobCardanoVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)
    depositTx <- requestCommitTx n1 depositUTxO
    Backend.submitTransaction backend depositTx
    waitMatch 10 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "CommitRecorded"
      guard $ v ^? key "headId" == Just (toJSON headId)
      guard $ v ^? key "utxoToCommit" == Just (toJSON depositUTxO)

    -- n2 is back and does observe the deposit
    withUnsyncedHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] $ \n2 -> do
      waitMatch (10 * blockTime) n2 $ \v -> do
        guard $ v ^? key "tag" == Just "CommitRecorded"
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "utxoToCommit" == Just (toJSON depositUTxO)

resumeFromLatestKnownPoint :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
resumeFromLatestKnownPoint tracer workDir backend hydraScriptsTxId = do
  networkId <- Backend.queryNetworkId backend
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
      <&> setNetworkId networkId

  chainSlot :: ChainSlot <-
    withHydraNodeCatchingUp hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      chainSlot <- waitMatch 20 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "Greetings"
        guard $ v ^? key "headStatus" == Just (toJSON Idle)
        slot <- v ^? key "currentSlot"
        parseMaybe parseJSON slot

      -- wait for some ticks to be observed
      threadDelay 1
      pure chainSlot

  chainSlot' <-
    withHydraNodeCatchingUp hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      waitMatch 20 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "Greetings"
        guard $ v ^? key "headStatus" == Just (toJSON Idle)
        slot <- v ^? key "currentSlot"
        parseMaybe parseJSON slot

  chainSlot `shouldSatisfy` (< chainSlot')
 where
  hydraTracer = contramap FromHydraNode tracer

restartedNodeCanClose :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
restartedNodeCanClose tracer workDir backend hydraScriptsTxId = do
  refuelIfNeeded tracer backend Alice 100_000_000
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> modifyConfig (\config -> config{startChainFrom = Nothing})

  let hydraTracer = contramap FromHydraNode tracer
  headId1 <- withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    send n1 $ input "Init" []
    waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

  withUnsyncedHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Also expect to see past server outputs replayed
    headId2 <- waitMatch (20 * blockTime * 10) n1 $ headIsOpenWith (Set.fromList [alice])
    headId1 `shouldBe` headId2
    -- Heads now open directly, so we close instead of abort
    send n1 $ input "Close" []
    waitMatch 20 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "HeadIsClosed"
      guard $ v ^? key "headId" == Just (toJSON headId2)
  withUnsyncedHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    waitMatch (20 * blockTime * 10) n1 $ \v -> do
      guard $ v ^? key "tag" == Just "Greetings"
      guard $ v ^? key "headStatus" == Just "Closed"
      guard $ v ^? key "me" == Just (toJSON alice)
      guard $ isJust (v ^? key "hydraNodeVersion")

nodeReObservesOnChainTxs :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
nodeReObservesOnChainTxs tracer workDir backend hydraScriptsTxId = do
  refuelIfNeeded tracer backend Alice 100_000_000
  refuelIfNeeded tracer backend Bob 100_000_000
  networkId <- Backend.queryNetworkId backend
  -- Start hydra-node on chain tip
  tip <- Backend.queryTip backend
  blockTime <- Backend.getBlockTime backend

  -- NOTE: Adapt periods to block times
  let timing = Timing{blockTime, contestationPeriod = truncate $ 10 * blockTime, depositPeriod = truncate $ 50 * blockTime}
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
      <&> modifyConfig (\config -> config{startChainFrom = Nothing})

  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
      <&> modifyConfig (\config -> config{startChainFrom = Nothing})

  (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
  commitUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)

  let hydraTracer = contramap FromHydraNode tracer

  withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
    (headId', decrementOuts) <- withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
      send n1 $ input "Init" []

      headId <- waitMatch (20 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice, bob])
      _ <- waitMatch (20 * blockTime) n2 $ headIsOpenWith (Set.fromList [alice, bob])

      resp <-
        parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
          <&> setRequestBodyJSON commitUTxO
            >>= httpJSON

      let depositTransaction = getResponseBody resp :: Tx
      let tx = signTx aliceCardanoSk depositTransaction

      Backend.submitTransaction backend tx

      waitFor hydraTracer (depositTimeout timing) [n1, n2] $
        output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= commitUTxO]

      waitFor hydraTracer (depositTimeout timing) [n1, n2] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx)]

      getSnapshotUTxO n1 `shouldReturn` commitUTxO

      let aliceAddress = mkVkAddress networkId aliceCardanoVk

      decommitTx <- do
        let (i, o) = List.head $ UTxO.toList commitUTxO
        either (failure . show) pure $
          mkSimpleTx (i, o) (aliceAddress, txOutValue o) aliceCardanoSk

      let decommitUTxO = utxoFromTx decommitTx
          decommitTxId = txId decommitTx

      -- Sometimes use websocket, sometimes use HTTP
      join . generate $
        elements
          [ send n1 $ input "Decommit" ["decommitTx" .= decommitTx]
          , postDecommit n1 decommitTx
          ]

      waitFor hydraTracer 10 [n1, n2] $
        output "DecommitRequested" ["headId" .= headId, "decommitTx" .= decommitTx, "utxoToDecommit" .= decommitUTxO]
      waitFor hydraTracer 10 [n1, n2] $
        output "DecommitApproved" ["headId" .= headId, "decommitTxId" .= decommitTxId, "utxoToDecommit" .= decommitUTxO]

      failAfter 10 $ waitForUTxO backend decommitUTxO

      distributedUTxO <- waitForAllMatch 10 [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "DecommitFinalized"
        guard $ v ^? key "headId" == Just (toJSON headId)
        v ^? key "distributedUTxO" . _JSON

      guard $ distributedUTxO `UTxO.containsOutputs` UTxO.txOutputs (utxoFromTx decommitTx)

      pure (headId, decommitUTxO)

    bobChainConfigFromTip <-
      chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
        <&> modifyConfig (\config -> config{startChainFrom = Just tip})

    withTempDir "blank-state" $ \tmpDir -> do
      callProcess "cp" ["-r", workDir </> "state-2", tmpDir]
      callProcess "rm" ["-rf", tmpDir </> "state-2" </> "state*"]
      callProcess "rm" ["-rf", tmpDir </> "state-2" </> "last-known-revision"]
      withUnsyncedHydraNode hydraTracer bobChainConfigFromTip tmpDir 2 bobSk [aliceVk] [1] $ \n2 -> do
        -- Also expect to see past server outputs replayed
        headId2 <- waitMatch (10 * blockTime * 10) n2 $ headIsOpenWith (Set.fromList [alice, bob])
        headId2 `shouldBe` headId'

        distributedUTxO <- waitForAllMatch 5 [n2] $ \v -> do
          guard $ v ^? key "tag" == Just "DecommitFinalized"
          guard $ v ^? key "headId" == Just (toJSON headId2)
          v ^? key "distributedUTxO" . _JSON

        guard $ distributedUTxO `UTxO.containsOutputs` UTxO.txOutputs decrementOuts

        send n1 $ input "Close" []

        deadline' <- waitMatch (20 * blockTime) n2 $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          guard $ v ^? key "headId" == Just (toJSON headId')
          v ^? key "contestationDeadline" . _JSON
        remainingTime <- diffUTCTime deadline' <$> getCurrentTime
        waitFor hydraTracer (remainingTime + 3 * blockTime) [n1, n2] $
          output "ReadyToFanout" ["headId" .= headId']
        send n1 $ input "Fanout" []

        waitForAllMatch (10 * blockTime) [n1, n2] $ headIsFinalizedWith headId' mempty

-- | Step through the full life cycle of a Hydra Head with only a single
-- participant. This scenario is also used by the smoke test run via the
-- `hydra-cluster` executable.
singlePartyHeadFullLifeCycle ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
singlePartyHeadFullLifeCycle tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 55_000_000
    -- Start hydra-node on chain tip
    tip <- Backend.queryTip backend
    blockTime <- Backend.getBlockTime backend
    networkId <- Backend.queryNetworkId backend
    let timing = mkTestTiming blockTime
    let Timing{depositPeriod = timingDepositPeriod} = timing
    contestationPeriod <- CP.fromNominalDiffTime $ 20 * blockTime
    aliceChainConfig <-
      chainConfigFor' Alice workDir backend hydraScriptsTxId [] contestationPeriod timingDepositPeriod
        <&> modifyConfig (\config -> config{startChainFrom = Just tip})
          . setNetworkId networkId

    (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
    let aliceAddress = mkVkAddress networkId aliceCardanoVk

    -- Prepare deposit payload
    (walletVk, walletSk) <- generate genKeyPair
    let depositAmount = 10_000_000
    depositUTxO <- seedFromFaucet backend walletVk (lovelaceToValue depositAmount) (contramap FromFaucet tracer)
    let changeAddress = mkVkAddress @Era networkId walletVk
    let (i, o) = List.head $ UTxO.toList depositUTxO
    let witness = BuildTxWith $ KeyWitness KeyWitnessForSpending
    let blueprint =
          unsafeBuildTransaction $
            defaultTxBodyContent
              & addTxIns [(i, witness)]
              & addTxOut (fromCtxUTxOTxOut o)
    let clientPayload =
          Aeson.object
            [ "blueprintTx" .= blueprint
            , "utxo" .= depositUTxO
            , "changeAddress" .= changeAddress
            ]

    withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Open head
      send n1 $ input "Init" []
      headId <- waitMatch (30 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
      -- Deposit UTxO
      res <-
        runReq defaultHttpConfig $
          req POST (http "127.0.0.1" /: "commit") (ReqBodyJson clientPayload) (Proxy :: Proxy (JsonResponse Tx)) (port $ 4000 + 1)

      let tx = signTx walletSk $ responseBody res
      Backend.submitTransaction backend tx

      waitMatch (depositTimeout timing) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "CommitFinalized"
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "depositTxId" == Just (toJSON $ getTxId (getTxBody tx))
        pure ()

      utxo <- getSnapshotUTxO n1
      l2tx <- mkTransferTx networkId utxo walletSk aliceCardanoVk
      send n1 $ input "NewTx" ["transaction" .= l2tx]
      waitMatch (20 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "snapshot" . key "number" == Just (toJSON (2 :: Integer))

      utxo' <- getSnapshotUTxO n1

      decommitTx <- do
        let (input', output') = List.head $ UTxO.toList utxo'
        either (failure . show) pure $
          mkSimpleTx (input', output') (aliceAddress, txOutValue o) aliceCardanoSk

      send n1 $ input "Decommit" ["decommitTx" .= decommitTx]

      distributedUTxO <- waitForAllMatch (50 * blockTime) [n1] $ \v -> do
        guard $ v ^? key "tag" == Just "DecommitFinalized"
        guard $ v ^? key "headId" == Just (toJSON headId)
        v ^? key "distributedUTxO" . _JSON

      guard $ distributedUTxO `UTxO.containsOutputs` UTxO.txOutputs (utxoFromTx decommitTx)

      -- Close head
      send n1 $ input "Close" []

      deadline <- waitMatch (50 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        v ^? key "contestationDeadline" . _JSON
      -- Wait for fanout
      remainingTime <- diffUTCTime deadline <$> getCurrentTime
      waitFor hydraTracer (remainingTime + 50 * blockTime) [n1] $
        output "ReadyToFanout" ["headId" .= headId]
      send n1 $ input "Fanout" []

      waitForAllMatch (50 * blockTime) [n1] $ headIsFinalizedWith headId mempty
    traceRemainingFunds Alice
 where
  hydraTracer = contramap FromHydraNode tracer

  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    utxo <- Backend.queryUTxOFor backend QueryTip actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, utxo}

-- | Open a Hydra Head with only a single participant but some arbitrary UTxO
-- committed.
singlePartyOpenAHead ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  Maybe (Positive Natural) ->
  -- | Continuation called when the head is open
  (HydraClient -> SigningKey PaymentKey -> HeadId -> IO a) ->
  IO a
singlePartyOpenAHead tracer workDir backend hydraScriptsTxId persistenceRotateAfter callback =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 25_000_000
    -- Start hydra-node on chain tip
    tip <- Backend.queryTip backend
    blockTime <- Backend.getBlockTime backend
    let timing = mkTestTiming blockTime
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
        <&> modifyConfig (\config -> config{startChainFrom = Just tip})

    (walletVk, walletSk) <- generate genKeyPair
    let keyPath = workDir <> "/wallet.sk"
    _ <- writeFileTextEnvelope (File keyPath) Nothing walletSk
    traceWith tracer CreatedKey{keyPath}

    utxoToDeposit <- seedFromFaucet backend walletVk (lovelaceToValue 100_000_000) (contramap FromFaucet tracer)

    let hydraTracer = contramap FromHydraNode tracer
    options <- prepareHydraNode aliceChainConfig workDir 1 aliceSk [] [] id
    let options' = options{persistenceRotateAfter}
    withPreparedHydraNode hydraTracer workDir 1 options' $ \n1 -> do
      void $ waitForNodesSynced (5 * blockTime) [n1]
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
      -- Deposit funds into head
      depositTx <- requestCommitTx n1 utxoToDeposit <&> signTx walletSk
      Backend.submitTransaction backend depositTx
      waitFor hydraTracer (depositTimeout timing) [n1] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

      callback n1 walletSk headId

-- | Single hydra-node where the deposit is done using some wallet UTxO.
canDeposit ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
canDeposit tracer workDir backend hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer backend Alice
        returnFundsToFaucet tracer backend AliceFunds
  )
    $ do
      refuelIfNeeded tracer backend Alice 25_000_000
      blockTime <- Backend.getBlockTime backend
      let timing = mkTestTiming blockTime
      aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
      let hydraNodeId = 1
      let hydraTracer = contramap FromHydraNode tracer
      withHydraNode hydraTracer blockTime aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

        (walletVk, walletSk) <- keysFor AliceFunds
        utxoToDeposit <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)

        res <-
          runReq defaultHttpConfig $
            req
              POST
              (http "127.0.0.1" /: "commit")
              (ReqBodyJson utxoToDeposit)
              (Proxy :: Proxy (JsonResponse (DraftCommitTxResponse Tx)))
              (port $ 4000 + hydraNodeId)

        let DraftCommitTxResponse{commitTx} = responseBody res
        let depositTx = signTx walletSk commitTx
        Backend.submitTransaction backend depositTx

        waitFor hydraTracer (depositTimeout timing) [n1] $
          output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]
        getSnapshotUTxO n1 `shouldReturn` utxoToDeposit

singlePartyUsesScriptOnL2 ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
singlePartyUsesScriptOnL2 tracer workDir backend hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer backend Alice
        returnFundsToFaucet tracer backend AliceFunds
  )
    $ do
      refuelIfNeeded tracer backend Alice 250_000_000
      blockTime <- Backend.getBlockTime backend
      let timing = mkTestTiming blockTime
      aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
      let hydraNodeId = 1
      let hydraTracer = contramap FromHydraNode tracer
      withHydraNode hydraTracer blockTime aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

        (walletVk, walletSk) <- keysFor AliceFunds

        -- Create money on L1
        let commitAmount = 100_000_000
        utxoToDeposit <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)

        -- Deposit it into L2
        depositTx <- requestCommitTx n1 utxoToDeposit <&> signTx walletSk
        Backend.submitTransaction backend depositTx

        -- Check UTxO is present in L2
        waitFor hydraTracer (depositTimeout timing) [n1] $
          output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

        pparams <- getProtocolParameters n1
        networkId <- Backend.queryNetworkId backend

        -- Send the UTxO to a script; in preparation for running the script
        let serializedScript = dummyValidatorScript
        let scriptAddress = mkScriptAddress networkId serializedScript
        let scriptOutput =
              mkTxOutAutoBalance
                pparams
                scriptAddress
                (lovelaceToValue 0)
                (mkTxOutDatumHash ())
                ReferenceScriptNone

        systemStart <- Backend.querySystemStart backend QueryTip
        eraHistory <- Backend.queryEraHistory backend QueryTip
        stakePools <- Backend.queryStakePools backend QueryTip
        case buildTransactionWithPParams' pparams systemStart eraHistory stakePools (mkVkAddress networkId walletVk) utxoToDeposit [] [scriptOutput] Nothing of
          Left e -> error $ show e
          Right tx -> do
            let signedL2tx = signTx walletSk tx
            send n1 $ input "NewTx" ["transaction" .= signedL2tx]

            waitMatch (10 * blockTime) n1 $ \v -> do
              guard $ v ^? key "tag" == Just "SnapshotConfirmed"
              guard $
                toJSON signedL2tx
                  `elem` (v ^.. key "snapshot" . key "confirmed" . values)

            -- Now, spend the money from the script
            let scriptWitness =
                  BuildTxWith $
                    ScriptWitness scriptWitnessInCtx $
                      PlutusScriptWitness
                        serializedScript
                        (mkScriptDatum ())
                        (toScriptData ())
                        maxTxExecutionUnits

            let txIn = mkTxIn signedL2tx 0
            let remainder = mkTxIn signedL2tx 1

            let outAmt = foldMap txOutValue (txOuts' tx)
            let body =
                  defaultTxBodyContent
                    & addTxIns [(txIn, scriptWitness), (remainder, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
                    & addTxInsCollateral [remainder]
                    & addTxOuts [TxOut (mkVkAddress networkId walletVk) outAmt TxOutDatumNone ReferenceScriptNone]
                    & setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

            -- TODO: Instead of using `createAndValidateTransactionBody`, we
            -- should be able to just construct the Tx with autobalancing via
            -- `buildTransactionWithBody`. Unfortunately this is broken in the
            -- version of cardano-api that we presently use; in a future upgrade
            -- of that library we can try again.
            -- tx' <- either (failure . show) pure =<< buildTransactionWithBody networkId nodeSocket (mkVkAddress networkId walletVk) body utxoToDeposit
            txBody <- either (failure . show) pure (createAndValidateTransactionBody body)

            let spendTx' = makeSignedTransaction [] txBody
                spendTx = fromLedgerTx $ recomputeIntegrityHash pparams [PlutusV3] (toLedgerTx spendTx')
            let signedTx = signTx walletSk spendTx

            send n1 $ input "NewTx" ["transaction" .= signedTx]

            waitMatch (10 * blockTime) n1 $ \v -> do
              guard $ v ^? key "tag" == Just "SnapshotConfirmed"
              guard $
                toJSON signedTx
                  `elem` (v ^.. key "snapshot" . key "confirmed" . values)

            -- And check that we can close and fanout the head successfully
            send n1 $ input "Close" []
            deadline <- waitMatch (10 * blockTime) n1 $ \v -> do
              guard $ v ^? key "tag" == Just "HeadIsClosed"
              v ^? key "contestationDeadline" . _JSON
            remainingTime <- diffUTCTime deadline <$> getCurrentTime
            waitFor hydraTracer (remainingTime + 10 * blockTime) [n1] $
              output "ReadyToFanout" ["headId" .= headId]
            send n1 $ input "Fanout" []
            waitMatch (10 * blockTime) n1 $ \v ->
              guard $ v ^? key "tag" == Just "HeadIsFinalized"

            -- Assert final wallet balance
            (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
              `shouldReturn` lovelaceToValue commitAmount

-- | Open a head and run a script using 'Rewarding' script purpose and a zero
-- lovelace withdrawal.
singlePartyUsesWithdrawZeroTrick :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
singlePartyUsesWithdrawZeroTrick tracer workDir backend hydraScriptsTxId =
  -- Seed/return fuel
  bracket_ (refuelIfNeeded tracer backend Alice 250_000_000) (returnFundsToFaucet tracer backend Alice) $ do
    -- Seed/return funds
    (walletVk, walletSk) <- keysFor AliceFunds
    bracket
      (seedFromFaucet backend walletVk (lovelaceToValue 100_000_000) (contramap FromFaucet tracer))
      (\_ -> returnFundsToFaucet tracer backend AliceFunds)
      $ \utxoToDeposit -> do
        -- Start hydra-node and open a head
        blockTime <- Backend.getBlockTime backend
        let timing = mkTestTiming blockTime
        aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
        let hydraNodeId = 1
        let hydraTracer = contramap FromHydraNode tracer
        networkId <- Backend.queryNetworkId backend
        withHydraNode hydraTracer blockTime aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
          send n1 $ input "Init" []
          headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
          -- Deposit funds into head
          depositTx <- requestCommitTx n1 utxoToDeposit <&> signTx walletSk
          Backend.submitTransaction backend depositTx
          waitFor hydraTracer (depositTimeout timing) [n1] $
            output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

          -- Prepare a tx that re-spends everything owned by walletVk
          pparams <- getProtocolParameters n1
          let change = mkVkAddress networkId walletVk
          Right tx <- buildTransactionWithPParams pparams backend change utxoToDeposit [] [] Nothing

          -- Modify the tx to run a script via the withdraw 0 trick
          let redeemer = toLedgerData $ toScriptData ()
              exUnits = toLedgerExUnits maxTxExecutionUnits
              rewardAccount = RewardAccount Testnet (ScriptHashObj scriptHash)
              scriptHash = hashScript script
              script = toLedgerScript @_ @Era dummyRewardingScript
          let tx' =
                fromLedgerTx $
                  recomputeIntegrityHash pparams [PlutusV3] $
                    toLedgerTx tx
                      & bodyTxL . collateralInputsTxBodyL .~ Set.map toLedgerTxIn (UTxO.inputSet utxoToDeposit)
                      & bodyTxL . totalCollateralTxBodyL .~ SJust (UTxO.totalLovelace utxoToDeposit)
                      & bodyTxL . withdrawalsTxBodyL .~ Withdrawals (Map.singleton rewardAccount 0)
                      & witsTxL . rdmrsTxWitsL .~ Redeemers (Map.singleton (ConwayRewarding $ AsIx 0) (redeemer, exUnits))
                      & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash script

          let signedL2tx = signTx walletSk tx'
          send n1 $ input "NewTx" ["transaction" .= signedL2tx]

          waitMatch (10 * blockTime) n1 $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $
              toJSON signedL2tx
                `elem` (v ^.. key "snapshot" . key "confirmed" . values)

-- | Compute the integrity hash of a transaction using a list of plutus languages.
recomputeIntegrityHash ::
  (AlonzoEraPParams ppera, AlonzoEraTxWits txera, AlonzoEraTxBody txera, EraTx txera) =>
  PParams ppera ->
  [Language] ->
  Ledger.Tx txera ->
  Ledger.Tx txera
recomputeIntegrityHash pp languages tx = do
  tx & bodyTxL . scriptIntegrityHashTxBodyL .~ integrityHash
 where
  integrityHash =
    hashScriptIntegrity
      (Set.fromList $ getLanguageView pp <$> languages)
      (tx ^. witsTxL . rdmrsTxWitsL)
      (tx ^. witsTxL . datsTxWitsL)

canDepositScriptBlueprint ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
canDepositScriptBlueprint tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 20_000_000
    blockTime <- Backend.getBlockTime backend
    let timing = mkTestTiming blockTime
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer blockTime aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
      (clientPayload, blueprint) <- prepareScriptPayload 7_000_000

      commitTx <-
        fmap responseBody $
          runReq defaultHttpConfig $
            req POST (http "127.0.0.1" /: "commit") (ReqBodyJson clientPayload) (Proxy :: Proxy (JsonResponse Tx)) (port $ 4000 + hydraNodeId)
      Backend.submitTransaction backend commitTx

      -- The deposited UTxO in the head is keyed by the blueprint tx's outputs.
      let expectedDeposit = constructDepositUTxO (getTxId (getTxBody blueprint)) (txOuts' blueprint)
      waitFor hydraTracer (depositTimeout timing) [n1] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody commitTx)]
      getSnapshotUTxO n1 `shouldReturn` expectedDeposit
 where
  prepareScriptPayload lovelaceAmt = do
    networkId <- Backend.queryNetworkId backend
    let scriptAddress = mkScriptAddress networkId dummyValidatorScript
    let datumHash :: TxOutDatum ctx
        datumHash = mkTxOutDatumHash ()
    (scriptIn, scriptOut) <- createOutputAtAddress networkId backend scriptAddress datumHash (lovelaceToValue lovelaceAmt)
    let scriptUTxO = UTxO.singleton scriptIn scriptOut
    let scriptWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness dummyValidatorScript (mkScriptDatum ()) (toScriptData ())
    let blueprint =
          unsafeBuildTransaction $
            defaultTxBodyContent
              & addTxIns [(scriptIn, scriptWitness)]
              & addTxOut (fromCtxUTxOTxOut scriptOut)
    pure
      ( Aeson.object ["blueprintTx" .= blueprint, "utxo" .= scriptUTxO]
      , blueprint
      )

persistenceCanLoadWithNothingCommitted ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
persistenceCanLoadWithNothingCommitted tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 20_000_000
    blockTime <- Backend.getBlockTime backend
    let timing = mkTestTiming blockTime
    aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    headId <- withHydraNode hydraTracer blockTime aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
    let persistenceState = workDir </> "state-" <> show hydraNodeId </> "state"
    stateContents <- readFileBS persistenceState
    let headOpened = BSC.pack $ List.last (List.lines $ BSC.unpack stateContents)
    case headOpened ^? key "stateChanged" . key "tag" . _String of
      Nothing -> error "Failed to find HeadIsOpened in the state file"
      Just headIsOpen -> do
        headIsOpen `shouldBe` "HeadOpened"
        withUnsyncedHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
          headId' <- waitMatch (20 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
          headId' `shouldBe` headId

          getSnapshotUTxO n1 `shouldReturn` mempty

-- | Initialize open and close a head on a real network and ensure contestation
-- period longer than the time horizon is possible. For this it is enough that
-- we can close a head and not wait for the deadline.
canCloseWithLongContestationPeriod ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
canCloseWithLongContestationPeriod tracer workDir backend hydraScriptsTxId = do
  refuelIfNeeded tracer backend Alice 100_000_000
  -- Start hydra-node on chain tip
  tip <- Backend.queryTip backend
  blockTime <- Backend.getBlockTime backend
  let oneWeek = 60 * 60 * 24 * 7
      Timing{depositPeriod = defaultDepositPeriod} = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor' Alice workDir backend hydraScriptsTxId [] oneWeek defaultDepositPeriod
      <&> modifyConfig (\config -> config{startChainFrom = Just tip})
  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Initialize & open head
    send n1 $ input "Init" []
    _headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])
    -- Close head
    send n1 $ input "Close" []
    void $
      waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
  traceRemainingFunds Alice
 where
  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    utxo <- Backend.queryUTxOFor backend QueryTip actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, utxo}

canSubmitTransactionThroughAPI ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
canSubmitTransactionThroughAPI tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 25_000_000
    blockTime <- Backend.getBlockTime backend
    let timing = mkTestTiming blockTime
    aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer blockTime aliceChainConfig workDir hydraNodeId aliceSk [] [hydraNodeId] $ \_ -> do
      -- let's prepare a _user_ transaction from Bob to Carol
      (cardanoBobVk, cardanoBobSk) <- keysFor Bob
      (cardanoCarolVk, _) <- keysFor Carol
      networkId <- Backend.queryNetworkId backend
      -- create output for Bob to be sent to carol
      bobUTxO <- seedFromFaucet backend cardanoBobVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)
      let carolsAddress = mkVkAddress networkId cardanoCarolVk
          bobsAddress = mkVkAddress networkId cardanoBobVk
          carolsOutput =
            TxOut
              carolsAddress
              (lovelaceToValue $ Coin 2_000_000)
              TxOutDatumNone
              ReferenceScriptNone
      -- prepare fully balanced tx body
      buildTransaction backend bobsAddress bobUTxO (fst <$> UTxO.toList bobUTxO) [carolsOutput] >>= \case
        Left e -> failure $ show e
        Right tx -> do
          let unsignedTx = makeSignedTransaction [] $ getTxBody tx
          let unsignedRequest = toJSON unsignedTx
          sendRequest hydraNodeId unsignedRequest
            `shouldThrow` expectErrorStatus 400 (Just "MissingVKeyWitnessesUTXOW")

          let signedTx = signTx cardanoBobSk unsignedTx
          let signedRequest = toJSON signedTx
          (sendRequest hydraNodeId signedRequest <&> responseBody)
            `shouldReturn` TransactionSubmitted
 where
  sendRequest :: (MonadIO m, ToJSON tx) => Int -> tx -> m (JsonResponse TransactionSubmitted)
  sendRequest hydraNodeId tx =
    runReq defaultHttpConfig $
      req
        POST
        (http "127.0.0.1" /: "cardano-transaction")
        (ReqBodyJson tx)
        (Proxy :: Proxy (JsonResponse TransactionSubmitted))
        (port $ 4000 + hydraNodeId)

-- | Three hydra nodes open a head and we assert that none of them sees errors.
-- This was particularly misleading when everyone tries to post the collect
-- transaction concurrently.
threeNodesNoErrorsOnOpen :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
threeNodesNoErrorsOnOpen tracer tmpDir backend hydraScriptsTxId = do
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
  bobKeys@(bobCardanoVk, _) <- generate genKeyPair
  carolKeys@(carolCardanoVk, _) <- generate genKeyPair

  let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
      hydraKeys = [aliceSk, bobSk, carolSk]

  let contestationPeriod = 2
  let hydraTracer = contramap FromHydraNode tracer
  let nodeSocket' =
        case Backend.getOptions backend of
          Direct DirectOptions{nodeSocket} -> nodeSocket
          Blockfrost _ -> error "Unexpected Blockfrost options"
  blockTime <- Backend.getBlockTime backend
  let timing = Timing{blockTime, contestationPeriod, depositPeriod = truncate $ 3 * blockTime}
  withHydraCluster hydraTracer timing tmpDir nodeSocket' 1 cardanoKeys hydraKeys hydraScriptsTxId $ \clients -> do
    let leader = head clients
    waitForNodesConnected hydraTracer 20 clients

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send leader $ input "Init" []
    mapConcurrently_ shouldNotReceivePostTxError clients
 where
  --  Fail if a 'PostTxOnChainFailed' message is received.
  shouldNotReceivePostTxError client@HydraClient{hydraNodeId} = do
    blockTime <- Backend.getBlockTime backend
    err <- waitMatch (20 * blockTime) client $ \v -> do
      case v ^? key "tag" of
        Just "PostTxOnChainFailed" -> pure $ Left $ v ^? key "postTxError"
        Just "HeadIsOpen" -> pure $ Right ()
        _ -> Nothing
    case err of
      Left receivedError ->
        failure $ "node " <> show hydraNodeId <> " should not receive error: " <> show receivedError
      Right _headIsOpen ->
        pure ()

-- | Hydra nodes ABC run on ABC cluster and connect to each other.
-- Hydra nodes BC shut down.
-- Hydra nodes BC run on BC cluster and connect to each other.
-- Hydra nodes BC shut down.
-- Hydra nodes BC run and connect ABC cluster again.
nodeCanSupportMultipleEtcdClusters :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
nodeCanSupportMultipleEtcdClusters tracer workDir backend hydraScriptsTxId = do
  networkId <- Backend.queryNetworkId backend
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob, Carol] timing
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice, Carol] timing
      <&> setNetworkId networkId
  carolChainConfig <-
    chainConfigFor Carol workDir backend hydraScriptsTxId [Alice, Bob] timing
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer

  withUnsyncedHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    withUnsyncedHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withUnsyncedHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        waitForNodesConnected hydraTracer 30 $ n1 :| [n2, n3]

    bobChainConfig' <-
      chainConfigFor Bob workDir backend hydraScriptsTxId [Carol] timing
        <&> setNetworkId networkId
    carolChainConfig' <-
      chainConfigFor Carol workDir backend hydraScriptsTxId [Bob] timing
        <&> setNetworkId networkId

    waitForNodesDisconnected hydraTracer 60 $ n1 :| []

    withUnsyncedHydraNode hydraTracer bobChainConfig' workDir 2 bobSk [carolVk] [2, 3] $ \n2 -> do
      withUnsyncedHydraNode hydraTracer carolChainConfig' workDir 3 carolSk [bobVk] [2, 3] $ \n3 -> do
        waitForNodesConnected hydraTracer 30 $ n2 :| [n3]

    withUnsyncedHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withUnsyncedHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        waitForNodesConnected hydraTracer 30 $ n1 :| [n2, n3]

-- | Two hydra node setup where Alice is wrongly configured to use Carol's
-- cardano keys instead of Bob's which will prevent him to be notified the
-- `HeadIsInitializing` but he should still receive some notification.
initWithWrongKeys :: ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> backend -> [TxId] -> IO ()
initWithWrongKeys workDir tracer backend hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice
  (carolCardanoVk, _) <- keysFor Carol

  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [Carol] timing
  bobChainConfig <- chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer blockTime aliceChainConfig workDir 3 aliceSk [bobVk] [3, 4] $ \n1 -> do
    withHydraNode hydraTracer blockTime bobChainConfig workDir 4 bobSk [aliceVk] [3, 4] $ \n2 -> do
      seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
      send n1 $ input "Init" []
      headId <-
        waitForAllMatch 10 [n1] $
          headIsOpenWith (Set.fromList [alice, bob])

      let expectedParticipants =
            verificationKeyToOnChainId
              <$> [aliceCardanoVk, carolCardanoVk]

      -- We want the client to observe headId being opened without bob (node 2)
      -- being part of it
      participants <- waitMatch (10 * blockTime) n2 $ \v -> do
        guard $ v ^? key "tag" == Just (Aeson.String "IgnoredHeadInitializing")
        guard $ v ^? key "headId" == Just (toJSON headId)
        v ^? key "participants" . _JSON

      participants `shouldMatchList` expectedParticipants

startWithWrongPeers :: ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> backend -> [TxId] -> IO ()
startWithWrongPeers workDir tracer backend hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice

  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [Carol] timing
  bobChainConfig <- chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer blockTime aliceChainConfig workDir 3 aliceSk [bobVk] [3, 4] $ \n1 -> do
    -- NOTE: here we deliberately use the wrong peer list for Bob
    withHydraNode hydraTracer blockTime bobChainConfig workDir 4 bobSk [aliceVk] [4] $ \_ -> do
      seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

      (clusterPeers, configuredPeers) <- waitMatch (20 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just (Aeson.String "NetworkClusterIDMismatch")
        clusterPeers <- v ^? key "clusterPeers" . _String
        configuredPeers <- v ^? key "misconfiguredPeers" . _String
        pure (clusterPeers, configuredPeers)

      when (clusterPeers == configuredPeers) $
        failure "Expected clusterPeers and configuredPeers to be different"
      clusterPeers `shouldBe` "0.0.0.0:5003=http://0.0.0.0:5003,0.0.0.0:5004=http://0.0.0.0:5004"
      configuredPeers `shouldBe` "0.0.0.0:5004=http://0.0.0.0:5004"

-- | Open a a two participant head, deposit funds to it and distribute them on fanout.
canDepositConcurrently :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canDepositConcurrently tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      blockTime <- Backend.getBlockTime backend
      -- 2 concurrent deposits = 2 sequential increments on-chain
      let timing = mkTestTiming' 2 blockTime
      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
          <&> setNetworkId networkId
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
          <&> setNetworkId networkId
      withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitForAllMatch (10 * blockTime) [n1, n2] $ headIsOpenWith (Set.fromList [alice, bob])

          (walletVk, walletSk) <- generate genKeyPair
          -- Seed both UTxOs before drafting deposits so L1 confirmations don't
          -- eat into the deposit deadline window.
          commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)
          commitUTxO2 <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)

          -- Draft and submit both deposits concurrently.
          (tx, tx2) <-
            concurrently
              ( do
                  resp <-
                    parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
                      <&> setRequestBodyJSON commitUTxO >>= httpJSON
                  let signed = signTx walletSk (getResponseBody resp :: Tx)
                  Backend.submitTransaction backend signed
                  pure signed
              )
              ( do
                  resp <-
                    parseUrlThrow ("POST " <> hydraNodeBaseUrl n2 <> "/commit")
                      <&> setRequestBodyJSON commitUTxO2 >>= httpJSON
                  let signed = signTx walletSk (getResponseBody resp :: Tx)
                  Backend.submitTransaction backend signed
                  pure signed
              )

          -- Wait for both CommitFinalized events in any order, then assert all
          -- expected tx ids were seen (benchmark pattern).
          let expectedTxIds = Set.fromList [getTxId (getTxBody tx), getTxId (getTxBody tx2)]
          seenTxIds <- fmap Set.fromList $
            replicateM 2 $
              waitForAllMatch (depositTimeout timing) [n1, n2] $ \v -> do
                guard $ v ^? key "tag" == Just "CommitFinalized"
                guard $ v ^? key "headId" == Just (toJSON headId)
                v ^? key "depositTxId" >>= parseMaybe parseJSON
          seenTxIds `shouldBe` expectedTxIds

          getSnapshotUTxO n1 `shouldReturn` commitUTxO <> commitUTxO2

          send n2 $ input "Close" []
          deadline <- waitMatch (10 * blockTime) n2 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            v ^? key "contestationDeadline" . _JSON
          remainingTime <- diffUTCTime deadline <$> getCurrentTime
          waitFor hydraTracer (remainingTime + 3 * blockTime) [n1, n2] $
            output "ReadyToFanout" ["headId" .= headId]
          send n2 $ input "Fanout" []
          waitMatch (10 * blockTime) n2 $ \v ->
            guard $ v ^? key "tag" == Just "HeadIsFinalized"

          (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
            `shouldReturn` balance (commitUTxO <> commitUTxO2)
 where
  hydraTracer = contramap FromHydraNode tracer

rejectDeposit :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
rejectDeposit tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 30_000_000
    -- NOTE: Adapt periods to block times
    blockTime <- Backend.getBlockTime backend
    let timing = Timing{blockTime, contestationPeriod = truncate $ 10 * blockTime, depositPeriod = truncate $ 100 * blockTime}
    networkId <- Backend.queryNetworkId backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
        <&> setNetworkId networkId

    let pparamsDecorator = atKey "utxoCostPerByte" ?~ toJSON (Aeson.Number 4310)
    optionsWithUTxOCostPerByte <- prepareHydraNode aliceChainConfig workDir 1 aliceSk [] [] pparamsDecorator
    withPreparedHydraNode hydraTracer workDir 1 optionsWithUTxOCostPerByte $ \n1 -> do
      void $ waitForNodesSynced (10 * blockTime) [n1]
      send n1 $ input "Init" []
      _headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      (walletVk, _) <- generate genKeyPair
      commitUTxO' <- seedFromFaucet backend walletVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)
      TxOut _ _ _ refScript <- generate genTxOutWithReferenceScript
      datum <- generate genDatum
      let commitUTxO :: UTxO =
            UTxO.fromList $
              (\(i, TxOut addr _ _ _) -> (i, TxOut addr (lovelaceToValue 0) datum refScript))
                <$> UTxO.toList commitUTxO'
      response <-
        L.parseRequest ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
          <&> setRequestBodyJSON (commitUTxO :: UTxO)
            >>= httpJSON

      let expectedError = getResponseBody response :: PostTxError Tx

      expectedError `shouldSatisfy` \case
        DepositTooLow{minimumValue, providedValue} -> providedValue < minimumValue
        _ -> False
 where
  hydraTracer = contramap FromHydraNode tracer

-- | Open a single participant head and deposit part of a UTxO, with the
-- remainder returned to a change address. This exercises the 'changeAddress'
-- balancing path in the deposit blueprint API.
canDepositPartially :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canDepositPartially tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 30_000_000
    blockTime <- Backend.getBlockTime backend
    let timing = mkTestTiming blockTime
    networkId <- Backend.queryNetworkId backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      (walletVk, walletSk) <- generate genKeyPair
      let seedAmount = 20_000_000
      let commitAmount = 10_000_000
      commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue seedAmount) (contramap FromFaucet tracer)

      let changeAddress = mkVkAddress @Era networkId walletVk
      let (i, o') = List.head $ UTxO.toList commitUTxO
      let o = modifyTxOutValue (const $ lovelaceToValue commitAmount) o'
      let witness = BuildTxWith $ KeyWitness KeyWitnessForSpending
      let blueprint =
            unsafeBuildTransaction $
              defaultTxBodyContent
                & addTxIns [(i, witness)]
                & addTxOut (fromCtxUTxOTxOut o)
      let (CAPI.TxIn _ index) = i
      let depositedUTxO = UTxO.singleton (CAPI.TxIn (getTxId $ getTxBody blueprint) index) o
      let clientPayload =
            Aeson.object
              [ "blueprintTx" .= blueprint
              , "utxo" .= commitUTxO
              , "changeAddress" .= changeAddress
              ]

      res <-
        runReq defaultHttpConfig $
          req POST (http "127.0.0.1" /: "commit") (ReqBodyJson clientPayload) (Proxy :: Proxy (JsonResponse Tx)) (port $ 4000 + 1)

      let tx = signTx walletSk $ responseBody res
      Backend.submitTransaction backend tx

      waitFor hydraTracer (depositTimeout timing) [n1] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx)]

      getSnapshotUTxO n1 `shouldReturn` depositedUTxO
      (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
        `shouldReturn` lovelaceToValue (seedAmount - commitAmount)

-- | Open a a single participant head, deposit and then recover it.
canRecoverDeposit :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canRecoverDeposit tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      -- NOTE: Directly expire deposits
      blockTime <- Backend.getBlockTime backend
      -- Use short periods so deposits expire quickly
      let timing = Timing{blockTime, contestationPeriod = 1, depositPeriod = 1}
      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
          <&> setNetworkId networkId
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
          <&> setNetworkId networkId
      withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        headId <- withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          waitForAllMatch (10 * blockTime) [n1, n2] $ headIsOpenWith (Set.fromList [alice, bob])
        -- stop the second node here

        -- Get some L1 funds
        (walletVk, walletSk) <- generate genKeyPair
        let commitAmount = 5_000_000
        commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)

        (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
          `shouldReturn` lovelaceToValue commitAmount

        depositTransaction <-
          parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
            <&> setRequestBodyJSON commitUTxO
              >>= httpJSON
            <&> getResponseBody

        let tx = signTx walletSk depositTransaction
        Backend.submitTransaction backend tx

        deadline <- waitForAllMatch 10 [n1] $ \v -> do
          guard $ v ^? key "tag" == Just "CommitRecorded"
          v ^? key "deadline" >>= parseMaybe parseJSON

        (selectLovelace . balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
          `shouldReturn` 0

        -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
        diff <- realToFrac . diffUTCTime deadline <$> getCurrentTime
        threadDelay $ diff + 1

        (`shouldReturn` "OK") $
          parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n1 <> "/commits/" <> show (txId tx))
            >>= httpJSON
            <&> getResponseBody @String

        waitForAllMatch 20 [n1] $ \v -> do
          guard $ v ^? key "tag" == Just "CommitRecovered"
          guard $ v ^? key "recoveredUTxO" == Just (toJSON commitUTxO)

        (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
          `shouldReturn` lovelaceToValue commitAmount
        send n1 $ input "Close" []

        deadline' <- waitMatch (10 * blockTime) n1 $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          v ^? key "contestationDeadline" . _JSON

        remainingTime <- diffUTCTime deadline' <$> getCurrentTime
        waitFor hydraTracer (remainingTime + 3 * blockTime) [n1] $
          output "ReadyToFanout" ["headId" .= headId]
        send n1 $ input "Fanout" []
        waitMatch (20 * blockTime) n1 $ \v ->
          guard $ v ^? key "tag" == Just "HeadIsFinalized"

        -- Assert final wallet balance
        (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
          `shouldReturn` balance commitUTxO
 where
  hydraTracer = contramap FromHydraNode tracer

-- | Open a single-participant head, perform 3 deposits, and then:
-- 1. Close the head and recover deposit #1
-- 2. Fanout the head and recover deposit #2
-- 3. Open a new head and recover deposit #3
canRecoverDepositInAnyState :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canRecoverDepositInAnyState tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 30_000_000
    -- NOTE: Directly expire deposits
    blockTime <- Backend.getBlockTime backend
    -- Use short periods so deposits expire quickly
    let timing = Timing{blockTime, contestationPeriod = 2, depositPeriod = 1}
    networkId <- Backend.queryNetworkId backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Init the head
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      -- Get some L1 funds
      (walletVk, walletSk) <- generate genKeyPair
      let commitAmount = 5_000_000
      commitUTxO1 <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)
      commitUTxO2 <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)
      commitUTxO3 <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)

      queryWalletBalance walletVk `shouldReturn` lovelaceToValue (commitAmount * 3)

      -- Increment commit #1
      depositReceipt1 <- increment n1 walletSk commitUTxO1
      queryWalletBalance walletVk `shouldReturn` lovelaceToValue (commitAmount * 2)

      -- Increment commit #2
      depositReceipt2 <- increment n1 walletSk commitUTxO2
      queryWalletBalance walletVk `shouldReturn` lovelaceToValue commitAmount

      -- Increment commit #3
      depositReceipt3 <- increment n1 walletSk commitUTxO3
      selectLovelace <$> queryWalletBalance walletVk `shouldReturn` 0

      -- 1. Close the head
      send n1 $ input "Close" []

      contestationDeadline <- waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        v ^? key "contestationDeadline" . _JSON

      -- Recover deposit #1
      recover n1 depositReceipt1 commitUTxO1
      queryWalletBalance walletVk `shouldReturn` balance commitUTxO1

      -- 2. Fanout the head
      remainingTime <- diffUTCTime contestationDeadline <$> getCurrentTime
      waitFor hydraTracer (remainingTime + 3 * blockTime) [n1] $
        output "ReadyToFanout" ["headId" .= headId]
      send n1 $ input "Fanout" []
      waitMatch (20 * blockTime) n1 $ \v ->
        guard $ v ^? key "tag" == Just "HeadIsFinalized"

      -- Recover deposit #2
      recover n1 depositReceipt2 commitUTxO2
      queryWalletBalance walletVk `shouldReturn` balance (commitUTxO1 <> commitUTxO2)

      -- 3. Open a new head
      send n1 $ input "Init" []
      _headId2 <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      -- Recover deposit #3
      recover n1 depositReceipt3 commitUTxO3
      queryWalletBalance walletVk `shouldReturn` balance (commitUTxO1 <> commitUTxO2 <> commitUTxO3)
 where
  hydraTracer = contramap FromHydraNode tracer

  queryWalletBalance walletVk =
    balance <$> Backend.queryUTxOFor backend QueryTip walletVk

  increment :: HydraClient -> SigningKey PaymentKey -> UTxO -> IO (TxId, UTCTime)
  increment n walletSk commitUTxO = do
    depositTransaction <-
      parseUrlThrow ("POST " <> hydraNodeBaseUrl n <> "/commit")
        <&> setRequestBodyJSON commitUTxO
          >>= httpJSON
        <&> getResponseBody

    let tx = signTx walletSk depositTransaction
    Backend.submitTransaction backend tx
    blockTime <- Backend.getBlockTime backend

    deadline <- waitMatch (10 * blockTime) n $ \v -> do
      guard $ v ^? key "tag" == Just "CommitRecorded"
      v ^? key "deadline" >>= parseMaybe parseJSON

    pure (getTxId $ getTxBody tx, deadline)

  recover :: HydraClient -> (TxId, UTCTime) -> UTxO -> IO ()
  recover n (depositId, deadline) commitUTxO = do
    -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
    diff <- realToFrac . diffUTCTime deadline <$> getCurrentTime
    threadDelay $ diff + 1

    (`shouldReturn` "OK") $
      parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n <> "/commits/" <> show depositId)
        >>= httpJSON
        <&> getResponseBody @String
    blockTime <- Backend.getBlockTime backend

    waitMatch (20 * blockTime) n $ \v -> do
      guard $ v ^? key "tag" == Just "CommitRecovered"
      guard $ v ^? key "recoveredUTxO" == Just (toJSON commitUTxO)

-- | Open a two-participant head, stop one node so deposits stay pending, then
-- verify that GET /commits lists them. Recovery is covered by canRecoverDeposit.
canSeePendingDeposits :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canSeePendingDeposits tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      blockTime <- Backend.getBlockTime backend
      let timing = mkTestTiming blockTime
      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
          <&> setNetworkId networkId
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
          <&> setNetworkId networkId
      withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        _ <- withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          _ <- waitForAllMatch (10 * blockTime) [n1, n2] $ headIsOpenWith (Set.fromList [alice, bob])
          -- Stop Bob here so deposits stay pending (can't reach CommitFinalized without both nodes).
          pure ()

        (walletVk, walletSk) <- generate genKeyPair
        commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)
        commitUTxO2 <- seedFromFaucet backend walletVk (lovelaceToValue 4_000_000) (contramap FromFaucet tracer)

        -- Submit two deposits and check each appears in GET /commits immediately.
        forM_ [commitUTxO, commitUTxO2] $ \utxo -> do
          depositTransaction <-
            parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
              <&> setRequestBodyJSON utxo
                >>= httpJSON
              <&> getResponseBody

          let tx = signTx walletSk depositTransaction
          let depositTxId = getTxId (getTxBody tx)
          liftIO $ Backend.submitTransaction backend tx

          liftIO $ waitForAllMatch 10 [n1] $ \v ->
            guard $ v ^? key "tag" == Just "CommitRecorded"

          pendingDeposits <-
            parseUrlThrow ("GET " <> hydraNodeBaseUrl n1 <> "/commits")
              >>= httpJSON
              <&> getResponseBody

          liftIO $ pendingDeposits `shouldContain` [depositTxId]
          pure depositTxId

        forM_ deposited $ \deposit -> do
          -- XXX: should know the deadline from the query above
          -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
          threadDelay $ realToFrac (toNominalDiffTime depositPeriod * 4)

          (`shouldReturn` "OK") $
            parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n1 <> "/commits/" <> show deposit)
              >>= httpJSON
              <&> getResponseBody @String

          waitForAllMatch (toNominalDiffTime depositPeriod) [n1] $ \v -> do
            guard $ v ^? key "tag" == Just "CommitRecovered"

        pendingDeposits :: [TxId] <-
          parseUrlThrow ("GET " <> hydraNodeBaseUrl n1 <> "/commits")
            >>= httpJSON
            <&> getResponseBody

        pendingDeposits `shouldBe` []
 where
  hydraTracer = contramap FromHydraNode tracer

-- | Open a a single participant head with some UTxO and incrementally decommit it.
canDecommit :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canDecommit tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 30_000_000
    blockTime <- Backend.getBlockTime backend
    let timing = mkTestTiming blockTime
    networkId <- Backend.queryNetworkId backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
        <&> setNetworkId networkId
    withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

      (walletVk, walletSk) <- generate genKeyPair
      let headAmount = 8_000_000
      let commitAmount = 5_000_000
      headUTxO <- seedFromFaucet backend walletVk (lovelaceToValue headAmount) (contramap FromFaucet tracer)
      commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)

      deposit <- requestCommitTx n1 (headUTxO <> commitUTxO) <&> signTx walletSk
      Backend.submitTransaction backend deposit
      waitFor hydraTracer (depositTimeout timing) [n1] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId deposit]

      -- Decommit the single commitUTxO by creating a fully "respending" decommit transaction
      let walletAddress = mkVkAddress networkId walletVk
      decommitTx <- do
        let (i, o) = List.head $ UTxO.toList commitUTxO
        either (failure . show) pure $
          mkSimpleTx (i, o) (walletAddress, txOutValue o) walletSk

      expectFailureOnUnsignedDecommitTx n1 headId decommitTx blockTime
      expectSuccessOnSignedDecommitTx n1 headId decommitTx

      -- After decommit Head UTxO should not contain decommitted outputs and wallet owns the funds on L1
      getSnapshotUTxO n1 `shouldReturn` headUTxO
      (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
        `shouldReturn` lovelaceToValue commitAmount

      -- Close and Fanout whatever is left in the Head back to L1
      send n1 $ input "Close" []
      deadline <- waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        v ^? key "contestationDeadline" . _JSON
      remainingTime <- diffUTCTime deadline <$> getCurrentTime
      waitFor hydraTracer (remainingTime + 3 * blockTime) [n1] $
        output "ReadyToFanout" ["headId" .= headId]
      send n1 $ input "Fanout" []
      waitMatch (10 * blockTime) n1 $ \v ->
        guard $ v ^? key "tag" == Just "HeadIsFinalized"

      -- Assert final wallet balance
      (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
        `shouldReturn` lovelaceToValue (headAmount + commitAmount)
 where
  expectSuccessOnSignedDecommitTx n headId decommitTx = do
    let decommitUTxO = utxoFromTx decommitTx
        decommitTxId = txId decommitTx
    -- Sometimes use websocket, sometimes use HTTP
    join . generate $
      elements
        [ send n $ input "Decommit" ["decommitTx" .= decommitTx]
        , postDecommit n decommitTx
        ]
    waitFor hydraTracer 10 [n] $
      output "DecommitRequested" ["headId" .= headId, "decommitTx" .= decommitTx, "utxoToDecommit" .= decommitUTxO]
    waitFor hydraTracer 10 [n] $
      output "DecommitApproved" ["headId" .= headId, "decommitTxId" .= decommitTxId, "utxoToDecommit" .= decommitUTxO]
    failAfter 10 $ waitForUTxO backend decommitUTxO

    distributedUTxO <- waitForAllMatch 10 [n] $ \v -> do
      guard $ v ^? key "tag" == Just "DecommitFinalized"
      guard $ v ^? key "headId" == Just (toJSON headId)
      v ^? key "distributedUTxO" . _JSON

    guard $ distributedUTxO `UTxO.containsOutputs` UTxO.txOutputs decommitUTxO

  expectFailureOnUnsignedDecommitTx :: HydraClient -> HeadId -> Tx -> NominalDiffTime -> IO ()
  expectFailureOnUnsignedDecommitTx n headId decommitTx blockTime = do
    let unsignedDecommitTx = makeSignedTransaction [] $ getTxBody decommitTx
    -- Note: Just send to websocket, as that's how the following code checks
    -- that it failed. We could do the same for the HTTP endpoint, but doesn't
    -- quite seem worth the effort.
    send n $ input "Decommit" ["decommitTx" .= unsignedDecommitTx]
    validationError <- waitMatch (10 * blockTime) n $ \v -> do
      guard $ v ^? key "headId" == Just (toJSON headId)
      guard $ v ^? key "tag" == Just (Aeson.String "DecommitInvalid")
      guard $ v ^? key "decommitTx" == Just (toJSON unsignedDecommitTx)
      v ^? key "decommitInvalidReason" . key "validationError" . key "reason" . _JSON

    validationError `shouldContain` "MissingVKeyWitnessesUTXOW"

  hydraTracer = contramap FromHydraNode tracer

-- | Integration test for the SideLoadSnapshot API: open a single-party head,
-- deposit UTxO, side-load the deposit snapshot, then spend the deposited UTxO.
-- This exercises the real Cardano ledger and network to ensure the API inputs
-- and outputs are wired up correctly. The consensus-resumption logic is covered
-- by the BehaviorSpec unit tests.
canSideLoadSnapshot :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canSideLoadSnapshot tracer workDir backend hydraScriptsTxId = do
  (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime

  networkId <- Backend.queryNetworkId backend
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [] timing
      <&> setNetworkId networkId

  withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    aliceUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)

    send n1 $ input "Init" []
    headId <- waitMatch (10 * blockTime) n1 $ headIsOpenWith (Set.fromList [alice])

    -- Deposit something and wait for it to be finalized on-chain
    depositTx <- requestCommitTx n1 aliceUTxO
    Backend.submitTransaction backend depositTx
    waitFor hydraTracer (depositTimeout timing) [n1] $
      output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

    -- Side-load the deposit snapshot (sn=1)
    snapshotConfirmed <- getSnapshotConfirmed n1
    send n1 $ input "SideLoadSnapshot" ["snapshot" .= snapshotConfirmed]
    waitMatch (10 * blockTime) n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotSideLoaded"
      guard $ v ^? key "snapshotNumber" == Just (toJSON (1 :: Integer))

    -- After sideload, spending the deposited UTxO must work on the real ledger
    utxo <- getSnapshotUTxO n1
    tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
    send n1 $ input "NewTx" ["transaction" .= tx]
    waitMatch (20 * blockTime) n1 $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      guard $ v ^? key "snapshot" . key "number" == Just (toJSON (2 :: Integer))
 where
  hydraTracer = contramap FromHydraNode tracer

canResumeOnMemberAlreadyBootstrapped :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canResumeOnMemberAlreadyBootstrapped tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob]
  [(aliceCardanoVk, _aliceCardanoSk), (bobCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

  networkId <- Backend.queryNetworkId backend
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
      <&> setNetworkId networkId
  withHydraNodeCatchingUp hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [1, 2] $ \n1 -> do
    waitMatch (20 * blockTime) n1 $ \v -> do
      guard $ v ^? key "tag" == Just "Greetings"
      guard $ v ^? key "headStatus" == Just (toJSON Idle)
    withHydraNodeCatchingUp hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1, 2] $ \n2 -> do
      waitMatch (20 * blockTime) n2 $ \v -> do
        guard $ v ^? key "tag" == Just "Greetings"
        guard $ v ^? key "headStatus" == Just (toJSON Idle)
      threadDelay 5

    callProcess "rm" ["-rf", workDir </> "state-2"]
    threadDelay 1

    withHydraNodeCatchingUp hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1, 2] (const $ threadDelay 1)
      `shouldThrow` \(e :: SomeException) ->
        "hydra-node" `isInfixOf` show e
          && "etcd" `isInfixOf` show e

    setEnv "ETCD_INITIAL_CLUSTER_STATE" "existing"
    withHydraNodeCatchingUp hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1, 2] (const $ pure ())
    unsetEnv "ETCD_INITIAL_CLUSTER_STATE"
 where
  hydraTracer = contramap FromHydraNode tracer

-- XXX: restart scenarios require 3 party cluster in order to observe PeerDisconnected instead of NetworkDisconnected
waitsForChainInSyncAndSecure :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
waitsForChainInSyncAndSecure tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob, Carol]
  [(aliceCardanoVk, _), (bobCardanoVk, _), (carolCardanoVk, carolCardanoSk)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

  blockTime <- Backend.getBlockTime backend
  networkId <- Backend.queryNetworkId backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob, Carol] timing
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice, Carol] timing
      <&> setNetworkId networkId
  carolChainConfig <-
    chainConfigFor Carol workDir backend hydraScriptsTxId [Alice, Bob] timing
      <&> setNetworkId networkId

  withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      -- Open a head while Carol online
      headId <- withHydraNode hydraTracer blockTime carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) [n1, n2, n3] $ headIsOpenWith (Set.fromList [alice, bob, carol])

        -- Carol deposits something
        carolUTxO <- seedFromFaucet backend carolCardanoVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)
        depositTx <- requestCommitTx n3 carolUTxO
        Backend.submitTransaction backend depositTx
        waitFor hydraTracer (depositTimeout timing) [n1, n2, n3] $
          output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

        pure headId

      -- Carol disconnects and the others observe it
      waitForAllMatch 5 [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- Wait for some blocks to roll forward
      let unsyncedPeriod = case carolChainConfig of
            Cardano CardanoChainConfig{unsyncedPeriod = up} -> up
            Offline{} -> defaultUnsyncedPeriodFor (let Timing{contestationPeriod = cp} = timing in cp)
      threadDelay $ realToFrac (unsyncedPeriodToNominalDiffTime unsyncedPeriod + 50 * blockTime)

      -- Alice closes the head while Carol offline
      send n1 $ input "Close" []
      waitForAllMatch (20 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        guard $ v ^? key "headId" == Just (toJSON headId)

      -- Carol restarts
      withHydraNodeCatchingUp hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- The node reports that it is in the Open state when restarting.
        -- NOTE: chainSyncedStatus in Greetings reflects the persisted sync status
        -- (InSync from last session), not the current one. The node may not have
        -- processed any Ticks yet to detect it is out of sync.
        waitMatch 5 n3 $ \v -> do
          guard $ v ^? key "tag" == Just "Greetings"
          guard $ v ^? key "headStatus" == Just (toJSON Open)
          guard $ v ^? key "me" == Just (toJSON carol)
          guard $ isJust (v ^? key "hydraNodeVersion")
        -- The node detects it is out of sync with the chain
        waitMatch 5 n3 $ \v -> do
          guard $ v ^? key "tag" == Just "SyncedStatusReport"
          guard $ v ^? key "synced" == Just (toJSON CatchingUp)

        -- Then, Carol attempts submits a new transaction,
        -- without waiting for head to be closed
        utxo <- getSnapshotUTxO n3
        tx <- mkTransferTx testNetworkId utxo carolCardanoSk carolCardanoVk
        send n3 $ input "NewTx" ["transaction" .= tx]

        waitMatch 5 n3 $ \v -> do
          guard $ v ^? key "tag" == Just "RejectedInputBecauseUnsynced"

        -- Carol API notifies the node is back on sync with the chain
        -- note this is tuned based on how long it takes to sync
        waitMatch (unsyncedPeriodToNominalDiffTime unsyncedPeriod + 5 * blockTime) n3 $ \v -> do
          guard $ v ^? key "tag" == Just "NodeSynced"

        -- Finally, Carol observes the head getting closed
        waitMatch (20 * blockTime) n3 $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          guard $ v ^? key "headId" == Just (toJSON headId)
 where
  hydraTracer = contramap FromHydraNode tracer

-- | Three hydra nodes open a head and we assert that none of them sees errors if a party is duplicated.
threeNodesWithMirrorParty :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
threeNodesWithMirrorParty tracer workDir backend hydraScriptsTxId = do
  let parties = [Alice, Bob]

  [(aliceCardanoVk, aliceCardanoSk), (bobCardanoVk, _)] <- forM parties keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
  networkId <- Backend.queryNetworkId backend
  blockTime <- Backend.getBlockTime backend
  let timing = mkTestTiming blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] timing
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] timing
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
  let allNodeIds = [1, 2, 3]
  withHydraNode hydraTracer blockTime aliceChainConfig workDir 1 aliceSk [bobVk] allNodeIds $ \n1 -> do
    withHydraNode hydraTracer blockTime bobChainConfig workDir 2 bobSk [aliceVk] allNodeIds $ \n2 -> do
      -- One party will participate using same hydra credentials
      withHydraNode hydraTracer blockTime aliceChainConfig workDir 3 aliceSk [bobVk] allNodeIds $ \n3 -> do
        let clients = [n1, n2, n3]
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) clients $ headIsOpenWith (Set.fromList [alice, bob])

        -- N1 & N3 deposit the same thing at the same time
        -- XXX: one will fail but the head will still be usable
        aliceUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)
        raceLabelled_
          ( "request-commit-tx-n1"
          , (requestCommitTx n1 aliceUTxO >>= Backend.submitTransaction backend)
              `catch` \(_ :: SubmitTransactionException) -> pure ()
          )
          ( "request-commit-tx-n3"
          , (requestCommitTx n3 aliceUTxO >>= Backend.submitTransaction backend)
              `catch` \(_ :: SubmitTransactionException) -> pure ()
          )

        -- N2 deposits something
        bobUTxO <- seedFromFaucet backend bobCardanoVk (lovelaceToValue 2_000_000) (contramap FromFaucet tracer)
        depositTx <- requestCommitTx n2 bobUTxO
        Backend.submitTransaction backend depositTx

        -- Wait for at least bob's deposit to be finalized
        waitFor hydraTracer (depositTimeout timing) clients $
          output "CommitFinalized" ["headId" .= headId, "depositTxId" .= txId depositTx]

        -- N3 performs a simple transaction from N3 to itself
        utxo <- getSnapshotUTxO n3
        tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
        send n3 $ input "NewTx" ["transaction" .= tx]

        -- Everyone confirms the tx snapshot (number depends on how many deposits succeeded)
        waitForAllMatch (200 * blockTime) clients $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          snNum <- v ^? key "snapshot" . key "number" . _JSON :: Maybe Integer
          guard $ snNum >= 1
          -- Check that confirmed list is non-empty (tx was included)
          confirmed <- v ^? key "snapshot" . key "confirmed" . _JSON :: Maybe [Value]
          guard $ not (null confirmed)

      -- \| Mirror party N3 disconnects and the others observe it
      waitForAllMatch (100 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- N1 performs another simple transaction from N1 to itself
      utxo <- getSnapshotUTxO n1
      tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
      send n1 $ input "NewTx" ["transaction" .= tx]

      -- Everyone confirms it (snapshot number depends on previous deposits)
      waitForAllMatch (200 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        snNum <- v ^? key "snapshot" . key "number" . _JSON :: Maybe Integer
        guard $ snNum >= 2
        confirmed <- v ^? key "snapshot" . key "confirmed" . _JSON :: Maybe [Value]
        guard $ not (null confirmed)

-- * L2 scenarios

-- | Respend all outputs owned by a given key in the head every 'delay' seconds,
-- for 'numTimes' times.
respendNTimes :: HasCallStack => HydraClient -> SigningKey PaymentKey -> DiffTime -> Int -> IO ()
respendNTimes client sk delay numTimes = do
  utxo <- getSnapshotUTxO client
  respend numTimes utxo
 where
  vk = getVerificationKey sk

  respend !n utxo
    | n <= 0 = pure ()
    | otherwise = do
        tx <- mkTransferTx testNetworkId utxo sk vk
        utxo' <- submitToHead (signTx sk tx)
        threadDelay delay
        respend (n - 1) utxo'

  submitToHead tx = do
    send client $ input "NewTx" ["transaction" .= tx]
    waitMatch 10 client $ \v -> do
      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
      guard $
        toJSON tx
          `elem` (v ^.. key "snapshot" . key "confirmed" . values)
      v ^? key "snapshot" . key "utxo" >>= parseMaybe parseJSON

-- * Utilities

-- | Refuel given 'Actor' with given 'Lovelace' if current marked UTxO is below that amount.
refuelIfNeeded ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  backend ->
  Actor ->
  Coin ->
  IO ()
refuelIfNeeded tracer backend actor amount = do
  Faucet.delayBF backend
  (actorVk, _) <- keysFor actor
  existingUtxo <- Backend.queryUTxOFor backend QueryTip actorVk
  traceWith tracer $ StartingFunds{actor = actorName actor, utxo = existingUtxo}
  let currentBalance = selectLovelace $ balance @Tx existingUtxo
  when (currentBalance < amount) $ do
    utxo <- seedFromFaucet backend actorVk (lovelaceToValue amount) (contramap FromFaucet tracer)
    traceWith tracer $ RefueledFunds{actor = actorName actor, refuelingAmount = amount, utxo}

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  backend ->
  Actor ->
  IO ()
returnFundsToFaucet tracer backend actor = do
  Faucet.returnFundsToFaucet (contramap FromFaucet tracer) backend actor

headIsOpenWith :: Set Party -> Value -> Maybe HeadId
headIsOpenWith expectedParties v = do
  guard $ v ^? key "tag" == Just "HeadIsOpen"
  parties <- v ^? key "parties" >>= parseMaybe parseJSON
  guard $ parties == expectedParties
  headId <- v ^? key "headId"
  parseMaybe parseJSON headId

headIsFinalizedWith :: HeadId -> UTxO -> Value -> Maybe ()
headIsFinalizedWith expectedHeadId expectedUTxO v = do
  guard $ v ^? key "tag" == Just "HeadIsFinalized"
  headId' <- v ^? key "headId" >>= parseMaybe parseJSON
  utxo <- v ^? key "utxo" >>= parseMaybe parseJSON
  guard (headId' == expectedHeadId)
  guard (UTxO.containsOutputs utxo (UTxO.txOutputs expectedUTxO))

expectErrorStatus ::
  -- | Expected http status code
  Int ->
  -- | Optional string expected to be present somewhere in the response body
  Maybe ByteString ->
  -- | Expected exception
  HttpException ->
  Bool
expectErrorStatus
  stat
  mbodyContains
  ( VanillaHttpException
      ( L.HttpExceptionRequest
          _
          (L.StatusCodeException response chunk)
        )
    ) =
    L.responseStatus response == toEnum stat && not (B.null chunk) && assertBodyContains mbodyContains chunk
   where
    -- NOTE: The documentation says: Response body parameter MAY include the beginning of the response body so this can be partial.
    -- https://hackage.haskell.org/package/http-client-0.7.13.1/docs/Network-HTTP-Client.html#t:HttpExceptionContent
    assertBodyContains :: Maybe ByteString -> ByteString -> Bool
    assertBodyContains (Just bodyContains) bodyChunk = bodyContains `isInfixOf` bodyChunk
    assertBodyContains Nothing _ = False
expectErrorStatus _ _ _ = False

-- | Get the base URL for HTTP API calls to a hydra-node.
hydraNodeBaseUrl :: HydraClient -> String
hydraNodeBaseUrl HydraClient{hydraNodeId} = "http://127.0.0.1:" <> show (4000 + hydraNodeId)
