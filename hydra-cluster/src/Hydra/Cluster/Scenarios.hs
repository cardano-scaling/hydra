{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.Cluster.Scenarios where

import Hydra.Prelude
import Test.Hydra.Prelude

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
import Cardano.Ledger.Plutus (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import CardanoClient (
  QueryPoint (QueryTip),
  SubmitTransactionException,
  waitForUTxO,
 )
import CardanoNode (NodeLog)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens ((.~), (?~), (^.), (^..), (^?))
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey, key, values, _JSON, _String)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (isInfixOf)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
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
  PolicyId (..),
  Tx,
  TxId (..),
  TxOutDatum,
  UTxO,
  addTxInReference,
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
  mkScriptRef,
  mkScriptReference,
  mkScriptWitness,
  mkTxIn,
  mkTxOutAutoBalance,
  mkTxOutDatumHash,
  mkVkAddress,
  modifyTxOutValue,
  policyAssetsToValue,
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
  valueToPolicyAssets,
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
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.Backend (ChainBackend, buildTransaction, buildTransactionWithPParams, buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Cluster.Faucet (FaucetLog, createOutputAtAddress, seedFromFaucet, seedFromFaucetWithMinting, seedFromFaucet_)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk, carolVk)
import Hydra.Cluster.Mithril (MithrilLog)
import Hydra.Cluster.Options (Options)
import Hydra.Cluster.Util (chainConfigFor, chainConfigFor', keysFor, modifyConfig, setNetworkId)
import Hydra.Contract.Dummy (dummyMintingScript, dummyRewardingScript, dummyValidatorScript, dummyValidatorScriptAlwaysFails)
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx, unsafeBuildTransaction)
import Hydra.Ledger.Cardano.Evaluate (maxTxExecutionUnits)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node.DepositPeriod (DepositPeriod (..))
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), DirectOptions (..), RunOptions (..), startChainFrom)
import Hydra.Tx (HeadId, IsTx (balance), Party, txId)
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Deposit (constructDepositUTxO)
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import HydraNode (
  HydraClient (..),
  HydraNodeLog,
  getProtocolParameters,
  getSnapshotConfirmed,
  getSnapshotLastSeen,
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
  waitMatch,
  withHydraCluster,
  withHydraNode,
  withPreparedHydraNode,
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
import Network.HTTP.Types (urlEncode)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Process (callProcess)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genDatum, genKeyPair, genTxOutWithReferenceScript, genUTxOWithAssetsSized)
import Test.QuickCheck (Positive, choose, elements, generate)

data EndToEndLog
  = ClusterOptions {options :: Options}
  | FromCardanoNode NodeLog
  | FromFaucet FaucetLog
  | FromHydraNode HydraNodeLog
  | FromMithril MithrilLog
  | StartingFunds {actor :: String, utxo :: UTxO}
  | RefueledFunds {actor :: String, refuelingAmount :: Coin, utxo :: UTxO}
  | RemainingFunds {actor :: String, utxo :: UTxO}
  | PublishedHydraScriptsAt {hydraScriptsTxId :: [TxId]}
  | UsingHydraScriptsAt {hydraScriptsTxId :: [TxId]}
  | CreatedKey {keyPath :: FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

oneOfThreeNodesStopsForAWhile :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
oneOfThreeNodesStopsForAWhile tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob, Carol]
  [(aliceCardanoVk, aliceCardanoSk), (bobCardanoVk, _), (carolCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)
  networkId <- Backend.queryNetworkId backend

  let contestationPeriod = 1
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob, Carol] contestationPeriod
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice, Carol] contestationPeriod <&> setNetworkId networkId

  carolChainConfig <-
    chainConfigFor Carol workDir backend hydraScriptsTxId [Alice, Bob] contestationPeriod
      <&> setNetworkId networkId
  blockTime <- Backend.getBlockTime backend
  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    aliceUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 1_000_000) (contramap FromFaucet tracer)
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- Init
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) [n1, n2, n3] $ headIsInitializingWith (Set.fromList [alice, bob, carol])

        -- Alice commits something
        requestCommitTx n1 aliceUTxO >>= Backend.submitTransaction backend

        -- Everyone else commits nothing
        mapConcurrently_ (\n -> requestCommitTx n mempty >>= Backend.submitTransaction backend) [n2, n3]

        -- Observe open with the relevant UTxOs
        waitFor hydraTracer (20 * blockTime) [n1, n2, n3] $
          output "HeadIsOpen" ["utxo" .= toJSON aliceUTxO, "headId" .= headId]

        -- Perform a simple transaction from alice to herself
        utxo <- getSnapshotUTxO n1
        tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
        send n1 $ input "NewTx" ["transaction" .= tx]

        -- Everyone confirms it
        waitForAllMatch (200 * blockTime) [n1, n2, n3] $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "snapshot" . key "number" == Just (toJSON (1 :: Integer))

      -- Carol disconnects and the others observe it
      -- waitForAllMatch (100 * blockTime) [n1, n2] $ \v -> do
      --   guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- Alice never-the-less submits a transaction
      utxo <- getSnapshotUTxO n1
      tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
      send n1 $ input "NewTx" ["transaction" .= tx]

      -- Carol reconnects, and then the snapshot can be confirmed
      withHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- Note: We can't use `waitForAlMatch` here as it expects them to
        -- emit the exact same datatype; but Carol will be behind in sequence
        -- numbers as she was offline.
        flip mapConcurrently_ [n1, n2, n3] $ \n ->
          waitMatch (200 * blockTime) n $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $ v ^? key "snapshot" . key "number" == Just (toJSON (2 :: Integer))
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

  let contestationPeriod = 1
  networkId <- Backend.queryNetworkId backend
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] $ \n1 -> do
    headId <- withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] $ \n2 -> do
      send n1 $ input "Init" []
      -- XXX: might need to tweak the wait time
      waitForAllMatch 10 [n1, n2] $ headIsInitializingWith (Set.fromList [alice, bob])

    -- n1 does a commit while n2 is down
    requestCommitTx n1 mempty >>= Backend.submitTransaction backend
    waitFor hydraTracer 10 [n1] $
      output "Committed" ["party" .= bob, "utxo" .= object mempty, "headId" .= headId]

    -- n2 is back and does observe the commit
    withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] $ \n2 -> do
      waitFor hydraTracer 10 [n2] $
        output "Committed" ["party" .= bob, "utxo" .= object mempty, "headId" .= headId]

restartedNodeCanAbort :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
restartedNodeCanAbort tracer workDir backend hydraScriptsTxId = do
  refuelIfNeeded tracer backend Alice 100_000_000
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [] 2
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> modifyConfig (\config -> config{startChainFrom = Nothing})

  let hydraTracer = contramap FromHydraNode tracer
  headId1 <- withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    send n1 $ input "Init" []
    -- XXX: might need to tweak the wait time
    waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Also expect to see past server outputs replayed
    headId2 <- waitMatch 20 n1 $ headIsInitializingWith (Set.fromList [alice])
    headId1 `shouldBe` headId2
    send n1 $ input "Abort" []
    waitFor hydraTracer 20 [n1] $
      output "HeadIsAborted" ["utxo" .= object mempty, "headId" .= headId2]
  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    waitMatch 20 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "Greetings"
      guard $ v ^? key "headStatus" == Just (toJSON Idle)
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
  let contestationPeriod = truncate $ 10 * blockTime
      depositPeriod = truncate $ 50 * blockTime
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
      <&> modifyConfig (\config -> config{startChainFrom = Nothing, depositPeriod})

  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
      <&> modifyConfig (\config -> config{startChainFrom = Nothing, depositPeriod})

  (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
  commitUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)

  let hydraTracer = contramap FromHydraNode tracer

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
    (headId', decrementOuts) <- withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
      send n1 $ input "Init" []

      headId <- waitMatch (20 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice, bob])
      _ <- waitMatch (20 * blockTime) n2 $ headIsInitializingWith (Set.fromList [alice, bob])

      requestCommitTx n1 mempty >>= Backend.submitTransaction backend
      requestCommitTx n2 mempty >>= Backend.submitTransaction backend

      waitFor hydraTracer (20 * blockTime) [n1, n2] $
        output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

      resp <-
        parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
          <&> setRequestBodyJSON commitUTxO
            >>= httpJSON

      let depositTransaction = getResponseBody resp :: Tx
      let tx = signTx aliceCardanoSk depositTransaction

      Backend.submitTransaction backend tx

      waitFor hydraTracer (2 * realToFrac depositPeriod) [n1, n2] $
        output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= commitUTxO]

      waitFor hydraTracer (20 * blockTime) [n1, n2] $
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

      guard $ distributedUTxO `UTxO.containsOutputs` utxoFromTx decommitTx

      pure (headId, decommitUTxO)

    bobChainConfigFromTip <-
      chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
        <&> modifyConfig (\config -> config{startChainFrom = Just tip})

    withTempDir "blank-state" $ \tmpDir -> do
      callProcess "cp" ["-r", workDir </> "state-2", tmpDir]
      callProcess "rm" ["-rf", tmpDir </> "state-2" </> "state*"]
      callProcess "rm" ["-rf", tmpDir </> "state-2" </> "last-known-revision"]
      withHydraNode hydraTracer bobChainConfigFromTip tmpDir 2 bobSk [aliceVk] [1] $ \n2 -> do
        -- Also expect to see past server outputs replayed
        headId2 <- waitMatch 5 n2 $ headIsInitializingWith (Set.fromList [alice, bob])
        headId2 `shouldBe` headId'
        waitFor hydraTracer 5 [n2] $
          output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId2]

        distributedUTxO <- waitForAllMatch 5 [n2] $ \v -> do
          guard $ v ^? key "tag" == Just "DecommitFinalized"
          guard $ v ^? key "headId" == Just (toJSON headId2)
          v ^? key "distributedUTxO" . _JSON

        guard $ distributedUTxO `UTxO.containsOutputs` decrementOuts

        send n1 $ input "Close" []

        deadline' <- waitMatch (20 * blockTime) n2 $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          guard $ v ^? key "headId" == Just (toJSON headId')
          v ^? key "contestationDeadline" . _JSON
        remainingTime <- diffUTCTime deadline' <$> getCurrentTime
        waitFor hydraTracer (remainingTime + 3 * blockTime) [n1, n2] $
          output "ReadyToFanout" ["headId" .= headId']
        send n1 $ input "Fanout" []

        waitForAllMatch (10 * blockTime) [n1, n2] $ checkFanout headId' mempty

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
  ( `finally`
      do
        returnFundsToFaucet tracer backend Alice
        returnFundsToFaucet tracer backend AliceFunds
  )
    $ do
      refuelIfNeeded tracer backend Alice 55_000_000
      -- Start hydra-node on chain tip
      tip <- Backend.queryTip backend
      blockTime <- Backend.getBlockTime backend
      networkId <- Backend.queryNetworkId backend
      contestationPeriod <- CP.fromNominalDiffTime $ 10 * blockTime
      aliceChainConfig <-
        chainConfigFor' Alice workDir backend hydraScriptsTxId [] contestationPeriod (DepositPeriod 100)
          <&> modifyConfig (\config -> config{startChainFrom = Just tip})
            . setNetworkId networkId
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
        -- Initialize & open head
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

        -- Commit something from external key
        (walletVk, walletSk) <- keysFor AliceFunds
        amount <- Coin <$> generate (choose (10_000_000, 50_000_000))
        utxoToCommit <- seedFromFaucet backend walletVk (lovelaceToValue amount) (contramap FromFaucet tracer)
        requestCommitTx n1 utxoToCommit <&> signTx walletSk >>= Backend.submitTransaction backend

        waitFor hydraTracer (50 * blockTime) [n1] $
          output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]
        -- Close head
        send n1 $ input "Close" []
        deadline <- waitMatch (50 * blockTime) n1 $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          guard $ v ^? key "headId" == Just (toJSON headId)
          v ^? key "contestationDeadline" . _JSON
        remainingTime <- diffUTCTime deadline <$> getCurrentTime
        waitFor hydraTracer (remainingTime + 50 * blockTime) [n1] $
          output "ReadyToFanout" ["headId" .= headId]
        send n1 $ input "Fanout" []

        waitForAllMatch (50 * blockTime) [n1] $ checkFanout headId utxoToCommit
      traceRemainingFunds Alice
      traceRemainingFunds AliceFunds
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
    let contestationPeriod = 100
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        <&> modifyConfig (\config -> config{startChainFrom = Just tip})

    (walletVk, walletSk) <- generate genKeyPair
    let keyPath = workDir <> "/wallet.sk"
    _ <- writeFileTextEnvelope (File keyPath) Nothing walletSk
    traceWith tracer CreatedKey{keyPath}

    utxoToCommit <- seedFromFaucet backend walletVk (lovelaceToValue 100_000_000) (contramap FromFaucet tracer)

    let hydraTracer = contramap FromHydraNode tracer
    options <- prepareHydraNode aliceChainConfig workDir 1 aliceSk [] [] id
    let options' = options{persistenceRotateAfter}
    withPreparedHydraNode hydraTracer workDir 1 options' $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      blockTime <- Backend.getBlockTime backend
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
      requestCommitTx n1 utxoToCommit <&> signTx walletSk >>= Backend.submitTransaction backend
      waitFor hydraTracer (10 * blockTime) [n1] $
        output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]

      callback n1 walletSk headId

-- | Single hydra-node where the commit is done using some wallet UTxO.
singlePartyCommitsFromExternal ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
singlePartyCommitsFromExternal tracer workDir backend hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer backend Alice
        returnFundsToFaucet tracer backend AliceFunds
  )
    $ do
      refuelIfNeeded tracer backend Alice 25_000_000
      let contestationPeriod = 100
      aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
      let hydraNodeId = 1
      let hydraTracer = contramap FromHydraNode tracer
      blockTime <- Backend.getBlockTime backend
      withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

        (walletVk, walletSk) <- keysFor AliceFunds
        utxoToCommit <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)

        res <-
          runReq defaultHttpConfig $
            req
              POST
              (http "127.0.0.1" /: "commit")
              (ReqBodyJson utxoToCommit)
              (Proxy :: Proxy (JsonResponse (DraftCommitTxResponse Tx)))
              (port $ 4000 + hydraNodeId)

        let DraftCommitTxResponse{commitTx} = responseBody res
        Backend.submitTransaction backend $ signTx walletSk commitTx

        lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
          guard $ v ^? key "headId" == Just (toJSON headId)
          guard $ v ^? key "tag" == Just "HeadIsOpen"
          pure $ v ^? key "utxo"
        lockedUTxO `shouldBe` Just (toJSON utxoToCommit)

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
      let contestationPeriod = 1
      aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
      let hydraNodeId = 1
      let hydraTracer = contramap FromHydraNode tracer
      blockTime <- Backend.getBlockTime backend
      withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

        (walletVk, walletSk) <- keysFor AliceFunds

        -- Create money on L1
        let commitAmount = 100_000_000
        utxoToCommit <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)

        -- Push it into L2
        requestCommitTx n1 utxoToCommit
          <&> signTx walletSk
            >>= \tx -> do
              Backend.submitTransaction backend tx

        -- Check UTxO is present in L2
        waitFor hydraTracer (10 * blockTime) [n1] $
          output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]

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
        case buildTransactionWithPParams' pparams systemStart eraHistory stakePools (mkVkAddress networkId walletVk) utxoToCommit [] [scriptOutput] Nothing of
          Left e -> error $ show e
          Right tx -> do
            let signedL2tx = signTx walletSk tx
            send n1 $ input "NewTx" ["transaction" .= signedL2tx]

            waitMatch 10 n1 $ \v -> do
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
            -- tx' <- either (failure . show) pure =<< buildTransactionWithBody networkId nodeSocket (mkVkAddress networkId walletVk) body utxoToCommit
            txBody <- either (failure . show) pure (createAndValidateTransactionBody body)

            let spendTx' = makeSignedTransaction [] txBody
                spendTx = fromLedgerTx $ recomputeIntegrityHash pparams [PlutusV3] (toLedgerTx spendTx')
            let signedTx = signTx walletSk spendTx

            send n1 $ input "NewTx" ["transaction" .= signedTx]

            waitMatch 10 n1 $ \v -> do
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
      $ \utxoToCommit -> do
        -- Start hydra-node and open a head
        let contestationPeriod = 1
        aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        let hydraNodeId = 1
        let hydraTracer = contramap FromHydraNode tracer
        blockTime <- Backend.getBlockTime backend
        networkId <- Backend.queryNetworkId backend
        withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
          send n1 $ input "Init" []
          headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
          requestCommitTx n1 utxoToCommit <&> signTx walletSk >>= Backend.submitTransaction backend
          waitFor hydraTracer (10 * blockTime) [n1] $
            output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]

          -- Prepare a tx that re-spends everything owned by walletVk
          pparams <- getProtocolParameters n1
          let change = mkVkAddress networkId walletVk
          Right tx <- buildTransactionWithPParams pparams backend change utxoToCommit [] [] Nothing

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
                      & bodyTxL . collateralInputsTxBodyL .~ Set.map toLedgerTxIn (UTxO.inputSet utxoToCommit)
                      & bodyTxL . totalCollateralTxBodyL .~ SJust (UTxO.totalLovelace utxoToCommit)
                      & bodyTxL . withdrawalsTxBodyL .~ Withdrawals (Map.singleton rewardAccount 0)
                      & witsTxL . rdmrsTxWitsL .~ Redeemers (Map.singleton (ConwayRewarding $ AsIx 0) (redeemer, exUnits))
                      & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash script

          let signedL2tx = signTx walletSk tx'
          send n1 $ input "NewTx" ["transaction" .= signedL2tx]

          waitMatch 10 n1 $ \v -> do
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

singlePartyCommitsScriptBlueprint ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
singlePartyCommitsScriptBlueprint tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 20_000_000
    blockTime <- Backend.getBlockTime backend
    -- NOTE: Adapt periods to block times
    let contestationPeriod = truncate $ 10 * blockTime
        depositPeriod = truncate $ 50 * blockTime
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        <&> modifyConfig (\c -> c{depositPeriod})
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    (_, walletSk) <- keysFor AliceFunds
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
      (clientPayload, scriptUTxO, _) <- prepareScriptPayload 7_000_000 0

      res <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson clientPayload)
            (Proxy :: Proxy (JsonResponse Tx))
            (port $ 4000 + hydraNodeId)

      let commitTx = responseBody res
      Backend.submitTransaction backend commitTx

      lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "tag" == Just "HeadIsOpen"
        pure $ v ^? key "utxo"
      lockedUTxO `shouldBe` Just (toJSON scriptUTxO)

      -- incrementally commit script to a running Head
      (clientPayload', _, blueprint) <- prepareScriptPayload 5_000_000 2_000_000

      res' <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson clientPayload')
            (Proxy :: Proxy (JsonResponse Tx))
            (port $ 4000 + hydraNodeId)

      let depositTransaction = responseBody res'
      let tx = signTx walletSk depositTransaction

      Backend.submitTransaction backend tx

      let expectedDeposit = constructDepositUTxO (getTxId $ getTxBody blueprint) (txOuts' blueprint)
      waitFor hydraTracer (2 * realToFrac depositPeriod) [n1] $
        output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= expectedDeposit]
      waitFor hydraTracer (20 * blockTime) [n1] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx)]
      getSnapshotUTxO n1 `shouldReturn` scriptUTxO <> expectedDeposit
 where
  prepareScriptPayload lovelaceAmt commitAmount = do
    networkId <- Backend.queryNetworkId backend
    let scriptAddress = mkScriptAddress networkId dummyValidatorScript
    let datumHash :: TxOutDatum ctx
        datumHash = mkTxOutDatumHash ()
    (scriptIn, scriptOut) <- createOutputAtAddress networkId backend scriptAddress datumHash (lovelaceToValue lovelaceAmt)
    let scriptOut' = modifyTxOutValue (const $ lovelaceToValue commitAmount) scriptOut
    let scriptUTxO = UTxO.singleton scriptIn scriptOut

    let scriptWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness dummyValidatorScript (mkScriptDatum ()) (toScriptData ())
    let spendingTx =
          unsafeBuildTransaction $
            defaultTxBodyContent
              & addTxIns [(scriptIn, scriptWitness)]
              & addTxOut (fromCtxUTxOTxOut scriptOut')
    pure
      ( Aeson.object
          [ "blueprintTx" .= spendingTx
          , "utxo" .= scriptUTxO
          ]
      , scriptUTxO
      , spendingTx
      )

singlePartyDepositReferenceScript ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
singlePartyDepositReferenceScript tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 20_000_000
    blockTime <- Backend.getBlockTime backend
    -- NOTE: Adapt periods to block times
    let contestationPeriod = truncate $ 10 * blockTime
        depositPeriod = truncate $ 50 * blockTime
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        <&> modifyConfig (\c -> c{depositPeriod})
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    (_, walletSk) <- keysFor AliceFunds
    -- incrementally commit script to a running Head
    (referenceUTxO, scriptUTxO) <- publishReferenceScript tracer 20_000_000
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
      res <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson (mempty :: UTxO))
            (Proxy :: Proxy (JsonResponse Tx))
            (port $ 4000 + hydraNodeId)

      let commitTx = responseBody res
      Backend.submitTransaction backend commitTx

      waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "tag" == Just "HeadIsOpen"
      (clientPayload, blueprint) <- prepareBlueprintRequest referenceUTxO scriptUTxO
      res' <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson clientPayload)
            (Proxy :: Proxy (JsonResponse Tx))
            (port $ 4000 + hydraNodeId)

      let depositTransaction = responseBody res'
      let tx = signTx walletSk depositTransaction

      Backend.submitTransaction backend tx

      let expectedDeposit = constructDepositUTxO (getTxId $ getTxBody blueprint) (txOuts' blueprint)

      waitFor hydraTracer (2 * realToFrac depositPeriod) [n1] $
        output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= expectedDeposit]
      waitFor hydraTracer (20 * blockTime) [n1] $
        output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx)]
      getSnapshotUTxO n1 `shouldReturn` expectedDeposit
 where
  publishReferenceScript :: Tracer IO EndToEndLog -> CAPI.Lovelace -> IO (UTxO, UTxO)
  publishReferenceScript t lovelaceAmt = do
    (vk, sk) <- keysFor AliceFunds
    utxo <- seedFromFaucet backend vk (lovelaceToValue lovelaceAmt) (contramap FromFaucet t)
    pparams <- Backend.queryProtocolParameters backend QueryTip
    networkId <- Backend.queryNetworkId backend
    systemStart <- Backend.querySystemStart backend QueryTip
    eraHistory <- Backend.queryEraHistory backend QueryTip
    stakePools <- Backend.queryStakePools backend QueryTip
    let changeAddress = mkVkAddress networkId (getVerificationKey sk)
    let unspendableScriptAddress =
          mkScriptAddress networkId dummyValidatorScriptAlwaysFails

    let mkScriptTxOut =
          mkTxOutAutoBalance
            pparams
            unspendableScriptAddress
            mempty
            TxOutDatumNone

    let scriptOut = mkScriptTxOut $ mkScriptRef dummyValidatorScript

    case buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxo [] [scriptOut] Nothing of
      Left err -> error $ show err
      Right tx -> do
        let signedTx = signTx sk tx
        Backend.submitTransaction backend signedTx
        void $ Backend.awaitTransaction backend signedTx vk

        let scriptAddress = mkScriptAddress networkId dummyValidatorScript
        let datumHash :: TxOutDatum ctx
            datumHash = mkTxOutDatumHash ()
        (scriptIn', scriptOut') <- createOutputAtAddress networkId backend scriptAddress datumHash (lovelaceToValue 5_000_000)
        let referenceUTxO = uncurry UTxO.singleton $ List.head $ UTxO.toList $ utxoFromTx signedTx
        pure (referenceUTxO, UTxO.singleton scriptIn' scriptOut')

  prepareBlueprintRequest :: UTxO -> UTxO -> IO (Value, Tx)
  prepareBlueprintRequest referenceUTxO scriptUTxO = do
    let dat = toScriptData ()
    let (scriptInput, scriptOut) = List.head $ UTxO.toList scriptUTxO
    let (refInput, _) = List.head $ UTxO.toList referenceUTxO
    let scriptWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptReference refInput dummyValidatorScript CAPI.InlineScriptDatum dat

    let blueprint' =
          unsafeBuildTransaction $
            defaultTxBodyContent
              & addTxIns [(scriptInput, scriptWitness)]
              & addTxOut (fromCtxUTxOTxOut scriptOut)
              & addTxInReference refInput (Just dat)

    let blueprint =
          fromLedgerTx $
            toLedgerTx blueprint'
              & witsTxL . rdmrsTxWitsL
                .~ Redeemers (Map.singleton (Ledger.ConwaySpending $ AsIx 0) (toLedgerData dat, ExUnits 0 0))
    pure
      ( Aeson.object
          [ "blueprintTx" .= blueprint
          , "utxo" .= (scriptUTxO <> referenceUTxO)
          ]
      , blueprint
      )

persistenceCanLoadWithEmptyCommit ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
persistenceCanLoadWithEmptyCommit tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 20_000_000
    let contestationPeriod = 100
    aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    blockTime <- Backend.getBlockTime backend
    headId <- withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

      requestCommitTx n1 mempty >>= Backend.submitTransaction backend
      waitFor hydraTracer (10 * blockTime) [n1] $
        output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]
      pure headId
    let persistenceState = workDir </> "state-" <> show hydraNodeId </> "state"
    stateContents <- readFileBS persistenceState
    let headOpened = BSC.pack $ List.last (List.lines $ BSC.unpack stateContents)
    case headOpened ^? key "stateChanged" . key "tag" . _String of
      Nothing -> error "Failed to find HeadIsOpened in the state file"
      Just headIsOpen -> do
        headIsOpen `shouldBe` "HeadOpened"
        withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
          waitFor hydraTracer (10 * blockTime) [n1] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          getSnapshotUTxO n1 `shouldReturn` mempty

-- | Single hydra-node where the commit is done from a raw transaction
-- blueprint.
singlePartyCommitsFromExternalTxBlueprint ::
  ChainBackend backend =>
  Tracer IO EndToEndLog ->
  FilePath ->
  backend ->
  [TxId] ->
  IO ()
singlePartyCommitsFromExternalTxBlueprint tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 20_000_000
    let contestationPeriod = 100
    aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    (someExternalVk, someExternalSk) <- generate genKeyPair
    blockTime <- Backend.getBlockTime backend
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

      someUTxO <- seedFromFaucet backend someExternalVk (lovelaceToValue 10_000_000) (contramap FromFaucet tracer)
      utxoToCommit <- seedFromFaucet backend someExternalVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)
      networkId <- Backend.queryNetworkId backend
      let someAddress = mkVkAddress networkId someExternalVk
      let someOutput =
            TxOut
              someAddress
              (lovelaceToValue $ Coin 2_000_000)
              TxOutDatumNone
              ReferenceScriptNone
      buildTransaction backend someAddress utxoToCommit (fst <$> UTxO.toList someUTxO) [someOutput] >>= \case
        Left e -> failure $ show e
        Right tx -> do
          let unsignedTx = makeSignedTransaction [] $ getTxBody tx
          let clientPayload =
                Aeson.object
                  [ "blueprintTx" .= unsignedTx
                  , "utxo" .= utxoToCommit
                  ]
          res <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (ReqBodyJson clientPayload)
                (Proxy :: Proxy (JsonResponse Tx))
                (port $ 4000 + hydraNodeId)

          let commitTx = responseBody res
          let signedTx = signTx someExternalSk commitTx
          Backend.submitTransaction backend signedTx

          lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
            guard $ v ^? key "headId" == Just (toJSON headId)
            guard $ v ^? key "tag" == Just "HeadIsOpen"
            pure $ v ^? key "utxo"
          lockedUTxO `shouldBe` Just (toJSON utxoToCommit)

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
  let oneWeek = 60 * 60 * 24 * 7
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [] oneWeek
      <&> modifyConfig (\config -> config{startChainFrom = Just tip})
  let hydraTracer = contramap FromHydraNode tracer
  blockTime <- Backend.getBlockTime backend
  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Initialize & open head
    send n1 $ input "Init" []
    headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
    -- Commit nothing for now
    requestCommitTx n1 mempty >>= Backend.submitTransaction backend
    waitFor hydraTracer (10 * blockTime) [n1] $
      output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]
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
    let contestationPeriod = 100
    aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [hydraNodeId] $ \_ -> do
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

  withHydraCluster hydraTracer tmpDir nodeSocket' 1 cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \clients -> do
    let leader = head clients
    waitForNodesConnected hydraTracer 20 clients

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send leader $ input "Init" []
    void . waitForAllMatch 10 (toList clients) $
      headIsInitializingWith (Set.fromList [alice, bob, carol])

    mapConcurrently_ (\n -> requestCommitTx n mempty >>= Backend.submitTransaction backend) clients

    mapConcurrently_ shouldNotReceivePostTxError clients
 where
  --  Fail if a 'PostTxOnChainFailed' message is received.
  shouldNotReceivePostTxError client@HydraClient{hydraNodeId} = do
    err <- waitMatch 10 client $ \v -> do
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
  let contestationPeriod = 2
  networkId <- Backend.queryNetworkId backend
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob, Carol] contestationPeriod
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice, Carol] contestationPeriod
      <&> setNetworkId networkId
  carolChainConfig <-
    chainConfigFor Carol workDir backend hydraScriptsTxId [Alice, Bob] contestationPeriod
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        waitForNodesConnected hydraTracer 30 $ n1 :| [n2, n3]

    bobChainConfig' <-
      chainConfigFor Bob workDir backend hydraScriptsTxId [Carol] contestationPeriod
        <&> setNetworkId networkId
    carolChainConfig' <-
      chainConfigFor Carol workDir backend hydraScriptsTxId [Bob] contestationPeriod
        <&> setNetworkId networkId

    waitForNodesDisconnected hydraTracer 60 $ n1 :| []

    withHydraNode hydraTracer bobChainConfig' workDir 2 bobSk [carolVk] [2, 3] $ \n2 -> do
      withHydraNode hydraTracer carolChainConfig' workDir 3 carolSk [bobVk] [2, 3] $ \n3 -> do
        waitForNodesConnected hydraTracer 30 $ n2 :| [n3]

    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        waitForNodesConnected hydraTracer 30 $ n1 :| [n2, n3]

-- | Two hydra node setup where Alice is wrongly configured to use Carol's
-- cardano keys instead of Bob's which will prevent him to be notified the
-- `HeadIsInitializing` but he should still receive some notification.
initWithWrongKeys :: ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> backend -> [TxId] -> IO ()
initWithWrongKeys workDir tracer backend hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice
  (carolCardanoVk, _) <- keysFor Carol

  let contestationPeriod = 2
  aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [Carol] contestationPeriod
  bobChainConfig <- chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig workDir 3 aliceSk [bobVk] [3, 4] $ \n1 -> do
    withHydraNode hydraTracer bobChainConfig workDir 4 bobSk [aliceVk] [3, 4] $ \n2 -> do
      seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

      send n1 $ input "Init" []
      headId <-
        waitForAllMatch 10 [n1] $
          headIsInitializingWith (Set.fromList [alice, bob])

      let expectedParticipants =
            verificationKeyToOnChainId
              <$> [aliceCardanoVk, carolCardanoVk]

      -- We want the client to observe headId being opened without bob (node 2)
      -- being part of it
      participants <- waitMatch 10 n2 $ \v -> do
        guard $ v ^? key "tag" == Just (Aeson.String "IgnoredHeadInitializing")
        guard $ v ^? key "headId" == Just (toJSON headId)
        v ^? key "participants" . _JSON

      participants `shouldMatchList` expectedParticipants

startWithWrongPeers :: ChainBackend backend => FilePath -> Tracer IO EndToEndLog -> backend -> [TxId] -> IO ()
startWithWrongPeers workDir tracer backend hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice

  let contestationPeriod = 2
  aliceChainConfig <- chainConfigFor Alice workDir backend hydraScriptsTxId [Carol] contestationPeriod
  bobChainConfig <- chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig workDir 3 aliceSk [bobVk] [3, 4] $ \n1 -> do
    -- NOTE: here we deliberately use the wrong peer list for Bob
    withHydraNode hydraTracer bobChainConfig workDir 4 bobSk [aliceVk] [4] $ \_ -> do
      seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

      (clusterPeers, configuredPeers) <- waitMatch 20 n1 $ \v -> do
        guard $ v ^? key "tag" == Just (Aeson.String "NetworkClusterIDMismatch")
        clusterPeers <- v ^? key "clusterPeers" . _String
        configuredPeers <- v ^? key "misconfiguredPeers" . _String
        pure (clusterPeers, configuredPeers)

      when (clusterPeers == configuredPeers) $
        failure "Expected clusterPeers and configuredPeers to be different"
      clusterPeers `shouldBe` "0.0.0.0:5003=http://0.0.0.0:5003,0.0.0.0:5004=http://0.0.0.0:5004"
      configuredPeers `shouldBe` "0.0.0.0:5004=http://0.0.0.0:5004"

-- | Open a a two participant head and incrementally commit to it.
canCommit :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> NominalDiffTime -> backend -> [TxId] -> IO ()
canCommit tracer workDir blockTime backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      -- NOTE: Adapt periods to block times
      let contestationPeriod = truncate $ 10 * blockTime
          depositPeriod = truncate $ 100 * blockTime
      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitMatch (10 * blockTime) n2 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= Backend.submitTransaction backend
          requestCommitTx n2 mempty >>= Backend.submitTransaction backend
          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- Get some L1 funds
          (walletVk, walletSk) <- generate genKeyPair
          commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)
          commitUTxO2 <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)

          resp <-
            parseUrlThrow ("POST " <> hydraNodeBaseUrl n2 <> "/commit")
              <&> setRequestBodyJSON commitUTxO
                >>= httpJSON

          let depositTransaction = getResponseBody resp :: Tx
          let tx = signTx walletSk depositTransaction

          Backend.submitTransaction backend tx

          waitFor hydraTracer (2 * realToFrac depositPeriod) [n1, n2] $
            output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= commitUTxO]
          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx)]

          getSnapshotUTxO n1 `shouldReturn` commitUTxO

          resp2 <-
            parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
              <&> setRequestBodyJSON commitUTxO2
                >>= httpJSON

          let depositTransaction' = getResponseBody resp2 :: Tx
          let tx' = signTx walletSk depositTransaction'

          Backend.submitTransaction backend tx'

          waitFor hydraTracer (2 * realToFrac depositPeriod) [n1, n2] $
            output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= commitUTxO2]
          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx')]

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

          -- Assert final wallet balance
          (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
            `shouldReturn` balance (commitUTxO <> commitUTxO2)
 where
  hydraTracer = contramap FromHydraNode tracer

-- | Open a a two participant head and incrementally commit part of the UTxO.
canDepositPartially :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> NominalDiffTime -> backend -> [TxId] -> IO ()
canDepositPartially tracer workDir blockTime backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      let contestationPeriod = truncate $ 10 * blockTime
          depositPeriod = truncate $ 50 * blockTime
      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitMatch (10 * blockTime) n2 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= Backend.submitTransaction backend
          requestCommitTx n2 mempty >>= Backend.submitTransaction backend
          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- Get some L1 funds
          (walletVk, walletSk) <- generate genKeyPair
          tokensUTxO <- generate (genUTxOWithAssetsSized 8 (Just $ PolicyId $ CAPI.hashScript $ CAPI.PlutusScript dummyMintingScript))
          let assetsToValue = foldMap ((mempty <>) . uncurry policyAssetsToValue) . Map.toList
          let totalTokenValue = UTxO.totalValue tokensUTxO
          let tokenAssets = valueToPolicyAssets totalTokenValue
          let tokenAssetValue = assetsToValue tokenAssets
          let seedAmount = 10_000_000
          -- NOTE: We (and also the users) need to make sure we give enough ADA when committing. If deposit tx ADA amount is too low
          -- and some ADA is added to it after balancing in the wallet, then we have problems matching on the 'CommitApproved' etc.
          let commitAmount = 5_000_000
          commitUTxOWithTokens <- seedFromFaucetWithMinting backend walletVk (lovelaceToValue seedAmount <> tokenAssetValue) (contramap FromFaucet tracer) (Just dummyMintingScript)
          (clientPayload, blueprint) <- prepareBlueprintRequest commitUTxOWithTokens commitAmount walletVk

          res <-
            runReq defaultHttpConfig $
              req
                POST
                (http "127.0.0.1" /: "commit")
                (ReqBodyJson clientPayload)
                (Proxy :: Proxy (JsonResponse Tx))
                (port $ 4000 + 1)

          let depositTransaction = responseBody res
          let tx = signTx walletSk depositTransaction
          putStrLn $ renderTxWithUTxO commitUTxOWithTokens tx
          Backend.submitTransaction backend tx

          let expectedDeposit = constructDepositUTxO (getTxId $ getTxBody blueprint) (txOuts' blueprint)

          waitFor hydraTracer (2 * realToFrac depositPeriod) [n1, n2] $
            output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= expectedDeposit]
          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "CommitFinalized" ["headId" .= headId, "depositTxId" .= getTxId (getTxBody tx)]

          getSnapshotUTxO n1 `shouldReturn` expectedDeposit
          -- check that user balance balance contains the change from the commit tx
          (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
            `shouldReturn` lovelaceToValue (seedAmount - commitAmount)
            <> tokenAssetValue

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

          -- Assert final wallet balance
          (balance <$> Backend.queryUTxOFor backend QueryTip walletVk)
            `shouldReturn` lovelaceToValue seedAmount
            <> tokenAssetValue
 where
  hydraTracer = contramap FromHydraNode tracer

  prepareBlueprintRequest :: UTxO -> Coin -> CAPI.VerificationKey PaymentKey -> IO (Value, Tx)
  prepareBlueprintRequest utxo commitAmount vk = do
    networkId <- Backend.queryNetworkId backend
    let changeAddress = mkVkAddress @Era networkId vk
    let (i, o') = List.head $ UTxO.toList utxo
    let o = modifyTxOutValue (const $ lovelaceToValue commitAmount) o'
    let witness = BuildTxWith $ KeyWitness KeyWitnessForSpending

    let blueprint =
          unsafeBuildTransaction $
            defaultTxBodyContent
              & addTxIns [(i, witness)]
              & addTxOut (fromCtxUTxOTxOut o)

    putStrLn $ renderTxWithUTxO utxo blueprint
    pure
      ( Aeson.object
          [ "blueprintTx" .= blueprint
          , "utxo" .= utxo
          , "changeAddress" .= changeAddress
          ]
      , blueprint
      )

rejectCommit :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> NominalDiffTime -> backend -> [TxId] -> IO ()
rejectCommit tracer workDir blockTime backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $ do
    refuelIfNeeded tracer backend Alice 30_000_000
    -- NOTE: Adapt periods to block times
    let contestationPeriod = truncate $ 10 * blockTime
        depositPeriod = truncate $ 100 * blockTime
    networkId <- Backend.queryNetworkId backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})

    let pparamsDecorator = atKey "utxoCostPerByte" ?~ toJSON (Aeson.Number 4310)
    optionsWithUTxOCostPerByte <- prepareHydraNode aliceChainConfig workDir 1 aliceSk [] [] pparamsDecorator

    withPreparedHydraNode hydraTracer workDir 1 optionsWithUTxOCostPerByte $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

      -- Commit nothing
      requestCommitTx n1 mempty >>= Backend.submitTransaction backend
      waitFor hydraTracer (20 * blockTime) [n1] $
        output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

      -- Get some L1 funds
      (walletVk, _) <- generate genKeyPair
      commitUTxO' <- seedFromFaucet backend walletVk (lovelaceToValue 1_000_000) (contramap FromFaucet tracer)
      TxOut _ _ _ refScript <- generate genTxOutWithReferenceScript
      datum <- generate genDatum
      let commitUTxO :: UTxO.UTxO =
            UTxO.fromList $
              (\(i, TxOut addr _ _ _) -> (i, TxOut addr (lovelaceToValue 0) datum refScript))
                <$> UTxO.toList commitUTxO'
      response <-
        L.parseRequest ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
          <&> setRequestBodyJSON (commitUTxO :: UTxO.UTxO)
            >>= httpJSON

      let expectedError = getResponseBody response :: PostTxError Tx

      expectedError `shouldSatisfy` \case
        DepositTooLow{minimumValue, providedValue} -> providedValue < minimumValue
        _ -> False
 where
  hydraTracer = contramap FromHydraNode tracer

-- | Open a a single participant head, deposit and then recover it.
canRecoverDeposit :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canRecoverDeposit tracer workDir backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      -- NOTE: Directly expire deposits
      contestationPeriod <- CP.fromNominalDiffTime 1
      blockTime <- Backend.getBlockTime backend
      let depositPeriod = 1
      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        headId <- withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= Backend.submitTransaction backend
          requestCommitTx n2 mempty >>= Backend.submitTransaction backend

          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- stop the second node here
          pure headId

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

        let path = BSC.unpack $ urlEncode False $ encodeUtf8 $ T.pack $ show (getTxId $ getTxBody tx)
        -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
        diff <- realToFrac . diffUTCTime deadline <$> getCurrentTime
        threadDelay $ diff + 1

        (`shouldReturn` "OK") $
          parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n1 <> "/commits/" <> path)
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
    contestationPeriod <- CP.fromNominalDiffTime 2
    blockTime <- Backend.getBlockTime backend
    let depositPeriod = 1
    networkId <- Backend.queryNetworkId backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
    withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Init the head
      send n1 $ input "Init" []
      headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

      -- Commit nothing
      requestCommitTx n1 mempty >>= Backend.submitTransaction backend

      waitFor hydraTracer (20 * blockTime) [n1] $
        output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

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
      headId2 <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

      -- Commit nothing
      requestCommitTx n1 mempty >>= Backend.submitTransaction backend

      waitFor hydraTracer (20 * blockTime) [n1] $
        output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId2]

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

    deadline <- waitMatch 10 n $ \v -> do
      guard $ v ^? key "tag" == Just "CommitRecorded"
      v ^? key "deadline" >>= parseMaybe parseJSON

    pure (getTxId $ getTxBody tx, deadline)

  recover :: HydraClient -> (TxId, UTCTime) -> UTxO -> IO ()
  recover n (depositId, deadline) commitUTxO = do
    -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
    diff <- realToFrac . diffUTCTime deadline <$> getCurrentTime
    threadDelay $ diff + 1

    let path = BSC.unpack $ urlEncode False $ encodeUtf8 $ T.pack $ show depositId
    (`shouldReturn` "OK") $
      parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n <> "/commits/" <> path)
        >>= httpJSON
        <&> getResponseBody @String

    waitMatch 20 n $ \v -> do
      guard $ v ^? key "tag" == Just "CommitRecovered"
      guard $ v ^? key "recoveredUTxO" == Just (toJSON commitUTxO)

-- | Make sure to be able to see pending deposits.
canSeePendingDeposits :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> NominalDiffTime -> backend -> [TxId] -> IO ()
canSeePendingDeposits tracer workDir blockTime backend hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer backend Alice) $
    (`finally` returnFundsToFaucet tracer backend Bob) $ do
      refuelIfNeeded tracer backend Alice 30_000_000
      refuelIfNeeded tracer backend Bob 30_000_000
      -- NOTE: Adapt periods to block times
      let contestationPeriod = truncate $ 10 * blockTime
          depositPeriod = truncate $ 100 * blockTime

      networkId <- Backend.queryNetworkId backend
      aliceChainConfig <-
        chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      bobChainConfig <-
        chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId . modifyConfig (\c -> c{depositPeriod})
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        _ <- withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= Backend.submitTransaction backend
          requestCommitTx n2 mempty >>= Backend.submitTransaction backend

          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- stop the second node here
          pure ()

        -- Get some L1 funds
        (walletVk, walletSk) <- generate genKeyPair
        commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue 5_000_000) (contramap FromFaucet tracer)
        commitUTxO2 <- seedFromFaucet backend walletVk (lovelaceToValue 4_000_000) (contramap FromFaucet tracer)
        commitUTxO3 <- seedFromFaucet backend walletVk (lovelaceToValue 3_000_000) (contramap FromFaucet tracer)

        deposited <- forM [commitUTxO, commitUTxO2, commitUTxO3] $ \utxo -> do
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
          let path = BSC.unpack $ urlEncode False $ encodeUtf8 $ T.pack $ show deposit
          -- XXX: should know the deadline from the query above
          -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
          threadDelay $ realToFrac (toNominalDiffTime depositPeriod * 4)

          (`shouldReturn` "OK") $
            parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n1 <> "/commits/" <> path)
              >>= httpJSON
              <&> getResponseBody @String

          waitForAllMatch 10 [n1] $ \v -> do
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
    let contestationPeriod = 1
    networkId <- Backend.queryNetworkId backend
    blockTime <- Backend.getBlockTime backend
    aliceChainConfig <-
      chainConfigFor Alice workDir backend hydraScriptsTxId [] contestationPeriod
        <&> setNetworkId networkId
    withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

      (walletVk, walletSk) <- generate genKeyPair
      let headAmount = 8_000_000
      let commitAmount = 5_000_000
      headUTxO <- seedFromFaucet backend walletVk (lovelaceToValue headAmount) (contramap FromFaucet tracer)
      commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue commitAmount) (contramap FromFaucet tracer)

      requestCommitTx n1 (headUTxO <> commitUTxO) <&> signTx walletSk >>= Backend.submitTransaction backend

      waitFor hydraTracer 10 [n1] $
        output "HeadIsOpen" ["utxo" .= toJSON (headUTxO <> commitUTxO), "headId" .= headId]

      -- Decommit the single commitUTxO by creating a fully "respending" decommit transaction
      let walletAddress = mkVkAddress networkId walletVk
      decommitTx <- do
        let (i, o) = List.head $ UTxO.toList commitUTxO
        either (failure . show) pure $
          mkSimpleTx (i, o) (walletAddress, txOutValue o) walletSk

      expectFailureOnUnsignedDecommitTx n1 headId decommitTx
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

    guard $ distributedUTxO `UTxO.containsOutputs` decommitUTxO

  expectFailureOnUnsignedDecommitTx :: HydraClient -> HeadId -> Tx -> IO ()
  expectFailureOnUnsignedDecommitTx n headId decommitTx = do
    let unsignedDecommitTx = makeSignedTransaction [] $ getTxBody decommitTx
    -- Note: Just send to websocket, as that's how the following code checks
    -- that it failed. We could do the same for the HTTP endpoint, but doesn't
    -- quite seem worth the effort.
    send n $ input "Decommit" ["decommitTx" .= unsignedDecommitTx]
    validationError <- waitMatch 10 n $ \v -> do
      guard $ v ^? key "headId" == Just (toJSON headId)
      guard $ v ^? key "tag" == Just (Aeson.String "DecommitInvalid")
      guard $ v ^? key "decommitTx" == Just (toJSON unsignedDecommitTx)
      v ^? key "decommitInvalidReason" . key "validationError" . key "reason" . _JSON

    validationError `shouldContain` "MissingVKeyWitnessesUTXOW"

  hydraTracer = contramap FromHydraNode tracer

-- | Can side load snapshot and resume agreement after a peer comes back online with healthy configuration
canSideLoadSnapshot :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canSideLoadSnapshot tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob, Carol]
  [(aliceCardanoVk, aliceCardanoSk), (bobCardanoVk, _), (carolCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend carolCardanoVk 100_000_000 (contramap FromFaucet tracer)
  blockTime <- Backend.getBlockTime backend
  let contestationPeriod = 1

  networkId <- Backend.queryNetworkId backend
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob, Carol] contestationPeriod
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice, Carol] contestationPeriod
      <&> setNetworkId networkId
  carolChainConfig <-
    chainConfigFor Carol workDir backend hydraScriptsTxId [Alice, Bob] contestationPeriod
      <&> setNetworkId networkId

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    aliceUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 1_000_000) (contramap FromFaucet tracer)
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      -- Carol starts its node misconfigured
      let pparamsDecorator = atKey "maxTxSize" ?~ toJSON (Aeson.Number 0)
      wrongOptions <- prepareHydraNode carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] pparamsDecorator
      tx <- withPreparedHydraNode hydraTracer workDir 3 wrongOptions $ \n3 -> do
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) [n1, n2, n3] $ headIsInitializingWith (Set.fromList [alice, bob, carol])

        -- Alice commits something
        requestCommitTx n1 aliceUTxO >>= Backend.submitTransaction backend

        -- Everyone else commits nothing
        mapConcurrently_ (\n -> requestCommitTx n mempty >>= Backend.submitTransaction backend) [n2, n3]

        -- Observe open with the relevant UTxOs
        waitFor hydraTracer (20 * blockTime) [n1, n2, n3] $
          output "HeadIsOpen" ["utxo" .= toJSON aliceUTxO, "headId" .= headId]

        -- Alice submits a new transaction
        utxo <- getSnapshotUTxO n1
        tx <- mkTransferTx testNetworkId utxo aliceCardanoSk aliceCardanoVk
        send n1 $ input "NewTx" ["transaction" .= tx]

        -- Alice and Bob accept it
        waitForAllMatch (200 * blockTime) [n1, n2] $ \v -> do
          guard $ v ^? key "tag" == Just "TxValid"
          guard $ v ^? key "transactionId" == Just (toJSON $ txId tx)

        -- Carol does not because of its node being misconfigured
        waitMatch 3 n3 $ \v -> do
          guard $ v ^? key "tag" == Just "TxInvalid"
          guard $ v ^? key "transaction" . key "txId" == Just (toJSON $ txId tx)

        -- We observe that a snapshot is in progress, but Carol has not signed it.
        seenSn1 <- getSnapshotLastSeen n1
        seenSn2 <- getSnapshotLastSeen n2
        seenSn1 `shouldBe` seenSn2
        seenSn3 <- getSnapshotLastSeen n3
        seenSn2 `shouldNotBe` seenSn3

        pure tx

      -- Carol disconnects and the others observe it
      waitForAllMatch (100 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- Carol reconnects with healthy reconfigured node
      withHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- Carol re-submits the same transaction
        send n3 $ input "NewTx" ["transaction" .= tx]
        -- Carol accepts it
        waitMatch 3 n3 $ \v -> do
          guard $ v ^? key "tag" == Just "TxValid"
          guard $ v ^? key "transactionId" == Just (toJSON $ txId tx)
        -- But now Alice and Bob does not because they already applied it
        waitForAllMatch (200 * blockTime) [n1, n2] $ \v -> do
          guard $ v ^? key "tag" == Just "TxInvalid"
          guard $ v ^? key "transaction" . key "txId" == Just (toJSON $ txId tx)

        -- \| Up to this point the head became stuck and no further SnapshotConfirmed
        -- including above tx will be seen signed by everyone.
        -- We observe that a snapshot is in progress, but Carol has not signed it.
        seenSn1 <- getSnapshotLastSeen n1
        seenSn2 <- getSnapshotLastSeen n2
        seenSn1 `shouldBe` seenSn2
        seenSn3 <- getSnapshotLastSeen n3
        seenSn2 `shouldNotBe` seenSn3

        -- The party side-loads latest confirmed snapshot (which is the initial)
        -- This also prunes local txs, and discards any signing round inflight
        snapshotConfirmed <- getSnapshotConfirmed n1
        flip mapConcurrently_ [n1, n2, n3] $ \n -> do
          send n $ input "SideLoadSnapshot" ["snapshot" .= snapshotConfirmed]
          waitMatch (200 * blockTime) n $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotSideLoaded"
            guard $ v ^? key "snapshotNumber" == Just (toJSON (0 :: Integer))

        -- Carol re-submits the same transaction (but anyone can at this point)
        send n3 $ input "NewTx" ["transaction" .= tx]

        -- Everyone confirms it
        -- Note: We can't use `waitForAllMatch` here as it expects them to
        -- emit the exact same datatype; but Carol will be behind in sequence
        -- numbers as she was offline.
        flip mapConcurrently_ [n1, n2, n3] $ \n ->
          waitMatch (200 * blockTime) n $ \v -> do
            guard $ v ^? key "tag" == Just "SnapshotConfirmed"
            guard $ v ^? key "snapshot" . key "number" == Just (toJSON (1 :: Integer))
            -- Just check that everyone signed it.
            let sigs = v ^.. key "signatures" . key "multiSignature" . values
            guard $ length sigs == 3

        -- Finally observe everyone having the same latest seen snapshot.
        seenSn1' <- getSnapshotLastSeen n1
        seenSn2' <- getSnapshotLastSeen n2
        seenSn1' `shouldBe` seenSn2'
        seenSn3' <- getSnapshotLastSeen n3
        seenSn2' `shouldBe` seenSn3'
 where
  hydraTracer = contramap FromHydraNode tracer

canResumeOnMemberAlreadyBootstrapped :: ChainBackend backend => Tracer IO EndToEndLog -> FilePath -> backend -> [TxId] -> IO ()
canResumeOnMemberAlreadyBootstrapped tracer workDir backend hydraScriptsTxId = do
  let clients = [Alice, Bob]
  [(aliceCardanoVk, _aliceCardanoSk), (bobCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ backend bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

  networkId <- Backend.queryNetworkId backend
  let contestationPeriod = 1
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
      <&> setNetworkId networkId

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [1, 2] $ \n1 -> do
    waitMatch 20 n1 $ \v -> do
      guard $ v ^? key "tag" == Just "Greetings"
      guard $ v ^? key "headStatus" == Just (toJSON Idle)
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1, 2] $ \n2 -> do
      waitMatch 20 n2 $ \v -> do
        guard $ v ^? key "tag" == Just "Greetings"
        guard $ v ^? key "headStatus" == Just (toJSON Idle)

      threadDelay 5

    callProcess "rm" ["-rf", workDir </> "state-2"]

    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1, 2] (const $ pure ())
      `shouldThrow` \(e :: SomeException) ->
        "hydra-node" `isInfixOf` show e
          && "etcd" `isInfixOf` show e

    setEnv "ETCD_INITIAL_CLUSTER_STATE" "existing"
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1, 2] (const $ pure ())
    unsetEnv "ETCD_INITIAL_CLUSTER_STATE"
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

  let contestationPeriod = 1
  aliceChainConfig <-
    chainConfigFor Alice workDir backend hydraScriptsTxId [Bob] contestationPeriod
      <&> setNetworkId networkId
  bobChainConfig <-
    chainConfigFor Bob workDir backend hydraScriptsTxId [Alice] contestationPeriod
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
  let allNodeIds = [1, 2, 3]
  blockTime <- Backend.getBlockTime backend
  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] allNodeIds $ \n1 -> do
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] allNodeIds $ \n2 -> do
      -- One party will participate using same hydra credentials
      withHydraNode hydraTracer aliceChainConfig workDir 3 aliceSk [bobVk] allNodeIds $ \n3 -> do
        let clients = [n1, n2, n3]
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) clients $ headIsInitializingWith (Set.fromList [alice, bob])

        -- N1 & N3 commit the same thing at the same time
        -- XXX: one will fail but the head will still open
        aliceUTxO <- seedFromFaucet backend aliceCardanoVk (lovelaceToValue 1_000_000) (contramap FromFaucet tracer)
        raceLabelled_
          ( "request-commit-tx-n1"
          , (requestCommitTx n1 aliceUTxO >>= Backend.submitTransaction backend)
              `catch` \(_ :: SubmitTransactionException) -> pure ()
          )
          ( "request-commit-tx-n3"
          , (requestCommitTx n3 aliceUTxO >>= Backend.submitTransaction backend)
              `catch` \(_ :: SubmitTransactionException) -> pure ()
          )

        -- N2 commits something
        bobUTxO <- seedFromFaucet backend bobCardanoVk (lovelaceToValue 1_000_000) (contramap FromFaucet tracer)
        requestCommitTx n2 bobUTxO >>= Backend.submitTransaction backend

        -- Observe open with relevant UTxO
        waitFor hydraTracer (20 * blockTime) clients $
          output "HeadIsOpen" ["utxo" .= toJSON (aliceUTxO <> bobUTxO), "headId" .= headId]

        -- N3 performs a simple transaction from N3 to itself
        utxo <- getSnapshotUTxO n3
        tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
        send n3 $ input "NewTx" ["transaction" .= tx]

        -- Everyone confirms it
        waitForAllMatch (200 * blockTime) clients $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "snapshot" . key "number" == Just (toJSON (1 :: Integer))

      -- \| Mirror party N3 disconnects and the others observe it
      waitForAllMatch (100 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- N1 performs another simple transaction from N1 to itself
      utxo <- getSnapshotUTxO n1
      tx <- mkTransferTx networkId utxo aliceCardanoSk aliceCardanoVk
      send n1 $ input "NewTx" ["transaction" .= tx]

      -- Everyone confirms it
      waitForAllMatch (200 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        guard $ v ^? key "snapshot" . key "number" == Just (toJSON (2 :: Integer))

-- * L2 scenarios

-- | Finds UTxO owned by given key in the head and creates transactions
-- respending it to the same address as fast as possible, forever.
-- NOTE: This relies on zero-fee protocol parameters.
respendUTxO :: HydraClient -> SigningKey PaymentKey -> NominalDiffTime -> IO ()
respendUTxO client sk delay = do
  utxo <- getSnapshotUTxO client
  forever $ respend utxo
 where
  vk = getVerificationKey sk

  respend utxo = do
    tx <- mkTransferTx testNetworkId utxo sk vk
    utxo' <- submitToHead (signTx sk tx)
    threadDelay $ realToFrac delay
    respend utxo'

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
returnFundsToFaucet tracer =
  Faucet.returnFundsToFaucet (contramap FromFaucet tracer)

headIsInitializingWith :: Set Party -> Value -> Maybe HeadId
headIsInitializingWith expectedParties v = do
  guard $ v ^? key "tag" == Just "HeadIsInitializing"
  parties <- v ^? key "parties" >>= parseMaybe parseJSON
  guard $ parties == expectedParties
  headId <- v ^? key "headId"
  parseMaybe parseJSON headId

checkFanout :: HeadId -> UTxO -> Value -> Maybe ()
checkFanout expectedHeadId expectedUTxO v = do
  guard $ v ^? key "tag" == Just "HeadIsFinalized"
  headId' <- v ^? key "headId" >>= parseMaybe parseJSON
  utxo <- v ^? key "utxo" >>= parseMaybe parseJSON
  guard (headId' == expectedHeadId)
  guard (UTxO.containsOutputs utxo expectedUTxO)

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
