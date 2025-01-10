{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.Cluster.Scenarios where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  QueryPoint (QueryTip),
  RunningNode (..),
  buildTransaction,
  queryTip,
  queryUTxOFor,
  submitTx,
  waitForUTxO,
 )
import CardanoNode (NodeLog)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens ((^..), (^?))
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, values, _JSON, _String)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (isInfixOf)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BSC
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Hydra.API.HTTPServer (
  DraftCommitTxResponse (..),
  TransactionSubmitted (..),
 )
import Hydra.Cardano.Api (
  Coin (..),
  File (File),
  Key (SigningKey),
  PaymentKey,
  Tx,
  TxId,
  UTxO,
  addTxIns,
  defaultTxBodyContent,
  getTxBody,
  getTxId,
  getVerificationKey,
  lovelaceToValue,
  makeSignedTransaction,
  mkScriptAddress,
  mkScriptDatum,
  mkScriptWitness,
  mkTxOutDatumHash,
  mkVkAddress,
  scriptWitnessInCtx,
  selectLovelace,
  signTx,
  toScriptData,
  txOutValue,
  utxoFromTx,
  writeFileTextEnvelope,
  pattern BuildTxWith,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Cluster.Faucet (FaucetLog, createOutputAtAddress, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk, carolVk)
import Hydra.Cluster.Mithril (MithrilLog)
import Hydra.Cluster.Options (Options)
import Hydra.Cluster.Util (chainConfigFor, keysFor, modifyConfig, setNetworkId)
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx, unsafeBuildTransaction)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (DirectChainConfig (..), networkId, startChainFrom)
import Hydra.Tx (HeadId, IsTx (balance), Party, txId)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod), fromNominalDiffTime)
import Hydra.Tx.Utils (dummyValidatorScript, verificationKeyToOnChainId)
import HydraNode (
  HydraClient (..),
  HydraNodeLog,
  getSnapshotUTxO,
  input,
  output,
  postDecommit,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
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
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (choose, elements, generate)

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

oneOfNNodesCanDropForAWhile :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
oneOfNNodesCanDropForAWhile tracer workDir cardanoNode hydraScriptsTxId = do
  let clients = [Alice, Bob, Carol]
  [(aliceCardanoVk, aliceCardanoSk), (bobCardanoVk, _), (carolCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ cardanoNode carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

  let contestationPeriod = UnsafeContestationPeriod 1
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob, Carol] contestationPeriod
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice, Carol] contestationPeriod
      <&> setNetworkId networkId

  carolChainConfig <-
    chainConfigFor Carol workDir nodeSocket hydraScriptsTxId [Alice, Bob] contestationPeriod
      <&> setNetworkId networkId

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk, carolVk] [1, 2, 3] $ \n1 -> do
    aliceUTxO <- seedFromFaucet cardanoNode aliceCardanoVk 1_000_000 (contramap FromFaucet tracer)
    withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk, carolVk] [1, 2, 3] $ \n2 -> do
      withHydraNode hydraTracer carolChainConfig workDir 3 carolSk [aliceVk, bobVk] [1, 2, 3] $ \n3 -> do
        -- Init
        send n1 $ input "Init" []
        headId <- waitForAllMatch (10 * blockTime) [n1, n2, n3] $ headIsInitializingWith (Set.fromList [alice, bob, carol])

        -- Alice commits something
        requestCommitTx n1 aliceUTxO >>= submitTx cardanoNode

        -- Everyone else commits nothing
        mapConcurrently_ (\n -> requestCommitTx n mempty >>= submitTx cardanoNode) [n2, n3]

        -- Observe open with the relevant UTxOs
        waitFor hydraTracer (20 * blockTime) [n1, n2, n3] $
          output "HeadIsOpen" ["utxo" .= toJSON aliceUTxO, "headId" .= headId]

        -- Perform a simple transaction from alice to herself
        utxo <- getSnapshotUTxO n1
        tx <- mkTransferTx testNetworkId utxo aliceCardanoSk aliceCardanoVk
        send n1 $ input "NewTx" ["transaction" .= tx]

        -- Everyone confirms it
        waitForAllMatch (200 * blockTime) [n1, n2, n3] $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          guard $ v ^? key "snapshot" . key "number" == Just (toJSON (1 :: Integer))

      -- Carol disconnects and the others observe it
      waitForAllMatch (100 * blockTime) [n1, n2] $ \v -> do
        guard $ v ^? key "tag" == Just "PeerDisconnected"

      -- Alice never-the-less submits a transaction
      utxo <- getSnapshotUTxO n1
      tx <- mkTransferTx testNetworkId utxo aliceCardanoSk aliceCardanoVk
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
  RunningNode{nodeSocket, networkId, blockTime} = cardanoNode
  hydraTracer = contramap FromHydraNode tracer

restartedNodeCanObserveCommitTx :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
restartedNodeCanObserveCommitTx tracer workDir cardanoNode hydraScriptsTxId = do
  let clients = [Alice, Bob]
  [(aliceCardanoVk, _), (bobCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

  let contestationPeriod = UnsafeContestationPeriod 1
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob] contestationPeriod
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] $ \n1 -> do
    headId <- withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] $ \n2 -> do
      send n1 $ input "Init" []
      -- XXX: might need to tweak the wait time
      waitForAllMatch 10 [n1, n2] $ headIsInitializingWith (Set.fromList [alice, bob])

    -- n1 does a commit while n2 is down
    requestCommitTx n1 mempty >>= submitTx cardanoNode
    waitFor hydraTracer 10 [n1] $
      output "Committed" ["party" .= bob, "utxo" .= object mempty, "headId" .= headId]

    -- n2 is back and does observe the commit
    withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] $ \n2 -> do
      waitFor hydraTracer 10 [n2] $
        output "Committed" ["party" .= bob, "utxo" .= object mempty, "headId" .= headId]
 where
  RunningNode{nodeSocket, networkId} = cardanoNode

testPreventResumeReconfiguredPeer :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
testPreventResumeReconfiguredPeer tracer workDir cardanoNode hydraScriptsTxId = do
  let contestationPeriod = UnsafeContestationPeriod 1
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob] contestationPeriod
      <&> setNetworkId networkId

  aliceChainConfigWithoutBob <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
      aliceStartsWithoutKnowingBob =
        withHydraNode hydraTracer aliceChainConfigWithoutBob workDir 2 aliceSk [] [1, 2]
      aliceRestartsWithBobConfigured =
        withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2]

  withHydraNode hydraTracer bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] $ \n1 -> do
    aliceStartsWithoutKnowingBob $ \n2 -> do
      failToConnect hydraTracer (n1 :| [n2])

    threadDelay 1

    aliceRestartsWithBobConfigured (const $ threadDelay 1)
      `shouldThrow` aFailure

    threadDelay 1

    removeDirectoryRecursive $ workDir </> "state-2"

    aliceRestartsWithBobConfigured $ \n2 -> do
      waitForNodesConnected hydraTracer 10 (n1 :| [n2])
 where
  RunningNode{nodeSocket, networkId} = cardanoNode

  aFailure :: Selector HUnitFailure
  aFailure = const True

  failToConnect tr nodes = waitForNodesConnected tr 10 nodes `shouldThrow` anyException

restartedNodeCanAbort :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
restartedNodeCanAbort tracer workDir cardanoNode hydraScriptsTxId = do
  refuelIfNeeded tracer cardanoNode Alice 100_000_000
  let contestationPeriod = UnsafeContestationPeriod 2
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> modifyConfig (\config -> config{networkId, startChainFrom = Nothing})

  let hydraTracer = contramap FromHydraNode tracer
  headId1 <- withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    send n1 $ input "Init" []
    -- XXX: might need to tweak the wait time
    waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Also expect to see past server outputs replayed
    headId2 <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])
    headId1 `shouldBe` headId2
    send n1 $ input "Abort" []
    waitFor hydraTracer 10 [n1] $
      output "HeadIsAborted" ["utxo" .= object mempty, "headId" .= headId2]
 where
  RunningNode{nodeSocket, networkId} = cardanoNode

-- | Step through the full life cycle of a Hydra Head with only a single
-- participant. This scenario is also used by the smoke test run via the
-- `hydra-cluster` executable.
singlePartyHeadFullLifeCycle ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
singlePartyHeadFullLifeCycle tracer workDir node hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer node Alice
        returnFundsToFaucet tracer node AliceFunds
  )
    $ do
      refuelIfNeeded tracer node Alice 55_000_000
      -- Start hydra-node on chain tip
      tip <- queryTip networkId nodeSocket
      contestationPeriod <- fromNominalDiffTime $ 10 * blockTime
      aliceChainConfig <-
        chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
          <&> modifyConfig (\config -> config{networkId, startChainFrom = Just tip})
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
        -- Initialize & open head
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

        -- Commit something from external key
        (walletVk, walletSk) <- keysFor AliceFunds
        amount <- Coin <$> generate (choose (10_000_000, 50_000_000))
        utxoToCommit <- seedFromFaucet node walletVk amount (contramap FromFaucet tracer)
        requestCommitTx n1 utxoToCommit <&> signTx walletSk >>= submitTx node

        waitFor hydraTracer (10 * blockTime) [n1] $
          output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]
        -- Close head
        send n1 $ input "Close" []
        deadline <- waitMatch (10 * blockTime) n1 $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          guard $ v ^? key "headId" == Just (toJSON headId)
          v ^? key "contestationDeadline" . _JSON
        remainingTime <- diffUTCTime deadline <$> getCurrentTime
        waitFor hydraTracer (remainingTime + 3 * blockTime) [n1] $
          output "ReadyToFanout" ["headId" .= headId]
        send n1 $ input "Fanout" []
        waitFor hydraTracer (10 * blockTime) [n1] $
          output "HeadIsFinalized" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]
      traceRemainingFunds Alice
      traceRemainingFunds AliceFunds
 where
  hydraTracer = contramap FromHydraNode tracer

  RunningNode{networkId, nodeSocket, blockTime} = node

  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    utxo <- queryUTxOFor networkId nodeSocket QueryTip actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, utxo}

-- | Open a Hydra Head with only a single participant but some arbitrary UTxO
-- committed.
singlePartyOpenAHead ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  -- | Continuation called when the head is open
  (HydraClient -> SigningKey PaymentKey -> IO ()) ->
  IO ()
singlePartyOpenAHead tracer workDir node hydraScriptsTxId callback =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    refuelIfNeeded tracer node Alice 25_000_000
    -- Start hydra-node on chain tip
    tip <- queryTip networkId nodeSocket
    let contestationPeriod = UnsafeContestationPeriod 100
    aliceChainConfig <-
      chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
        <&> modifyConfig (\config -> config{networkId, startChainFrom = Just tip})

    (walletVk, walletSk) <- generate genKeyPair
    let keyPath = workDir <> "/wallet.sk"
    _ <- writeFileTextEnvelope (File keyPath) Nothing walletSk
    traceWith tracer CreatedKey{keyPath}

    utxoToCommit <- seedFromFaucet node walletVk 100_000_000 (contramap FromFaucet tracer)

    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
      requestCommitTx n1 utxoToCommit <&> signTx walletSk >>= submitTx node
      waitFor hydraTracer (10 * blockTime) [n1] $
        output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]

      callback n1 walletSk
 where
  RunningNode{networkId, nodeSocket, blockTime} = node

-- | Single hydra-node where the commit is done using some wallet UTxO.
singlePartyCommitsFromExternal ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
singlePartyCommitsFromExternal tracer workDir node hydraScriptsTxId =
  ( `finally`
      do
        returnFundsToFaucet tracer node Alice
        returnFundsToFaucet tracer node AliceFunds
  )
    $ do
      refuelIfNeeded tracer node Alice 25_000_000
      aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
      let hydraNodeId = 1
      let hydraTracer = contramap FromHydraNode tracer
      withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
        send n1 $ input "Init" []
        headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

        (walletVk, walletSk) <- keysFor AliceFunds
        utxoToCommit <- seedFromFaucet node walletVk 5_000_000 (contramap FromFaucet tracer)
        res <-
          runReq defaultHttpConfig $
            req
              POST
              (http "127.0.0.1" /: "commit")
              (ReqBodyJson utxoToCommit)
              (Proxy :: Proxy (JsonResponse (DraftCommitTxResponse Tx)))
              (port $ 4000 + hydraNodeId)

        let DraftCommitTxResponse{commitTx} = responseBody res
        submitTx node $ signTx walletSk commitTx

        lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
          guard $ v ^? key "headId" == Just (toJSON headId)
          guard $ v ^? key "tag" == Just "HeadIsOpen"
          pure $ v ^? key "utxo"
        lockedUTxO `shouldBe` Just (toJSON utxoToCommit)
 where
  RunningNode{nodeSocket, blockTime} = node

singlePartyCommitsScriptBlueprint ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
singlePartyCommitsScriptBlueprint tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    refuelIfNeeded tracer node Alice 20_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    (_, walletSk) <- keysFor AliceFunds
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

      (clientPayload, scriptUTxO) <- prepareScriptPayload 3_000_000

      res <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson clientPayload)
            (Proxy :: Proxy (JsonResponse Tx))
            (port $ 4000 + hydraNodeId)

      let commitTx = responseBody res
      submitTx node commitTx

      lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "tag" == Just "HeadIsOpen"
        pure $ v ^? key "utxo"
      lockedUTxO `shouldBe` Just (toJSON scriptUTxO)
      -- incrementally commit script to a running Head
      (clientPayload', scriptUTxO') <- prepareScriptPayload 2_000_000

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

      submitTx node tx

      waitFor hydraTracer 10 [n1] $
        output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= scriptUTxO']
      waitFor hydraTracer 10 [n1] $
        output "CommitFinalized" ["headId" .= headId, "theDeposit" .= getTxId (getTxBody tx)]

      send n1 $ input "GetUTxO" []

      waitFor hydraTracer 10 [n1] $
        output "GetUTxOResponse" ["headId" .= headId, "utxo" .= (scriptUTxO <> scriptUTxO')]
 where
  prepareScriptPayload lovelaceAmt = do
    let scriptAddress = mkScriptAddress networkId dummyValidatorScript
    let datumHash = mkTxOutDatumHash ()
    (scriptIn, scriptOut) <- createOutputAtAddress node scriptAddress datumHash (lovelaceToValue lovelaceAmt)
    let scriptUTxO = UTxO.singleton (scriptIn, scriptOut)

    let scriptWitness =
          BuildTxWith $
            ScriptWitness scriptWitnessInCtx $
              mkScriptWitness dummyValidatorScript (mkScriptDatum ()) (toScriptData ())
    let spendingTx =
          unsafeBuildTransaction $
            defaultTxBodyContent
              & addTxIns [(scriptIn, scriptWitness)]
    pure
      ( Aeson.object
          [ "blueprintTx" .= spendingTx
          , "utxo" .= scriptUTxO
          ]
      , scriptUTxO
      )

  RunningNode{networkId, nodeSocket, blockTime} = node

persistenceCanLoadWithEmptyCommit ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
persistenceCanLoadWithEmptyCommit tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    refuelIfNeeded tracer node Alice 20_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    headId <- withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

      requestCommitTx n1 mempty >>= submitTx node
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

          send n1 $ input "GetUTxO" []

          waitFor hydraTracer 10 [n1] $
            output "GetUTxOResponse" ["headId" .= headId, "utxo" .= (mempty :: UTxO)]
 where
  RunningNode{nodeSocket, blockTime} = node

-- | Single hydra-node where the commit is done from a raw transaction
-- blueprint.
singlePartyCommitsFromExternalTxBlueprint ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
singlePartyCommitsFromExternalTxBlueprint tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    refuelIfNeeded tracer node Alice 20_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    (someExternalVk, someExternalSk) <- generate genKeyPair
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])

      someUTxO <- seedFromFaucet node someExternalVk 10_000_000 (contramap FromFaucet tracer)
      utxoToCommit <- seedFromFaucet node someExternalVk 5_000_000 (contramap FromFaucet tracer)
      let someAddress = mkVkAddress networkId someExternalVk
      let someOutput =
            TxOut
              someAddress
              (lovelaceToValue $ Coin 2_000_000)
              TxOutDatumNone
              ReferenceScriptNone
      buildTransaction networkId nodeSocket someAddress utxoToCommit (fst <$> UTxO.pairs someUTxO) [someOutput] >>= \case
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
          submitTx node signedTx

          lockedUTxO <- waitMatch (10 * blockTime) n1 $ \v -> do
            guard $ v ^? key "headId" == Just (toJSON headId)
            guard $ v ^? key "tag" == Just "HeadIsOpen"
            pure $ v ^? key "utxo"
          lockedUTxO `shouldBe` Just (toJSON utxoToCommit)
 where
  RunningNode{networkId, nodeSocket, blockTime} = node

-- | Initialize open and close a head on a real network and ensure contestation
-- period longer than the time horizon is possible. For this it is enough that
-- we can close a head and not wait for the deadline.
canCloseWithLongContestationPeriod ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
canCloseWithLongContestationPeriod tracer workDir node hydraScriptsTxId = do
  refuelIfNeeded tracer node Alice 100_000_000
  -- Start hydra-node on chain tip
  tip <- queryTip networkId nodeSocket
  let oneWeek = UnsafeContestationPeriod (60 * 60 * 24 * 7)
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] oneWeek
      <&> modifyConfig (\config -> config{networkId, startChainFrom = Just tip})
  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
    -- Initialize & open head
    send n1 $ input "Init" []
    headId <- waitMatch (10 * blockTime) n1 $ headIsInitializingWith (Set.fromList [alice])
    -- Commit nothing for now
    requestCommitTx n1 mempty >>= submitTx node
    waitFor hydraTracer (10 * blockTime) [n1] $
      output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]
    -- Close head
    send n1 $ input "Close" []
    void $
      waitMatch (10 * blockTime) n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
  traceRemainingFunds Alice
 where
  RunningNode{networkId, nodeSocket, blockTime} = node

  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    utxo <- queryUTxOFor networkId nodeSocket QueryTip actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, utxo}

canSubmitTransactionThroughAPI ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  [TxId] ->
  IO ()
canSubmitTransactionThroughAPI tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    refuelIfNeeded tracer node Alice 25_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [hydraNodeId] $ \_ -> do
      -- let's prepare a _user_ transaction from Bob to Carol
      (cardanoBobVk, cardanoBobSk) <- keysFor Bob
      (cardanoCarolVk, _) <- keysFor Carol
      -- create output for Bob to be sent to carol
      bobUTxO <- seedFromFaucet node cardanoBobVk 5_000_000 (contramap FromFaucet tracer)
      let carolsAddress = mkVkAddress networkId cardanoCarolVk
          bobsAddress = mkVkAddress networkId cardanoBobVk
          carolsOutput =
            TxOut
              carolsAddress
              (lovelaceToValue $ Coin 2_000_000)
              TxOutDatumNone
              ReferenceScriptNone
      -- prepare fully balanced tx body
      buildTransaction networkId nodeSocket bobsAddress bobUTxO (fst <$> UTxO.pairs bobUTxO) [carolsOutput] >>= \case
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
  RunningNode{networkId, nodeSocket} = node
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
threeNodesNoErrorsOnOpen :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
threeNodesNoErrorsOnOpen tracer tmpDir node@RunningNode{nodeSocket} hydraScriptsTxId = do
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
  bobKeys@(bobCardanoVk, _) <- generate genKeyPair
  carolKeys@(carolCardanoVk, _) <- generate genKeyPair

  let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
      hydraKeys = [aliceSk, bobSk, carolSk]

  let contestationPeriod = UnsafeContestationPeriod 2
  let hydraTracer = contramap FromHydraNode tracer
  withHydraCluster hydraTracer tmpDir nodeSocket 1 cardanoKeys hydraKeys hydraScriptsTxId contestationPeriod $ \clients -> do
    let leader = head clients
    waitForNodesConnected hydraTracer 20 clients

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ node carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send leader $ input "Init" []
    void . waitForAllMatch 10 (toList clients) $
      headIsInitializingWith (Set.fromList [alice, bob, carol])

    mapConcurrently_ (\n -> requestCommitTx n mempty >>= submitTx node) clients

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

-- | Two hydra node setup where Alice is wrongly configured to use Carol's
-- cardano keys instead of Bob's which will prevent him to be notified the
-- `HeadIsInitializing` but he should still receive some notification.
initWithWrongKeys :: FilePath -> Tracer IO EndToEndLog -> RunningNode -> [TxId] -> IO ()
initWithWrongKeys workDir tracer node@RunningNode{nodeSocket} hydraScriptsTxId = do
  (aliceCardanoVk, _) <- keysFor Alice
  (carolCardanoVk, _) <- keysFor Carol

  aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Carol] (UnsafeContestationPeriod 2)
  bobChainConfig <- chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] (UnsafeContestationPeriod 2)

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig workDir 3 aliceSk [bobVk] [3, 4] $ \n1 -> do
    withHydraNode hydraTracer bobChainConfig workDir 4 bobSk [aliceVk] [3, 4] $ \n2 -> do
      seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

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

-- | Open a a two participant head and incrementally commit to it.
canCommit :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
canCommit tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    (`finally` returnFundsToFaucet tracer node Bob) $ do
      refuelIfNeeded tracer node Alice 30_000_000
      refuelIfNeeded tracer node Bob 30_000_000
      -- NOTE: it is important to provide _large_ enough contestation period so that
      -- increment tx can be submitted before the deadline
      let contestationPeriod = UnsafeContestationPeriod 20
      aliceChainConfig <-
        chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId
      bobChainConfig <-
        chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          -- _ <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [bob])
          headId <- waitMatch 20 n2 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= submitTx node
          requestCommitTx n2 mempty >>= submitTx node
          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- Get some L1 funds
          (walletVk, walletSk) <- generate genKeyPair
          commitUTxO <- seedFromFaucet node walletVk 5_000_000 (contramap FromFaucet tracer)
          commitUTxO2 <- seedFromFaucet node walletVk 5_000_000 (contramap FromFaucet tracer)

          resp <-
            parseUrlThrow ("POST " <> hydraNodeBaseUrl n2 <> "/commit")
              <&> setRequestBodyJSON commitUTxO
                >>= httpJSON

          let depositTransaction = getResponseBody resp :: Tx
          let tx = signTx walletSk depositTransaction

          submitTx node tx

          waitFor hydraTracer 20 [n1, n2] $
            output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= commitUTxO]
          waitFor hydraTracer 20 [n1, n2] $
            output "CommitFinalized" ["headId" .= headId, "theDeposit" .= getTxId (getTxBody tx)]

          send n2 $ input "GetUTxO" []

          waitFor hydraTracer 20 [n2] $
            output "GetUTxOResponse" ["headId" .= headId, "utxo" .= commitUTxO]
          resp2 <-
            parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
              <&> setRequestBodyJSON commitUTxO2
                >>= httpJSON

          let depositTransaction' = getResponseBody resp2 :: Tx
          let tx' = signTx walletSk depositTransaction'

          submitTx node tx'

          waitFor hydraTracer 20 [n1, n2] $
            output "CommitApproved" ["headId" .= headId, "utxoToCommit" .= commitUTxO2]
          waitFor hydraTracer 20 [n1, n2] $
            output "CommitFinalized" ["headId" .= headId, "theDeposit" .= getTxId (getTxBody tx')]

          send n1 $ input "GetUTxO" []

          waitFor hydraTracer 20 [n1] $
            output "GetUTxOResponse" ["headId" .= headId, "utxo" .= (commitUTxO <> commitUTxO2)]

          send n2 $ input "Close" []

          deadline <- waitMatch (20 * blockTime) n2 $ \v -> do
            guard $ v ^? key "tag" == Just "HeadIsClosed"
            v ^? key "contestationDeadline" . _JSON

          remainingTime <- diffUTCTime deadline <$> getCurrentTime
          waitFor hydraTracer (remainingTime + 3 * blockTime) [n1, n2] $
            output "ReadyToFanout" ["headId" .= headId]
          send n2 $ input "Fanout" []
          waitMatch (20 * blockTime) n2 $ \v ->
            guard $ v ^? key "tag" == Just "HeadIsFinalized"

          -- Assert final wallet balance
          (balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
            `shouldReturn` balance (commitUTxO <> commitUTxO2)
 where
  RunningNode{networkId, nodeSocket, blockTime} = node

  hydraTracer = contramap FromHydraNode tracer

  hydraNodeBaseUrl HydraClient{hydraNodeId} = "http://127.0.0.1:" <> show (4000 + hydraNodeId)

-- | Open a a single participant head, deposit and then recover it.
canRecoverDeposit :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
canRecoverDeposit tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $
    (`finally` returnFundsToFaucet tracer node Bob) $ do
      refuelIfNeeded tracer node Alice 30_000_000
      refuelIfNeeded tracer node Bob 30_000_000
      -- NOTE: this value is also used to determine the deposit deadline
      let deadline = 5
      let contestationPeriod = UnsafeContestationPeriod deadline
      aliceChainConfig <-
        chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId
      bobChainConfig <-
        chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        headId <- withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= submitTx node
          requestCommitTx n2 mempty >>= submitTx node

          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- stop the second node here
          pure headId

        -- Get some L1 funds
        (walletVk, walletSk) <- generate genKeyPair
        let commitAmount = 5_000_000
        commitUTxO <- seedFromFaucet node walletVk commitAmount (contramap FromFaucet tracer)

        (balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
          `shouldReturn` lovelaceToValue commitAmount

        resp <-
          parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
            <&> setRequestBodyJSON commitUTxO
              >>= httpJSON

        let depositTransaction = getResponseBody resp :: Tx

        let tx = signTx walletSk depositTransaction

        submitTx node tx

        waitForAllMatch 10 [n1] $ \v -> do
          guard $ v ^? key "tag" == Just "CommitRecorded"

        (selectLovelace . balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
          `shouldReturn` 0

        let path = BSC.unpack $ urlEncode False $ encodeUtf8 $ T.pack $ show (getTxId $ getTxBody tx)
        -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
        -- NOTE: for some reason threadDelay on MacOS behaves differently than on Linux so we need + 1 here
        threadDelay $ fromIntegral (deadline * 2 + 1)

        recoverResp <-
          parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n1 <> "/commits/" <> path)
            >>= httpJSON

        (getResponseBody recoverResp :: String) `shouldBe` "OK"

        waitForAllMatch 20 [n1] $ \v -> do
          guard $ v ^? key "tag" == Just "CommitRecovered"

        (balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
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
        (balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
          `shouldReturn` balance commitUTxO
 where
  RunningNode{networkId, nodeSocket, blockTime} = node

  hydraTracer = contramap FromHydraNode tracer

  hydraNodeBaseUrl HydraClient{hydraNodeId} = "http://127.0.0.1:" <> show (4000 + hydraNodeId)

-- | Make sure to be able to see pending deposits.
canSeePendingDeposits :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
canSeePendingDeposits tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $
    (`finally` returnFundsToFaucet tracer node Bob) $ do
      refuelIfNeeded tracer node Alice 30_000_000
      refuelIfNeeded tracer node Bob 30_000_000
      let deadline = 1
      let contestationPeriod = UnsafeContestationPeriod deadline
      aliceChainConfig <-
        chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob] contestationPeriod
          <&> setNetworkId networkId
      bobChainConfig <-
        chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
          <&> setNetworkId networkId
      withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [bobVk] [2] $ \n1 -> do
        _ <- withHydraNode hydraTracer bobChainConfig workDir 2 bobSk [aliceVk] [1] $ \n2 -> do
          send n1 $ input "Init" []
          headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice, bob])

          -- Commit nothing
          requestCommitTx n1 mempty >>= submitTx node
          requestCommitTx n2 mempty >>= submitTx node

          waitFor hydraTracer (20 * blockTime) [n1, n2] $
            output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

          -- stop the second node here
          pure ()

        -- Get some L1 funds
        (walletVk, walletSk) <- generate genKeyPair
        commitUTxO <- seedFromFaucet node walletVk 5_000_000 (contramap FromFaucet tracer)
        commitUTxO2 <- seedFromFaucet node walletVk 4_000_000 (contramap FromFaucet tracer)
        commitUTxO3 <- seedFromFaucet node walletVk 3_000_000 (contramap FromFaucet tracer)

        deposited <- flip execStateT [] $ forM [commitUTxO, commitUTxO2, commitUTxO3] $ \utxo -> do
          resp <-
            parseUrlThrow ("POST " <> hydraNodeBaseUrl n1 <> "/commit")
              <&> setRequestBodyJSON utxo
                >>= httpJSON

          let depositTransaction = getResponseBody resp :: Tx

          let tx = signTx walletSk depositTransaction

          liftIO $ submitTx node tx

          liftIO $ waitForAllMatch 10 [n1] $ \v ->
            guard $ v ^? key "tag" == Just "CommitRecorded"

          pendingDepositReq <-
            parseUrlThrow ("GET " <> hydraNodeBaseUrl n1 <> "/commits")
              >>= httpJSON

          let expectedResponse = getTxId (getTxBody tx)
          _ <- modify (expectedResponse :)
          expected <- get
          let expectedResp = getResponseBody pendingDepositReq :: [TxId]
          liftIO $ expectedResp `shouldBe` expected

        forM_ deposited $ \deposit -> do
          let path = BSC.unpack $ urlEncode False $ encodeUtf8 $ T.pack $ show deposit
          -- NOTE: we need to wait for the deadline to pass before we can recover the deposit
          threadDelay $ fromIntegral (deadline * 2)
          recoverResp <-
            parseUrlThrow ("DELETE " <> hydraNodeBaseUrl n1 <> "/commits/" <> path)
              >>= httpJSON

          (getResponseBody recoverResp :: String) `shouldBe` "OK"

          waitForAllMatch 10 [n1] $ \v -> do
            guard $ v ^? key "tag" == Just "CommitRecovered"

        pendingDepositReq <-
          parseUrlThrow ("GET " <> hydraNodeBaseUrl n1 <> "/commits")
            >>= httpJSON

        let expectedResp = getResponseBody pendingDepositReq :: [TxId]
        expectedResp `shouldBe` []
 where
  RunningNode{networkId, nodeSocket, blockTime} = node

  hydraTracer = contramap FromHydraNode tracer

  hydraNodeBaseUrl HydraClient{hydraNodeId} = "http://127.0.0.1:" <> show (4000 + hydraNodeId)

-- | Open a a single participant head with some UTxO and incrementally decommit it.
canDecommit :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> [TxId] -> IO ()
canDecommit tracer workDir node hydraScriptsTxId =
  (`finally` returnFundsToFaucet tracer node Alice) $ do
    refuelIfNeeded tracer node Alice 30_000_000
    let contestationPeriod = UnsafeContestationPeriod 1
    aliceChainConfig <-
      chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
        <&> setNetworkId networkId
    withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

      (walletVk, walletSk) <- generate genKeyPair
      let headAmount = 8_000_000
      let commitAmount = 5_000_000
      headUTxO <- seedFromFaucet node walletVk headAmount (contramap FromFaucet tracer)
      commitUTxO <- seedFromFaucet node walletVk commitAmount (contramap FromFaucet tracer)

      requestCommitTx n1 (headUTxO <> commitUTxO) <&> signTx walletSk >>= submitTx node

      waitFor hydraTracer 10 [n1] $
        output "HeadIsOpen" ["utxo" .= toJSON (headUTxO <> commitUTxO), "headId" .= headId]

      -- Decommit the single commitUTxO by creating a fully "respending" decommit transaction
      let walletAddress = mkVkAddress networkId walletVk
      decommitTx <- do
        let (i, o) = List.head $ UTxO.pairs commitUTxO
        either (failure . show) pure $
          mkSimpleTx (i, o) (walletAddress, txOutValue o) walletSk

      expectFailureOnUnsignedDecommitTx n1 headId decommitTx
      expectSuccessOnSignedDecommitTx n1 headId decommitTx

      -- After decommit Head UTxO should not contain decommitted outputs and wallet owns the funds on L1
      getSnapshotUTxO n1 `shouldReturn` headUTxO
      (balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
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
      (balance <$> queryUTxOFor networkId nodeSocket QueryTip walletVk)
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
    failAfter 10 $ waitForUTxO node decommitUTxO
    waitFor hydraTracer 10 [n] $
      output "DecommitFinalized" ["headId" .= headId, "decommitTxId" .= decommitTxId]

  expectFailureOnUnsignedDecommitTx n headId decommitTx = do
    let unsignedDecommitTx = makeSignedTransaction [] $ getTxBody decommitTx
    join . generate $
      elements
        [ send n $ input "Decommit" ["decommitTx" .= unsignedDecommitTx]
        , postDecommit n unsignedDecommitTx
        ]

    validationError <- waitMatch 10 n $ \v -> do
      guard $ v ^? key "headId" == Just (toJSON headId)
      guard $ v ^? key "tag" == Just (Aeson.String "DecommitInvalid")
      guard $ v ^? key "decommitTx" == Just (toJSON unsignedDecommitTx)
      v ^? key "decommitInvalidReason" . key "validationError" . key "reason" . _JSON

    validationError `shouldContain` "MissingVKeyWitnessesUTXOW"

  hydraTracer = contramap FromHydraNode tracer

  RunningNode{networkId, nodeSocket, blockTime} = node

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
  Tracer IO EndToEndLog ->
  RunningNode ->
  Actor ->
  Coin ->
  IO ()
refuelIfNeeded tracer node actor amount = do
  (actorVk, _) <- keysFor actor
  existingUtxo <- queryUTxOFor networkId nodeSocket QueryTip actorVk
  traceWith tracer $ StartingFunds{actor = actorName actor, utxo = existingUtxo}
  let currentBalance = selectLovelace $ balance @Tx existingUtxo
  when (currentBalance < amount) $ do
    utxo <- seedFromFaucet node actorVk amount (contramap FromFaucet tracer)
    traceWith tracer $ RefueledFunds{actor = actorName actor, refuelingAmount = amount, utxo}
 where
  RunningNode{networkId, nodeSocket} = node

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  Tracer IO EndToEndLog ->
  RunningNode ->
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
