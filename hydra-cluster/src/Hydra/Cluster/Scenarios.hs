{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.Cluster.Scenarios where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (
  NodeLog,
  QueryPoint (QueryTip),
  RunningNode (..),
  buildTransaction,
  queryCurrentEra,
  queryTip,
  queryUTxOFor,
  submitTx,
 )
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens ((^?))
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _JSON)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (isInfixOf)
import Data.ByteString qualified as B
import Data.Set qualified as Set
import Hydra.API.HTTPServer (
  DraftCommitTxRequest (..),
  DraftCommitTxResponse (..),
  ScriptInfo (..),
  TransactionSubmitted (..),
  TxOutWithWitness (..),
 )
import Hydra.Cardano.Api (AnyCardanoEra (..), CardanoEra, File (File), Lovelace (..), PlutusScriptV2, Tx, TxId, UTxO, fromPlutusScript, lovelaceToValue, makeSignedTransaction, mkScriptAddress, mkTxOutDatumHash, mkTxOutDatumInline, mkVkAddress, selectLovelace, signTx, toLedgerTx, toScriptData, writeFileTextEnvelope, pattern ReferenceScriptNone, pattern TxOut, pattern TxOutDatumNone)
import Hydra.Chain.Direct.Tx (verificationKeyToOnChainId)
import Hydra.Cluster.Faucet (FaucetLog, createOutputAtAddress, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Faucet qualified as Faucet
import Hydra.Cluster.Fixture (Actor (..), actorName, alice, aliceSk, aliceVk, bob, bobSk, bobVk, carol, carolSk)
import Hydra.Cluster.Util (chainConfigFor, keysFor, modifyConfig, setNetworkId)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.HeadId (HeadId)
import Hydra.Ledger (IsTx (balance))
import Hydra.Ledger.Cardano (genKeyPair)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options (networkId, startChainFrom)
import Hydra.Party (Party)
import HydraNode (
  HydraClient (..),
  HydraNodeLog,
  input,
  output,
  requestCommitTx,
  send,
  waitFor,
  waitForAllMatch,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
 )
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
import PlutusLedgerApi.Test.Examples qualified as Plutus
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.QuickCheck (generate)

data EndToEndLog
  = FromCardanoNode NodeLog
  | FromFaucet FaucetLog
  | FromHydraNode HydraNodeLog
  | StartingFunds {actor :: String, utxo :: UTxO}
  | RefueledFunds {actor :: String, refuelingAmount :: Lovelace, utxo :: UTxO}
  | RemainingFunds {actor :: String, utxo :: UTxO}
  | PublishedHydraScriptsAt {hydraScriptsTxId :: TxId}
  | UsingHydraScriptsAt {hydraScriptsTxId :: TxId}
  | CreatedKey {keyPath :: FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

restartedNodeCanObserveCommitTx :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> TxId -> IO ()
restartedNodeCanObserveCommitTx tracer workDir cardanoNode hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  let clients = [Alice, Bob]
  [(aliceCardanoVk, _), (bobCardanoVk, _)] <- forM clients keysFor
  seedFromFaucet_ cardanoNode era aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
  seedFromFaucet_ cardanoNode era bobCardanoVk 100_000_000 (contramap FromFaucet tracer)

  let contestationPeriod = UnsafeContestationPeriod 1
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Bob] contestationPeriod
      <&> setNetworkId networkId

  bobChainConfig <-
    chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] contestationPeriod
      <&> setNetworkId networkId

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] pparams $ \n1 -> do
    headId <- withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] pparams $ \n2 -> do
      send n1 $ input "Init" []
      -- XXX: might need to tweak the wait time
      waitForAllMatch 10 [n1, n2] $ headIsInitializingWith (Set.fromList [alice, bob])

    -- n1 does a commit while n2 is down
    requestCommitTx n1 mempty >>= submitTx cardanoNode
    waitFor hydraTracer 10 [n1] $
      output "Committed" ["party" .= bob, "utxo" .= object mempty, "headId" .= headId]

    -- n2 is back and does observe the commit
    withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] pparams $ \n2 -> do
      waitFor hydraTracer 10 [n2] $
        output "Committed" ["party" .= bob, "utxo" .= object mempty, "headId" .= headId]
 where
  RunningNode{nodeSocket, networkId, pparams} = cardanoNode

testPreventResumeReconfiguredPeer :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> TxId -> IO ()
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
        withHydraNode hydraTracer aliceChainConfigWithoutBob workDir 2 aliceSk [] [1, 2] pparams
      aliceRestartsWithBobConfigured =
        withHydraNode hydraTracer aliceChainConfig workDir 2 aliceSk [bobVk] [1, 2] pparams

  withHydraNode hydraTracer bobChainConfig workDir 1 bobSk [aliceVk] [1, 2] pparams $ \n1 -> do
    aliceStartsWithoutKnowingBob $ \n2 -> do
      failToConnect hydraTracer [n1, n2]

    threadDelay 1

    aliceRestartsWithBobConfigured (const $ threadDelay 1)
      `shouldThrow` aFailure

    threadDelay 1

    removeDirectoryRecursive $ workDir </> "state-2"

    aliceRestartsWithBobConfigured $ \n2 -> do
      waitForNodesConnected hydraTracer 10 [n1, n2]
 where
  RunningNode{nodeSocket, networkId, pparams} = cardanoNode

  aFailure :: Selector HUnitFailure
  aFailure = const True

  failToConnect tr nodes = waitForNodesConnected tr 10 nodes `shouldThrow` anyException

restartedNodeCanAbort :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> TxId -> IO ()
restartedNodeCanAbort tracer workDir cardanoNode hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  refuelIfNeeded tracer cardanoNode era Alice 100_000_000
  let contestationPeriod = UnsafeContestationPeriod 2
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
      -- we delibelately do not start from a chain point here to highlight the
      -- need for persistence
      <&> modifyConfig (\config -> config{networkId, startChainFrom = Nothing})

  let hydraTracer = contramap FromHydraNode tracer
  headId1 <- withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] pparams $ \n1 -> do
    send n1 $ input "Init" []
    -- XXX: might need to tweak the wait time
    waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])

  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] pparams $ \n1 -> do
    -- Also expect to see past server outputs replayed
    headId2 <- waitMatch 10 n1 $ headIsInitializingWith (Set.fromList [alice])
    headId1 `shouldBe` headId2
    send n1 $ input "Abort" []
    waitFor hydraTracer 10 [n1] $
      output "HeadIsAborted" ["utxo" .= object mempty, "headId" .= headId2]
 where
  RunningNode{nodeSocket, networkId, pparams} = cardanoNode

-- | Step through the full life cycle of a Hydra Head with only a single
-- participant. This scenario is also used by the smoke test run via the
-- `hydra-cluster` executable.
singlePartyHeadFullLifeCycle ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
singlePartyHeadFullLifeCycle tracer workDir node hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (`finally` returnFundsToFaucet tracer node era Alice) $ do
    refuelIfNeeded tracer node era Alice 25_000_000
    -- Start hydra-node on chain tip
    tip <- queryTip networkId nodeSocket
    let contestationPeriod = UnsafeContestationPeriod 100
    aliceChainConfig <-
      chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] contestationPeriod
        <&> modifyConfig (\config -> config{networkId, startChainFrom = Just tip})
    withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] pparams $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch 600 n1 $ headIsInitializingWith (Set.fromList [alice])
      -- Commit nothing for now
      requestCommitTx n1 mempty >>= submitTx node
      waitFor hydraTracer 600 [n1] $
        output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]
      -- Close head
      send n1 $ input "Close" []
      deadline <- waitMatch 600 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        guard $ v ^? key "headId" == Just (toJSON headId)
        v ^? key "contestationDeadline" . _JSON
      -- Expect to see ReadyToFanout within 600 seconds after deadline.
      -- XXX: We still would like to have a network-specific time here
      remainingTime <- realToFrac . diffUTCTime deadline <$> getCurrentTime
      waitFor hydraTracer (remainingTime + 60) [n1] $
        output "ReadyToFanout" ["headId" .= headId]
      send n1 $ input "Fanout" []
      waitFor hydraTracer 600 [n1] $
        output "HeadIsFinalized" ["utxo" .= object mempty, "headId" .= headId]
    traceRemainingFunds Alice
 where
  hydraTracer = contramap FromHydraNode tracer

  RunningNode{networkId, nodeSocket, pparams} = node

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
  TxId ->
  -- | Continuation called when the head is open
  (HydraClient -> IO ()) ->
  IO ()
singlePartyOpenAHead tracer workDir node hydraScriptsTxId callback = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (`finally` returnFundsToFaucet tracer node era Alice) $ do
    refuelIfNeeded tracer node era Alice 25_000_000
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

    utxoToCommit <- seedFromFaucet node era walletVk 100_000_000 (contramap FromFaucet tracer)

    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] pparams $ \n1 -> do
      -- Initialize & open head
      send n1 $ input "Init" []
      headId <- waitMatch 600 n1 $ headIsInitializingWith (Set.fromList [alice])
      -- Commit nothing for now
      requestCommitTx n1 utxoToCommit <&> signTx walletSk >>= submitTx node
      waitFor hydraTracer 600 [n1] $
        output "HeadIsOpen" ["utxo" .= toJSON utxoToCommit, "headId" .= headId]

      callback n1
 where
  RunningNode{networkId, nodeSocket, pparams} = node

-- | Exercise committing a script utxo that uses inline datums.
singlePartyCommitsExternalScriptWithInlineDatum ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
singlePartyCommitsExternalScriptWithInlineDatum tracer workDir node hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (`finally` returnFundsToFaucet tracer node era Alice) $ do
    refuelIfNeeded tracer node era Alice 25_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] pparams $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch 600 n1 $ headIsInitializingWith (Set.fromList [alice])

      -- Prepare a script output on the network
      -- FIXME: spending validators have 3 arguments? does this succeed still? is it not run?
      let script = fromPlutusScript @PlutusScriptV2 $ Plutus.alwaysSucceedingNAryFunction 2
          scriptAddress = mkScriptAddress @PlutusScriptV2 networkId script
          reedemer = 1 :: Integer
          datum = 2 :: Integer
          scriptInfo = ScriptInfo (toScriptData reedemer) Nothing script
      (scriptTxIn, scriptTxOut) <- createOutputAtAddress node pparams scriptAddress (mkTxOutDatumInline datum)

      -- Commit the script output using known witness
      let clientPayload =
            DraftCommitTxRequest
              { utxoToCommit =
                  UTxO.singleton
                    ( scriptTxIn
                    , TxOutWithWitness
                        { txOut = scriptTxOut
                        , witness = Just scriptInfo
                        }
                    )
              }
      res <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson clientPayload)
            (Proxy :: Proxy (JsonResponse DraftCommitTxResponse))
            (port $ 4000 + hydraNodeId)
      let DraftCommitTxResponse{commitTx} = responseBody res
      submitTx node commitTx

      lockedUTxO <- waitMatch 60 n1 $ \v -> do
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "tag" == Just "HeadIsOpen"
        pure $ v ^? key "utxo"
      lockedUTxO `shouldBe` Just (toJSON $ UTxO.singleton (scriptTxIn, scriptTxOut))
 where
  RunningNode{networkId, nodeSocket, pparams} = node

-- | Single hydra-node where the commit is done from an external UTxO owned by a
-- script which requires providing script, datum and redeemer instead of
-- signing the transaction.
singlePartyCommitsFromExternalScript ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
singlePartyCommitsFromExternalScript tracer workDir node hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (`finally` returnFundsToFaucet tracer node era Alice) $ do
    refuelIfNeeded tracer node era Alice 25_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] pparams $ \n1 -> do
      send n1 $ input "Init" []
      headId <- waitMatch 600 n1 $ headIsInitializingWith (Set.fromList [alice])

      -- Prepare a script output on the network
      let script = fromPlutusScript @PlutusScriptV2 $ Plutus.alwaysSucceedingNAryFunction 2
          scriptAddress = mkScriptAddress @PlutusScriptV2 networkId script
          reedemer = 1 :: Integer
          datum = 2 :: Integer
          scriptInfo = ScriptInfo (toScriptData reedemer) (Just $ toScriptData datum) script
      (scriptTxIn, scriptTxOut) <- createOutputAtAddress node pparams scriptAddress (mkTxOutDatumHash datum)

      -- Commit the script output using known witness
      let clientPayload =
            DraftCommitTxRequest
              { utxoToCommit =
                  UTxO.singleton
                    ( scriptTxIn
                    , TxOutWithWitness
                        { txOut = scriptTxOut
                        , witness = Just scriptInfo
                        }
                    )
              }
      res <-
        runReq defaultHttpConfig $
          req
            POST
            (http "127.0.0.1" /: "commit")
            (ReqBodyJson clientPayload)
            (Proxy :: Proxy (JsonResponse DraftCommitTxResponse))
            (port $ 4000 + hydraNodeId)

      let DraftCommitTxResponse{commitTx} = responseBody res
      submitTx node commitTx

      lockedUTxO <- waitMatch 60 n1 $ \v -> do
        guard $ v ^? key "headId" == Just (toJSON headId)
        guard $ v ^? key "tag" == Just "HeadIsOpen"
        pure $ v ^? key "utxo"
      lockedUTxO `shouldBe` Just (toJSON $ UTxO.singleton (scriptTxIn, scriptTxOut))
 where
  RunningNode{networkId, nodeSocket, pparams} = node

singlePartyCannotCommitExternallyWalletUtxo ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
singlePartyCannotCommitExternallyWalletUtxo tracer workDir node hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (`finally` returnFundsToFaucet tracer node era Alice) $ do
    refuelIfNeeded tracer node era Alice 25_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [1] pparams $ \n1 -> do
      send n1 $ input "Init" []
      _headId <- waitMatch 60 n1 $ headIsInitializingWith (Set.fromList [alice])

      -- these keys should mimic external wallet keys needed to sign the commit tx

      -- internal wallet uses the actor keys internally so we need to use utxo
      -- present at this public key
      (userVk, _userSk) <- keysFor Alice
      -- submit the tx using our external user key to get a utxo to commit
      utxoToCommit <- seedFromFaucet node era userVk 2_000_000 (contramap FromFaucet tracer)
      -- Request to build a draft commit tx from hydra-node
      requestCommitTx n1 utxoToCommit `shouldThrow` expectErrorStatus 400 (Just "SpendingNodeUtxoForbidden")
 where
  RunningNode{networkId, nodeSocket, pparams} = node

-- | Initialize open and close a head on a real network and ensure contestation
-- period longer than the time horizon is possible. For this it is enough that
-- we can close a head and not wait for the deadline.
canCloseWithLongContestationPeriod ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
canCloseWithLongContestationPeriod tracer workDir node hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  refuelIfNeeded tracer node era Alice 100_000_000
  -- Start hydra-node on chain tip
  tip <- queryTip networkId nodeSocket
  let oneWeek = UnsafeContestationPeriod (60 * 60 * 24 * 7)
  aliceChainConfig <-
    chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] oneWeek
      <&> modifyConfig (\config -> config{networkId, startChainFrom = Just tip})
  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig workDir 1 aliceSk [] [1] pparams $ \n1 -> do
    -- Initialize & open head
    send n1 $ input "Init" []
    headId <- waitMatch 60 n1 $ headIsInitializingWith (Set.fromList [alice])
    -- Commit nothing for now
    requestCommitTx n1 mempty >>= submitTx node
    waitFor hydraTracer 60 [n1] $
      output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]
    -- Close head
    send n1 $ input "Close" []
    void $
      waitMatch 60 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
  traceRemainingFunds Alice
 where
  RunningNode{networkId, nodeSocket, pparams} = node

  traceRemainingFunds actor = do
    (actorVk, _) <- keysFor actor
    utxo <- queryUTxOFor networkId nodeSocket QueryTip actorVk
    traceWith tracer RemainingFunds{actor = actorName actor, utxo}

canSubmitTransactionThroughAPI ::
  Tracer IO EndToEndLog ->
  FilePath ->
  RunningNode ->
  TxId ->
  IO ()
canSubmitTransactionThroughAPI tracer workDir node hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (`finally` returnFundsToFaucet tracer node era Alice) $ do
    refuelIfNeeded tracer node era Alice 25_000_000
    aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [] $ UnsafeContestationPeriod 100
    let hydraNodeId = 1
    let hydraTracer = contramap FromHydraNode tracer
    withHydraNode hydraTracer aliceChainConfig workDir hydraNodeId aliceSk [] [hydraNodeId] pparams $ \_ -> do
      -- let's prepare a _user_ transaction from Bob to Carol
      (cardanoBobVk, cardanoBobSk) <- keysFor Bob
      (cardanoCarolVk, _) <- keysFor Carol
      -- create output for Bob to be sent to carol
      bobUTxO <- seedFromFaucet node era cardanoBobVk 5_000_000 (contramap FromFaucet tracer)
      let carolsAddress = mkVkAddress networkId cardanoCarolVk
          bobsAddress = mkVkAddress networkId cardanoBobVk
          carolsOutput =
            TxOut
              carolsAddress
              (lovelaceToValue $ Lovelace 2_000_000)
              TxOutDatumNone
              ReferenceScriptNone
      -- prepare fully balanced tx body
      buildTransaction networkId nodeSocket era bobsAddress bobUTxO (fst <$> UTxO.pairs bobUTxO) [carolsOutput] >>= \case
        Left e -> failure $ show e
        Right body -> do
          let unsignedTx = makeSignedTransaction [] body
          let unsignedRequest = toJSON $ toLedgerTx unsignedTx
          sendRequest hydraNodeId unsignedRequest
            `shouldThrow` expectErrorStatus 400 (Just "MissingVKeyWitnessesUTXOW")

          let signedTx = signTx cardanoBobSk unsignedTx
          let signedRequest = toJSON $ toLedgerTx signedTx
          (sendRequest hydraNodeId signedRequest <&> responseBody)
            `shouldReturn` TransactionSubmitted
 where
  RunningNode{networkId, nodeSocket, pparams} = node
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
threeNodesNoErrorsOnOpen :: Tracer IO EndToEndLog -> FilePath -> RunningNode -> TxId -> IO ()
threeNodesNoErrorsOnOpen tracer tmpDir node@RunningNode{networkId, nodeSocket, pparams} hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  aliceKeys@(aliceCardanoVk, _) <- generate genKeyPair
  bobKeys@(bobCardanoVk, _) <- generate genKeyPair
  carolKeys@(carolCardanoVk, _) <- generate genKeyPair

  let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
      hydraKeys = [aliceSk, bobSk, carolSk]

  let contestationPeriod = UnsafeContestationPeriod 2
  let hydraTracer = contramap FromHydraNode tracer
  withHydraCluster hydraTracer tmpDir nodeSocket 0 cardanoKeys hydraKeys hydraScriptsTxId pparams contestationPeriod $ \(leader :| rest) -> do
    let clients = leader : rest
    waitForNodesConnected hydraTracer 20 clients

    -- Funds to be used as fuel by Hydra protocol transactions
    seedFromFaucet_ node era aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ node era bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
    seedFromFaucet_ node era carolCardanoVk 100_000_000 (contramap FromFaucet tracer)

    send leader $ input "Init" []
    void . waitForAllMatch 10 clients $
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
initWithWrongKeys :: FilePath -> Tracer IO EndToEndLog -> RunningNode -> TxId -> IO ()
initWithWrongKeys workDir tracer node@RunningNode{networkId, nodeSocket, pparams} hydraScriptsTxId = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  (aliceCardanoVk, _) <- keysFor Alice
  (carolCardanoVk, _) <- keysFor Carol

  aliceChainConfig <- chainConfigFor Alice workDir nodeSocket hydraScriptsTxId [Carol] (UnsafeContestationPeriod 2)
  bobChainConfig <- chainConfigFor Bob workDir nodeSocket hydraScriptsTxId [Alice] (UnsafeContestationPeriod 2)

  let hydraTracer = contramap FromHydraNode tracer
  withHydraNode hydraTracer aliceChainConfig workDir 3 aliceSk [bobVk] [3, 4] pparams $ \n1 -> do
    withHydraNode hydraTracer bobChainConfig workDir 4 bobSk [aliceVk] [3, 4] pparams $ \n2 -> do
      seedFromFaucet_ node era aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

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

-- * Utilities

-- | Refuel given 'Actor' with given 'Lovelace' if current marked UTxO is below that amount.
refuelIfNeeded ::
  Tracer IO EndToEndLog ->
  RunningNode ->
  CardanoEra era ->
  Actor ->
  Lovelace ->
  IO ()
refuelIfNeeded tracer node era actor amount = do
  (actorVk, _) <- keysFor actor
  existingUtxo <- queryUTxOFor networkId nodeSocket QueryTip actorVk
  traceWith tracer $ StartingFunds{actor = actorName actor, utxo = existingUtxo}
  let currentBalance = selectLovelace $ balance @Tx existingUtxo
  when (currentBalance < amount) $ do
    utxo <- seedFromFaucet node era actorVk amount (contramap FromFaucet tracer)
    traceWith tracer $ RefueledFunds{actor = actorName actor, refuelingAmount = amount, utxo}
 where
  RunningNode{networkId, nodeSocket} = node

-- | Return the remaining funds to the faucet
returnFundsToFaucet ::
  Tracer IO EndToEndLog ->
  RunningNode ->
  CardanoEra era ->
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
