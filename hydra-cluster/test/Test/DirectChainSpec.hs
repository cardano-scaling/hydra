{-# LANGUAGE DuplicateRecordFields #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (waitForUTxO)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Concurrent.STM (takeTMVar)
import Control.Concurrent.STM.TMVar (putTMVar)
import Data.List.Split (splitWhen)
import Hydra.Cardano.Api (
  ChainPoint (..),
  lovelaceToValue,
  unFile,
 )
import Hydra.Chain (
  Chain (Chain, postTx),
  ChainEvent (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
  initHistory,
 )
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Cardano (loadChainContext, mkTinyWallet)
import Hydra.Chain.Direct (DirectBackend (..), IntersectionNotFoundException (..), withDirectChain)
import Hydra.Chain.Direct.Handlers (CardanoChainLog)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Cluster.Faucet (
  FaucetLog,
  publishHydraScriptsAs,
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Alice, Bob, Carol, Faucet),
  alice,
  aliceSk,
  bob,
  carol,
  cperiod,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor, modifyConfig, readConfigFile)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Tracer, nullTracer, showLogsOnFailure)
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), ChainConfig (..), DirectOptions (..), toArgNetworkId)
import Hydra.Tx.Crypto (aggregate, sign)
import Hydra.Tx.HeadId (HeadId, HeadSeed (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Hydra.Tx.Snapshot qualified as Snapshot
import Hydra.Tx.Utils (
  splitUTxO,
  verificationKeyToOnChainId,
 )
import System.FilePath ((</>))
import System.Process (proc, readCreateProcess)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)

spec :: Spec
spec = around (showLogsOnFailure "DirectChainSpec") $ do
  it "can open a head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \_ backend -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp backend hydraScriptsTxId [Bob, Carol] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $ \aliceChain@CardanoChainTest{postTx} -> do
          -- Bob setup
          bobChainConfig <- chainConfigFor Bob tmp backend hydraScriptsTxId [Alice, Carol] cperiod
          withDirectChainTest nullTracer bobChainConfig bob $ \bobChain@CardanoChainTest{} -> do
            -- Scenario
            participants <- loadParticipants [Alice, Bob, Carol]
            let headParameters = HeadParameters cperiod [alice, bob, carol]
            postTx $ InitTx{participants, headParameters}
            (aliceHeadId, aliceHeadSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants
            (bobHeadId, bobHeadSeed) <- bobChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            aliceHeadSeed `shouldBe` bobHeadSeed
            aliceHeadId `shouldBe` bobHeadId

  it "can open, close & fanout a Head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \_ backend -> do
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp backend hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@CardanoChainTest{postTx} -> do
            -- Scenario
            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            (headId, headSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            let v = 0
            let snapshotVersion = 0
            let snapshot =
                  Snapshot
                    { headId
                    , number = 1
                    , utxo = mempty
                    , confirmed = []
                    , utxoToCommit = Nothing
                    , utxoToDecommit = Nothing
                    , version = snapshotVersion
                    }

            postTx $ CloseTx headId headParameters snapshotVersion (ConfirmedSnapshot{snapshot, signatures = aggregate [sign aliceSk snapshot]})

            deadline <-
              waitMatch aliceChain $ \case
                Observation{observedTx = OnCloseTx{snapshotNumber, contestationDeadline}}
                  | snapshotNumber == 1 -> Just contestationDeadline
                _ -> Nothing
            now <- getCurrentTime
            unless (deadline > now) $
              failure $
                "contestationDeadline in the past: " <> show deadline <> ", now: " <> show now
            delayUntil deadline

            waitMatch aliceChain $ \case
              Tick t _ | t > deadline -> Just ()
              _ -> Nothing
            postTx $
              FanoutTx
                { utxo = Snapshot.utxo snapshot
                , -- if snapshotVersion is not the same as local version, it
                  -- means we observed a commit so it needs to be fanned-out as well
                  utxoToCommit = if snapshotVersion /= v then Snapshot.utxoToCommit snapshot else Nothing
                , utxoToDecommit = Snapshot.utxoToDecommit snapshot
                , headSeed
                , contestationDeadline = deadline
                }
            let expectedUTxO =
                  ( Snapshot.utxo snapshot
                      <> if snapshotVersion /= v
                        then fromMaybe mempty (Snapshot.utxoToCommit snapshot)
                        else mempty
                  )
                    `withoutUTxO` fromMaybe mempty (Snapshot.utxoToDecommit snapshot)
            aliceChain `observesInTimeSatisfying` \case
              OnFanoutTx _ finalUTxO ->
                if UTxO.containsOutputs finalUTxO (UTxO.txOutputs expectedUTxO)
                  then pure ()
                  else failure "OnFanoutTx does not contain expected UTxO"
              _ -> failure "expected OnFanoutTx"

            failAfter 5 $
              waitForUTxO backend expectedUTxO

  it "can restart head to point in the past and replay on-chain events" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \_ backend -> do
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp backend hydraScriptsTxId [] cperiod
        participants <- loadParticipants [Alice]
        let headParameters = HeadParameters cperiod [alice]
        -- Scenario
        tip <- withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@CardanoChainTest{postTx} -> do
            tip <- Backend.queryTip backend
            postTx $ InitTx{participants, headParameters}
            void $ aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants
            pure tip

        let aliceChainConfig' = aliceChainConfig & modifyConfig (\cfg -> cfg{startChainFrom = Just tip})
        -- REVIEW: It's a bit weird now that we would use the original chain
        -- state here. Does this test even make sense with persistence?
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig' alice $
          \aliceChain@CardanoChainTest{} ->
            void $ aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

  it "cannot restart head to an unknown point" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \_ backend -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet

        let headerHash = fromString (replicate 64 '0')
        let fakeTip = ChainPoint 42 headerHash
        aliceChainConfig <-
          chainConfigFor Alice tmp backend hydraScriptsTxId [] cperiod
            <&> modifyConfig (\cfg -> cfg{startChainFrom = Just fakeTip})
        let action = withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $ \_ ->
              threadDelay 5 >> fail "should not execute main action but did?"

        action `shouldThrow` \case
          IntersectionNotFound{} -> True

  it "can publish and query reference scripts in a timely manner" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \_ backend -> do
        readConfigFile ("credentials" </> "faucet.sk") >>= writeFileBS (tmp </> "faucet.sk")
        let DirectBackend DirectOptions{nodeSocket, networkId} = backend
        hydraScriptsTxIdStr <-
          readCreateProcess
            ( proc
                "hydra-node"
                ( "publish-scripts"
                    : mconcat
                      [ ["--node-socket", unFile nodeSocket]
                      , toArgNetworkId networkId
                      , ["--cardano-signing-key", tmp </> "faucet.sk"]
                      ]
                )
            )
            ""
        let hydraScriptsTxId = fromString <$> splitWhen (== ',') (filter (/= '\n') hydraScriptsTxIdStr)
        failAfter 5 $ void $ Backend.queryScriptRegistry backend hydraScriptsTxId

  it "can only contest once" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \_ backend -> do
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp backend hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@CardanoChainTest{postTx} -> do
            (aliceExternalVk, _aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet backend aliceExternalVk (lovelaceToValue 1_000_000) (contramap FromFaucet tracer)

            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            -- Alice close with the initial snapshot U0
            postTx $ CloseTx headId headParameters 0 InitialSnapshot{headId}
            deadline <- waitMatch aliceChain $ \case
              Observation{observedTx = OnCloseTx{snapshotNumber, contestationDeadline}}
                | snapshotNumber == 0 -> Just contestationDeadline
              _ -> Nothing
            let (inHead, toDecommit) = splitUTxO someUTxO
            -- Alice contests with some snapshot U1 -> successful
            let snapshot1 =
                  Snapshot
                    { headId
                    , number = 1
                    , utxo = inHead
                    , confirmed = []
                    , utxoToCommit = Nothing
                    , utxoToDecommit = Just toDecommit
                    , version = 0
                    }
            postTx $
              ContestTx
                { headId
                , headParameters
                , openVersion = 0
                , contestingSnapshot =
                    ConfirmedSnapshot
                      { snapshot = snapshot1
                      , signatures = aggregate [sign aliceSk snapshot1]
                      }
                }
            aliceChain `observesInTime` OnContestTx{headId, snapshotNumber = 1, contestationDeadline = deadline}

            -- Alice contests with some snapshot U2 -> expect fail
            let snapshot2 =
                  Snapshot
                    { headId
                    , number = 2
                    , utxo = inHead
                    , confirmed = []
                    , utxoToCommit = Nothing
                    , utxoToDecommit = Just toDecommit
                    , version = 1
                    }
            let contestAgain =
                  postTx $
                    ContestTx
                      { headId
                      , headParameters
                      , openVersion = 1
                      , contestingSnapshot =
                          ConfirmedSnapshot
                            { snapshot = snapshot2
                            , signatures = aggregate [sign aliceSk snapshot2]
                            }
                      }
            -- NOTE: We deliberately expect the transaction creation and
            -- submission code of the Chain.Direct module to fail here because
            -- the scripts don't validate. That is, the on-chain code prevented
            -- this from happening and NOT any off-chain guard we added. Also
            -- note, that we don't try to check whether it's failing for the
            -- right reason here (see corresponding mutation test for this).
            contestAgain `shouldThrow` \case
              (ScriptFailedInWallet{} :: PostTxError Tx) -> True
              _ -> False

data DirectChainTestLog
  = FromNode NodeLog
  | FromDirectChain Text CardanoChainLog
  | FromBlockfrostChain Text CardanoChainLog
  | FromFaucet FaucetLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CardanoChainTest tx m = CardanoChainTest
  { postTx :: PostChainTx tx -> m ()
  , waitCallback :: m (ChainEvent tx)
  }

-- | Wrapper around 'withDirectChain' that threads a 'ChainStateType tx' through
-- 'postTx' and 'waitCallback' calls.
withDirectChainTest ::
  Tracer IO CardanoChainLog ->
  ChainConfig ->
  Party ->
  (CardanoChainTest Tx IO -> IO a) ->
  IO a
withDirectChainTest tracer config party action = do
  (configuration, backend) <-
    case config of
      Cardano cfg@CardanoChainConfig{chainBackendOptions} ->
        case chainBackendOptions of
          Direct directOptions -> pure (cfg, DirectBackend directOptions)
          _ -> failure $ "unexpected chainBackendOptions: " <> show chainBackendOptions
      otherConfig -> failure $ "unexpected chainConfig: " <> show otherConfig
  ctx <- loadChainContext backend configuration party
  eventMVar <- newLabelledEmptyTMVarIO "direct-chain-events"

  let callback event = atomically $ putTMVar eventMVar event

  wallet <- mkTinyWallet backend tracer configuration
  withDirectChain backend tracer configuration ctx wallet (initHistory initialChainState) callback $ \Chain{postTx} -> do
    action
      CardanoChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        }

hasInitTxWith :: (HasCallStack, IsTx tx) => HeadParameters -> [OnChainId] -> OnChainTx tx -> IO (HeadId, HeadSeed)
hasInitTxWith HeadParameters{contestationPeriod = expectedContestationPeriod, parties = expectedParties} expectedParticipants = \case
  OnInitTx{headId, headSeed, headParameters = HeadParameters{contestationPeriod, parties}, participants} -> do
    expectedParticipants `shouldMatchList` participants
    expectedContestationPeriod `shouldBe` contestationPeriod
    expectedParties `shouldMatchList` parties
    pure (headId, headSeed)
  tx -> failure ("Unexpected observation: " <> show tx)

observesInTime :: IsTx tx => CardanoChainTest tx IO -> OnChainTx tx -> IO ()
observesInTime chain expected =
  observesInTimeSatisfying chain (`shouldBe` expected)

observesInTimeSatisfying :: CardanoChainTest tx IO -> (OnChainTx tx -> IO a) -> IO a
observesInTimeSatisfying directChainTest = observesInTimeSatisfying' directChainTest 10

observesInTime' :: IsTx tx => CardanoChainTest tx IO -> OnChainTx tx -> IO ()
observesInTime' chain expected =
  observesInTimeSatisfying' chain 200 (`shouldBe` expected)

observesInTimeSatisfying' :: CardanoChainTest tx IO -> NominalDiffTime -> (OnChainTx tx -> IO a) -> IO a
observesInTimeSatisfying' CardanoChainTest{waitCallback} waitTime check =
  failAfter waitTime go
 where
  go = do
    e <- waitCallback
    case e of
      Observation{observedTx} ->
        check observedTx
      _TickOrRollback ->
        go

waitMatch :: CardanoChainTest tx IO -> (ChainEvent tx -> Maybe b) -> IO b
waitMatch CardanoChainTest{waitCallback} match = go
 where
  go = do
    a <- waitCallback
    maybe go pure (match a)

delayUntil :: (MonadDelay m, MonadTime m) => UTCTime -> m ()
delayUntil target = do
  now <- getCurrentTime
  threadDelay . realToFrac $ diffUTCTime target now

-- | Load key files for given 'Actor's (see keysFor) and directly convert them to 'OnChainId'.
loadParticipants :: [Actor] -> IO [OnChainId]
loadParticipants actors =
  forM actors $ \a -> do
    (vk, _) <- keysFor a
    pure $ verificationKeyToOnChainId vk
