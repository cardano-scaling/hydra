{-# LANGUAGE DuplicateRecordFields #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO (UTxO' (UTxO, toMap))
import CardanoClient (
  QueryPoint (QueryTip),
  buildAddress,
  queryTip,
  queryUTxO,
  submitTx,
  waitForUTxO,
 )
import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
import Control.Concurrent.STM (newEmptyTMVarIO, takeTMVar)
import Control.Concurrent.STM.TMVar (putTMVar)
import Hydra.Cardano.Api (
  ChainPoint (..),
  CtxUTxO,
  Key (SigningKey),
  KeyWitnessInCtx (KeyWitnessForSpending),
  PaymentKey,
  TxOut,
  WitCtxTxIn,
  Witness,
  lovelaceToValue,
  signTx,
  txOutValue,
  unFile,
  pattern KeyWitness,
 )
import Hydra.Chain (
  Chain (Chain, draftCommitTx, postTx),
  ChainEvent (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
  initHistory,
 )
import Hydra.Chain.Direct (
  IntersectionNotFoundException (..),
  loadChainContext,
  mkTinyWallet,
  withDirectChain,
 )
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
import Hydra.Chain.Direct.State (ChainContext (..), initialChainState)
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
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (aggregate, sign)
import Hydra.HeadId (HeadId, HeadSeed (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genKeyPair)
import Hydra.Logging (Tracer, nullTracer, showLogsOnFailure)
import Hydra.Options (
  ChainConfig (..),
  toArgNetworkId,
 )
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import System.Process (proc, readCreateProcess)
import Test.QuickCheck (generate)

spec :: Spec
spec = around (showLogsOnFailure "DirectChainSpec") $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Bob, Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [bob, carol] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob [alice, carol] hydraScriptsTxId
            withDirectChainTest nullTracer bobChainConfig bobChainContext $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceHeadSeed <- snd <$> aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]
                bobHeadSeed <- snd <$> bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]

                aliceHeadSeed `shouldBe` bobHeadSeed

                postTx $ AbortTx{utxo = mempty, headSeed = aliceHeadSeed}

                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Bob, Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [bob, carol] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob [alice, carol] hydraScriptsTxId
            withDirectChainTest (contramap (FromDirectChain "bob") tracer) bobChainConfig bobChainContext $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                let aliceCommitment = 66_000_000
                -- Mimic "external commit" by using different keys for Alice.
                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair

                aliceUTxO <- seedFromFaucet node aliceExternalVk aliceCommitment (contramap FromFaucet tracer)

                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                (headId, aliceHeadSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]
                bobHeadSeed <- snd <$> bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]

                aliceHeadSeed `shouldBe` bobHeadSeed

                externalCommit node aliceChain aliceExternalSk headId aliceUTxO

                aliceChain `observesInTime` OnCommitTx alice aliceUTxO
                bobChain `observesInTime` OnCommitTx alice aliceUTxO

                postTx $ AbortTx{utxo = aliceUTxO, headSeed = aliceHeadSeed}
                --
                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

                let aliceExternalAddress = buildAddress aliceExternalVk networkId

                -- Expect that Alice got her committed value back to her
                -- external address
                utxo <- queryUTxO networkId nodeSocket QueryTip [aliceExternalAddress]
                let aliceValues = txOutValue <$> toList utxo
                aliceValues `shouldContain` [lovelaceToValue aliceCommitment]

  it "cannot abort a non-participating head" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [carol] hydraScriptsTxId

        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx = alicePostTx} -> do
            -- Bob setup
            (bobCardanoVk, _) <- keysFor Bob
            seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob [alice, carol] hydraScriptsTxId

            withDirectChainTest nullTracer bobChainConfig bobChainContext $
              \bobChain@DirectChainTest{postTx = bobPostTx} -> do
                -- Scenario
                alicePostTx $ InitTx $ HeadParameters cperiod [alice, carol]
                void $ aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, carol]

                -- Expect bob's chain layer to see the init of alice and carols exlusive head
                headSeed <- snd <$> bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, carol]

                bobPostTx AbortTx{utxo = mempty, headSeed}
                  `shouldThrow` \case
                    -- Note: We expect bob to be able to construct the abort tx but failing to submit
                    FailedToConstructAbortTx @Tx -> False
                    ScriptFailedInWallet{} -> True
                    _ -> False

  it "can commit" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            aliceUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]

            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]
            -- deliberately use alice's key known to hydra-node to trigger the error
            externalCommit node aliceChain aliceCardanoSk headId aliceUTxO
              `shouldThrow` \case
                (SpendingNodeUtxoForbidden :: PostTxError Tx) -> True
                _ -> False

            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            newAliceUTxO <- seedFromFaucet node aliceExternalVk 1_000_000 (contramap FromFaucet tracer)

            externalCommit node aliceChain aliceExternalSk headId newAliceUTxO
            aliceChain `observesInTime` OnCommitTx alice newAliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            postTx $ InitTx $ HeadParameters cperiod [alice]
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            (_, aliceExternalSk) <- generate genKeyPair
            externalCommit node aliceChain aliceExternalSk headId mempty
            aliceChain `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet node aliceExternalVk 1_000_000 (contramap FromFaucet tracer)
            let params = HeadParameters cperiod [alice]
            postTx $ InitTx params
            (headId, headSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            externalCommit node aliceChain aliceExternalSk headId someUTxO
            aliceChain `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx headId
            aliceChain `observesInTime` OnCollectComTx{headId}


            let snapshot =
                  Snapshot
                    { headId
                    , number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    }

            postTx . CloseTx headId $
              ConfirmedSnapshot
                { snapshot
                , signatures = aggregate [sign aliceSk snapshot]
                }

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
                { utxo = someUTxO
                , headSeed
                , contestationDeadline = deadline
                }
            aliceChain `observesInTime` OnFanoutTx
            failAfter 5 $
              waitForUTxO networkId nodeSocket someUTxO

  it "can restart head to point in the past and replay on-chain events" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId

        -- Scenario
        tip <- withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            tip <- queryTip networkId nodeSocket
            postTx $ InitTx $ HeadParameters cperiod [alice]
            void $ aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]
            pure tip

        let aliceChainConfig' = aliceChainConfig{startChainFrom = Just tip}
        -- REVIEW: It's a bit weird now that we would use the original chain
        -- state here. Does this test even make sense with persistence?
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig' aliceChainContext $
          \aliceChain@DirectChainTest{} ->
            void $ aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

  it "cannot restart head to an unknown point" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet

        let headerHash = fromString (replicate 64 '0')
        let fakeTip = ChainPoint 42 headerHash
        aliceChainConfig <-
          chainConfigFor Alice tmp nodeSocket [] cperiod
            <&> \cfg -> cfg{startChainFrom = Just fakeTip}
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        let action = withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $ \_ ->
              threadDelay 5 >> fail "should not execute main action but did?"

        action `shouldThrow` \case
          IntersectionNotFound{} -> True

  it "can publish and query reference scripts in a timely manner" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \RunningNode{nodeSocket, networkId} -> do
        DirectChainConfig{cardanoSigningKey} <- chainConfigFor Faucet tmp nodeSocket [] cperiod
        hydraScriptsTxIdStr <-
          readCreateProcess
            ( proc
                "hydra-node"
                ( "publish-scripts"
                    : mconcat
                      [ ["--node-socket", unFile nodeSocket]
                      , toArgNetworkId networkId
                      , ["--cardano-signing-key", cardanoSigningKey]
                      ]
                )
            )
            ""
        let hydraScriptsTxId = fromString hydraScriptsTxIdStr
        failAfter 5 $ void $ queryScriptRegistry networkId nodeSocket hydraScriptsTxId

  it "can only contest once" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice [] hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext $
          \aliceChain@DirectChainTest{postTx} -> do
            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet node aliceExternalVk 1_000_000 (contramap FromFaucet tracer)
            let params = HeadParameters cperiod [alice]
            postTx $ InitTx params
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            externalCommit node aliceChain aliceExternalSk headId someUTxO
            aliceChain `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx headId
            -- Head is open with someUTxO

            -- Alice close with the initial snapshot U0
            postTx $ CloseTx headId InitialSnapshot{headId, initialUTxO = someUTxO}
            waitMatch aliceChain $ \case
              Observation{observedTx = OnCloseTx{snapshotNumber}}
                | snapshotNumber == 0 -> Just ()
              _ -> Nothing

            -- Alice contests with some snapshot U1 -> successful
            let snapshot1 =
                  Snapshot
                    { headId
                    , number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    }
            postTx . ContestTx headId $
              ConfirmedSnapshot
                { snapshot = snapshot1
                , signatures = aggregate [sign aliceSk snapshot1]
                }
            aliceChain `observesInTime` OnContestTx{snapshotNumber = 1}

            -- Alice contests with some snapshot U2 -> expect fail
            let snapshot2 =
                  Snapshot
                    { headId
                    , number = 2
                    , utxo = someUTxO
                    , confirmed = []
                    }
            let contestAgain =
                  postTx . ContestTx headId $
                    ConfirmedSnapshot
                      { snapshot = snapshot2
                      , signatures = aggregate [sign aliceSk snapshot2]
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
  | FromDirectChain Text DirectChainLog
  | FromFaucet FaucetLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data DirectChainTest tx m = DirectChainTest
  { postTx :: PostChainTx tx -> m ()
  , waitCallback :: m (ChainEvent tx)
  , draftCommitTx :: UTxO' (TxOut CtxUTxO, Witness WitCtxTxIn) -> HeadId -> m tx
  }

-- | Wrapper around 'withDirectChain' that threads a 'ChainStateType tx' through
-- 'postTx' and 'waitCallback' calls.
withDirectChainTest ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  ChainContext ->
  (DirectChainTest Tx IO -> IO a) ->
  IO a
withDirectChainTest tracer config ctx action = do
  eventMVar <- newEmptyTMVarIO

  let callback event = atomically $ putTMVar eventMVar event

  wallet <- mkTinyWallet tracer config

  withDirectChain tracer config ctx wallet (initHistory initialChainState) callback $ \Chain{postTx, draftCommitTx} -> do
    action
      DirectChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        , draftCommitTx = \utxo headId -> do
            eTx <- draftCommitTx headId utxo
            case eTx of
              Left e -> throwIO e
              Right tx -> pure tx
        }

hasInitTxWith :: (HasCallStack, IsTx tx) => ContestationPeriod -> [Party] -> OnChainTx tx -> IO (HeadId, HeadSeed)
hasInitTxWith expectedContestationPeriod expectedParties = \case
  OnInitTx{headId, headSeed, contestationPeriod, parties} -> do
    expectedContestationPeriod `shouldBe` contestationPeriod
    expectedParties `shouldBe` parties
    pure (headId, headSeed)
  tx -> failure ("Unexpected observation: " <> show tx)

observesInTime :: IsTx tx => DirectChainTest tx IO -> OnChainTx tx -> IO ()
observesInTime chain expected =
  observesInTimeSatisfying chain (`shouldBe` expected)

observesInTimeSatisfying :: DirectChainTest tx IO -> (OnChainTx tx -> IO a) -> IO a
observesInTimeSatisfying DirectChainTest{waitCallback} check =
  failAfter 10 go
 where
  go = do
    e <- waitCallback
    case e of
      Observation{observedTx} ->
        check observedTx
      _TickOrRollback ->
        go

waitMatch :: DirectChainTest tx IO -> (ChainEvent tx -> Maybe b) -> IO b
waitMatch DirectChainTest{waitCallback} match = go
 where
  go = do
    a <- waitCallback
    maybe go pure (match a)

delayUntil :: (MonadDelay m, MonadTime m) => UTCTime -> m ()
delayUntil target = do
  now <- getCurrentTime
  threadDelay . realToFrac $ diffUTCTime target now

-- Commit using a wallet/external unknown to a hydra-node.
externalCommit ::
  RunningNode ->
  DirectChainTest Tx IO ->
  SigningKey PaymentKey ->
  HeadId ->
  UTxO' (TxOut CtxUTxO) ->
  IO ()
externalCommit node hydraClient externalSk headId utxoToCommit' = do
  let utxoToCommit =
        UTxO $ (,KeyWitness KeyWitnessForSpending) <$> toMap utxoToCommit'
  commitTx <- draftCommitTx utxoToCommit headId
  let signedTx = signTx externalSk commitTx
  submitTx node signedTx
 where
  DirectChainTest{draftCommitTx} = hydraClient
