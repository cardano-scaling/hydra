{-# LANGUAGE DuplicateRecordFields #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.Api (bodyTxL, reqSignerHashesTxBodyL)
import CardanoClient (
  QueryPoint (QueryTip),
  RunningNode (..),
  buildAddress,
  queryTip,
  queryUTxO,
  submitTx,
  waitForUTxO,
 )
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Concurrent.STM (newEmptyTMVarIO, takeTMVar)
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Lens ((<>~))
import Data.Set qualified as Set
import Hydra.Cardano.Api (
  ChainPoint (..),
  CtxUTxO,
  Key (SigningKey),
  PaymentKey,
  TxOut,
  UTxO',
  fromLedgerTx,
  lovelaceToValue,
  signTx,
  toLedgerKeyHash,
  toLedgerTx,
  txOutValue,
  unFile,
  verificationKeyHash,
 )
import Hydra.Chain (
  Chain (Chain, draftCommitTx, postTx),
  ChainEvent (..),
  CommitBlueprintTx (..),
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
import Hydra.Chain.Direct.State (initialChainState, splitUTxO)
import Hydra.Chain.Direct.Tx (verificationKeyToOnChainId)
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
import Hydra.Crypto (aggregate, sign)
import Hydra.HeadId (HeadId, HeadSeed (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genKeyPair)
import Hydra.Logging (Tracer, nullTracer, showLogsOnFailure)
import Hydra.OnChainId (OnChainId)
import Hydra.Options (
  ChainConfig (..),
  DirectChainConfig (..),
  toArgNetworkId,
 )
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcess)
import Test.QuickCheck (choose, generate)

spec :: Spec
spec = around (showLogsOnFailure "DirectChainSpec") $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [Bob, Carol] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $ \aliceChain@DirectChainTest{postTx} -> do
          -- Bob setup
          bobChainConfig <- chainConfigFor Bob tmp nodeSocket hydraScriptsTxId [Alice, Carol] cperiod
          withDirectChainTest nullTracer bobChainConfig bob $ \bobChain@DirectChainTest{} -> do
            -- Scenario
            participants <- loadParticipants [Alice, Bob, Carol]
            let headParameters = HeadParameters cperiod [alice, bob, carol]
            postTx $ InitTx{participants, headParameters}
            (aliceHeadId, aliceHeadSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants
            (bobHeadId, bobHeadSeed) <- bobChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            aliceHeadSeed `shouldBe` bobHeadSeed

            postTx $ AbortTx{utxo = mempty, headSeed = aliceHeadSeed}

            aliceChain `observesInTime` OnAbortTx{headId = aliceHeadId}
            bobChain `observesInTime` OnAbortTx{headId = bobHeadId}

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [Bob, Carol] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket hydraScriptsTxId [Alice, Carol] cperiod
            withDirectChainTest (contramap (FromDirectChain "bob") tracer) bobChainConfig bob $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                let aliceCommitment = 66_000_000
                -- Mimic "external commit" by using different keys for Alice.
                (aliceExternalVk, aliceExternalSk) <- generate genKeyPair

                aliceUTxO <- seedFromFaucet node aliceExternalVk aliceCommitment (contramap FromFaucet tracer)

                participants <- loadParticipants [Alice, Bob, Carol]
                let headParameters = HeadParameters cperiod [alice, bob, carol]
                postTx $ InitTx{participants, headParameters}
                (aliceHeadId, aliceHeadSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants
                (bobHeadId, bobHeadSeed) <- bobChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

                aliceHeadSeed `shouldBe` bobHeadSeed

                externalCommit node aliceChain aliceExternalSk aliceHeadId aliceUTxO

                aliceChain `observesInTime` OnCommitTx aliceHeadId alice aliceUTxO
                bobChain `observesInTime` OnCommitTx bobHeadId alice aliceUTxO

                postTx $ AbortTx{utxo = aliceUTxO, headSeed = aliceHeadSeed}
                --
                aliceChain `observesInTime` OnAbortTx{headId = aliceHeadId}
                bobChain `observesInTime` OnAbortTx{headId = bobHeadId}

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
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod

        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx = alicePostTx} -> do
            -- Bob setup
            (bobCardanoVk, _) <- keysFor Bob
            seedFromFaucet_ node bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket hydraScriptsTxId [] cperiod

            withDirectChainTest nullTracer bobChainConfig bob $
              \bobChain@DirectChainTest{postTx = bobPostTx} -> do
                -- Scenario
                aliceParticipants <- loadParticipants [Carol, Alice]
                let headParameters = HeadParameters cperiod [alice, carol]
                alicePostTx $ InitTx{participants = aliceParticipants, headParameters}
                void $ aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters aliceParticipants

                -- Expect bob's chain layer to see the init of alice and carols exlusive head
                headSeed <- snd <$> bobChain `observesInTimeSatisfying` hasInitTxWith headParameters aliceParticipants
                -- headSeed2 <- generate arbitrary

                bobPostTx AbortTx{utxo = mempty, headSeed}
                  `shouldThrow` \case
                    -- Note: We expect bob to be able to construct the abort tx but failing to submit
                    FailedToConstructAbortTx @Tx -> False
                    ScriptFailedInWallet{} -> True
                    _ -> False

  it "can commit using external wallet" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            aliceUTxO <- seedFromFaucet node aliceExternalVk 1_000_000 (contramap FromFaucet tracer)
            externalCommit node aliceChain aliceExternalSk headId aliceUTxO

            aliceChain `observesInTime` OnCommitTx headId alice aliceUTxO

  it "can commit using internal hydra-node wallet" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            aliceUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 (contramap FromFaucet tracer)
            externalCommit node aliceChain aliceCardanoSk headId aliceUTxO

            aliceChain `observesInTime` OnCommitTx headId alice aliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            (_, aliceExternalSk) <- generate genKeyPair
            externalCommit node aliceChain aliceExternalSk headId mempty
            aliceChain `observesInTime` OnCommitTx headId alice mempty

  it "can commit with multiple required signatures" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            newAliceUTxO <- seedFromFaucet node aliceExternalVk 3_000_000 (contramap FromFaucet tracer)

            numberOfKeyWits <- generate $ choose (2, 10)
            randomKeys <- generate $ replicateM numberOfKeyWits genKeyPair

            let blueprintTx =
                  fromLedgerTx
                    ( toLedgerTx (txSpendingUTxO newAliceUTxO)
                        & bodyTxL . reqSignerHashesTxBodyL
                          <>~ Set.fromList (toLedgerKeyHash . verificationKeyHash . fst <$> randomKeys)
                    )

            externalCommit' node aliceChain (aliceExternalSk : fmap snd randomKeys) headId newAliceUTxO blueprintTx
            aliceChain `observesInTime` OnCommitTx headId alice newAliceUTxO

  it "can open, close & fanout a Head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet node aliceExternalVk 1_000_000 (contramap FromFaucet tracer)
            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            (headId, headSeed) <- aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            externalCommit node aliceChain aliceExternalSk headId someUTxO
            aliceChain `observesInTime` OnCommitTx headId alice someUTxO

            postTx $ CollectComTx someUTxO headId headParameters
            aliceChain `observesInTime` OnCollectComTx{headId}
            let (inHead, toDecommit) = splitUTxO someUTxO

            let snapshot =
                  Snapshot
                    { headId
                    , number = 1
                    , utxo = inHead
                    , confirmed = []
                    , utxoToDecommit = Just toDecommit
                    }

            postTx . CloseTx headId headParameters $
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
                { utxo = inHead
                , utxoToDecommit = Just toDecommit
                , headSeed
                , contestationDeadline = deadline
                }
            aliceChain `observesInTime` OnFanoutTx headId
            failAfter 5 $
              waitForUTxO node (inHead <> toDecommit)

  it "can restart head to point in the past and replay on-chain events" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        participants <- loadParticipants [Alice]
        let headParameters = HeadParameters cperiod [alice]
        -- Scenario
        tip <- withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            tip <- queryTip networkId nodeSocket
            postTx $ InitTx{participants, headParameters}
            void $ aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants
            pure tip

        let aliceChainConfig' = aliceChainConfig & modifyConfig (\cfg -> cfg{startChainFrom = Just tip})
        -- REVIEW: It's a bit weird now that we would use the original chain
        -- state here. Does this test even make sense with persistence?
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig' alice $
          \aliceChain@DirectChainTest{} ->
            void $ aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

  it "cannot restart head to an unknown point" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet

        let headerHash = fromString (replicate 64 '0')
        let fakeTip = ChainPoint 42 headerHash
        aliceChainConfig <-
          chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
            <&> modifyConfig (\cfg -> cfg{startChainFrom = Just fakeTip})
        let action = withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $ \_ ->
              threadDelay 5 >> fail "should not execute main action but did?"

        action `shouldThrow` \case
          IntersectionNotFound{} -> True

  it "can publish and query reference scripts in a timely manner" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \RunningNode{nodeSocket, networkId} -> do
        readConfigFile ("credentials" </> "faucet.sk") >>= writeFileBS (tmp </> "faucet.sk")
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
        let hydraScriptsTxId = fromString hydraScriptsTxIdStr
        failAfter 5 $ void $ queryScriptRegistry networkId nodeSocket hydraScriptsTxId

  it "can only contest once" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket hydraScriptsTxId [] cperiod
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig alice $
          \aliceChain@DirectChainTest{postTx} -> do
            (aliceExternalVk, aliceExternalSk) <- generate genKeyPair
            someUTxO <- seedFromFaucet node aliceExternalVk 1_000_000 (contramap FromFaucet tracer)

            participants <- loadParticipants [Alice]
            let headParameters = HeadParameters cperiod [alice]
            postTx $ InitTx{participants, headParameters}
            headId <- fst <$> aliceChain `observesInTimeSatisfying` hasInitTxWith headParameters participants

            externalCommit node aliceChain aliceExternalSk headId someUTxO
            aliceChain `observesInTime` OnCommitTx headId alice someUTxO

            postTx $ CollectComTx someUTxO headId headParameters
            -- Head is open with someUTxO
            aliceChain `observesInTime` OnCollectComTx headId

            -- Alice close with the initial snapshot U0
            postTx $ CloseTx headId headParameters InitialSnapshot{headId, initialUTxO = someUTxO}
            deadline <- waitMatch aliceChain $ \case
              Observation{observedTx = OnCloseTx{snapshotNumber, contestationDeadline}}
                | snapshotNumber == 0 -> Just contestationDeadline
              _ -> Nothing

            -- Alice contests with some snapshot U1 -> successful
            let snapshot1 =
                  Snapshot
                    { headId
                    , number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    , utxoToDecommit = Nothing
                    }
            postTx . ContestTx headId headParameters $
              ConfirmedSnapshot
                { snapshot = snapshot1
                , signatures = aggregate [sign aliceSk snapshot1]
                }
            aliceChain `observesInTime` OnContestTx{headId, snapshotNumber = 1, contestationDeadline = deadline}

            -- Alice contests with some snapshot U2 -> expect fail
            let snapshot2 =
                  Snapshot
                    { headId
                    , number = 2
                    , utxo = someUTxO
                    , confirmed = []
                    , utxoToDecommit = Nothing
                    }
            let contestAgain =
                  postTx . ContestTx headId headParameters $
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
  , draftCommitTx :: HeadId -> UTxOType tx -> tx -> m tx
  }

-- | Wrapper around 'withDirectChain' that threads a 'ChainStateType tx' through
-- 'postTx' and 'waitCallback' calls.
withDirectChainTest ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  Party ->
  (DirectChainTest Tx IO -> IO a) ->
  IO a
withDirectChainTest tracer config party action = do
  directConfig <- case config of
    Direct cfg -> pure cfg
    otherConfig -> failure $ "unexpected chainConfig: " <> show otherConfig
  ctx <- loadChainContext directConfig party
  eventMVar <- newEmptyTMVarIO

  let callback event = atomically $ putTMVar eventMVar event

  wallet <- mkTinyWallet tracer directConfig

  withDirectChain tracer directConfig ctx wallet (initHistory initialChainState) callback $ \Chain{postTx, draftCommitTx} -> do
    action
      DirectChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        , draftCommitTx = \headId utxo blueprintTx -> do
            eTx <- draftCommitTx headId $ CommitBlueprintTx{lookupUTxO = utxo, blueprintTx}
            case eTx of
              Left e -> throwIO e
              Right tx -> pure tx
        }

hasInitTxWith :: (HasCallStack, IsTx tx) => HeadParameters -> [OnChainId] -> OnChainTx tx -> IO (HeadId, HeadSeed)
hasInitTxWith HeadParameters{contestationPeriod = expectedContestationPeriod, parties = expectedParties} expectedParticipants = \case
  OnInitTx{headId, headSeed, headParameters = HeadParameters{contestationPeriod, parties}, participants} -> do
    expectedParticipants `shouldMatchList` participants
    expectedContestationPeriod `shouldBe` contestationPeriod
    expectedParties `shouldMatchList` parties
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
externalCommit node hydraClient externalSk headId utxoToCommit = do
  let blueprintTx = txSpendingUTxO utxoToCommit
  externalCommit' node hydraClient [externalSk] headId utxoToCommit blueprintTx

externalCommit' ::
  RunningNode ->
  DirectChainTest Tx IO ->
  [SigningKey PaymentKey] ->
  HeadId ->
  UTxO' (TxOut CtxUTxO) ->
  Tx ->
  IO ()
externalCommit' node hydraClient externalSks headId utxoToCommit blueprintTx = do
  commitTx <- draftCommitTx headId utxoToCommit blueprintTx
  let signedTx = everybodySigns commitTx externalSks
  submitTx node signedTx
 where
  everybodySigns tx' [] = tx'
  everybodySigns tx' (sk : sks) = everybodySigns (signTx sk tx') sks

  DirectChainTest{draftCommitTx} = hydraClient

-- | Load key files for given 'Actor's (see keysFor) and directly convert them to 'OnChainId'.
loadParticipants :: [Actor] -> IO [OnChainId]
loadParticipants actors =
  forM actors $ \a -> do
    (vk, _) <- keysFor a
    pure $ verificationKeyToOnChainId vk
