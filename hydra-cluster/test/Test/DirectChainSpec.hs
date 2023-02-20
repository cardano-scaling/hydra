{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (
  QueryPoint (QueryTip),
  buildAddress,
  queryTip,
  queryUTxO,
  waitForUTxO,
 )
import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
import Control.Concurrent.STM (newEmptyTMVarIO, newTVarIO, readTVarIO, takeTMVar)
import Control.Concurrent.STM.TMVar (putTMVar)
import Control.Concurrent.STM.TVar (writeTVar)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Hydra.Cardano.Api (
  ChainPoint (..),
  lovelaceToValue,
  txOutValue,
  unsafeDeserialiseFromRawBytesBase16,
 )
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct (
  IntersectionNotFoundException (..),
  initialChainState,
  loadChainContext,
  mkTinyWallet,
  withDirectChain,
 )
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
import Hydra.Chain.Direct.State (ChainContext, ctxParties, genHydraContextFor)
import Hydra.Cluster.Faucet (
  FaucetLog,
  Marked (Fuel, Normal),
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
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genOneUTxOFor)
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
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Bob, Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob hydraScriptsTxId
            hydraCtx' <- generate $ genHydraContextFor (length $ cardanoVerificationKeys bobChainConfig)
            let parties' = ctxParties hydraCtx'
            withDirectChainTest nullTracer bobChainConfig bobChainContext parties' $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]
                bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]

                postTx $ AbortTx mempty

                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Bob, Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob hydraScriptsTxId
            hydraCtx' <- generate $ genHydraContextFor (length $ cardanoVerificationKeys bobChainConfig)
            let parties' = ctxParties hydraCtx'
            withDirectChainTest (contramap (FromDirectChain "bob") tracer) bobChainConfig bobChainContext parties' $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                let aliceCommitment = 66_000_000
                aliceUTxO <- seedFromFaucet node aliceCardanoVk aliceCommitment Normal (contramap FromFaucet tracer)

                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]
                bobChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, bob, carol]

                postTx $ CommitTx alice aliceUTxO

                aliceChain `observesInTime` OnCommitTx alice aliceUTxO
                bobChain `observesInTime` OnCommitTx alice aliceUTxO

                postTx $ AbortTx aliceUTxO

                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

                let aliceAddress = buildAddress aliceCardanoVk networkId

                -- Expect that alice got her committed value back
                utxo <- queryUTxO networkId nodeSocket QueryTip [aliceAddress]
                let aliceValues = txOutValue <$> toList utxo
                aliceValues `shouldContain` [lovelaceToValue aliceCommitment]

  it "cannot abort a non-participating head" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Carol] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId

        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx = alicePostTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Carol] cperiod
            bobChainContext <- loadChainContext bobChainConfig bob hydraScriptsTxId

            hydraCtx' <- generate $ genHydraContextFor (length $ cardanoVerificationKeys bobChainConfig)
            let parties' = ctxParties hydraCtx'
            withDirectChainTest nullTracer bobChainConfig bobChainContext parties' $
              \DirectChainTest{postTx = bobPostTx} -> do
                -- Scenario
                alicePostTx $ InitTx $ HeadParameters cperiod [alice, carol]
                aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice, carol]

                bobPostTx (AbortTx mempty)
                  `shouldThrow` \case
                    InvalidStateToPost{txTried} -> txTried == AbortTx @Tx mempty
                    _ -> False

  it "can commit" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            aliceUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            someUTxOA <- generate $ genOneUTxOFor aliceCardanoVk
            someUTxOB <- generate $ genOneUTxOFor aliceCardanoVk

            postTx (CommitTx alice (someUTxOA <> someUTxOB))
              `shouldThrow` (== MoreThanOneUTxOCommitted @Tx)

            postTx (CommitTx alice someUTxOA)
              `shouldThrow` \case
                (InternalWalletError{} :: PostTxError Tx) -> True
                _ -> False

            postTx $ CommitTx alice aliceUTxO
            aliceChain `observesInTime` OnCommitTx alice aliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            postTx $ CommitTx alice mempty
            aliceChain `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            someUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            postTx $ CommitTx alice someUTxO
            aliceChain `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx someUTxO
            aliceChain `observesInTime` OnCollectComTx

            let snapshot =
                  Snapshot
                    { number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    }

            postTx . CloseTx $
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
              failure $ "contestationDeadline in the past: " <> show deadline <> ", now: " <> show now
            delayUntil deadline

            waitMatch aliceChain $ \case
              Tick t | t > deadline -> Just ()
              _ -> Nothing
            postTx $
              FanoutTx
                { utxo = someUTxO
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
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId

        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        -- Scenario
        tip <- withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            tip <- queryTip networkId nodeSocket
            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]
            pure tip

        let aliceChainConfig' = aliceChainConfig{startChainFrom = Just tip}
        hydraCtx' <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig')
        let parties' = ctxParties hydraCtx'
        -- REVIEW: It's a bit weird now that we would use the original chain
        -- state here. Does this test even make sense with persistence?
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig' aliceChainContext parties' $
          \aliceChain@DirectChainTest{} ->
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

  it "cannot restart head to an unknown point" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet

        let headerHash = unsafeDeserialiseFromRawBytesBase16 (B8.replicate 64 '0')
        let fakeTip = ChainPoint 42 headerHash
        aliceChainConfig <-
          chainConfigFor Alice tmp nodeSocket [] cperiod
            <&> \cfg -> cfg{startChainFrom = Just fakeTip}
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        let action = withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $ \_ ->
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
                ( "publish-scripts" :
                  mconcat
                    [ ["--node-socket", nodeSocket]
                    , ["--network-id", toArgNetworkId networkId]
                    , ["--cardano-signing-key", cardanoSigningKey]
                    ]
                )
            )
            ""
        let hydraScriptsTxId =
              let removeTrailingNewline = BS.init
               in unsafeDeserialiseFromRawBytesBase16
                    (removeTrailingNewline (encodeUtf8 hydraScriptsTxIdStr))
        failAfter 5 $ void $ queryScriptRegistry networkId nodeSocket hydraScriptsTxId

  it "can only contest once" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [] cperiod
        aliceChainContext <- loadChainContext aliceChainConfig alice hydraScriptsTxId
        hydraCtx <- generate $ genHydraContextFor (length $ cardanoVerificationKeys aliceChainConfig)
        let parties = ctxParties hydraCtx
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainContext parties $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            someUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTimeSatisfying` hasInitTxWith cperiod [alice]

            postTx $ CommitTx alice someUTxO
            aliceChain `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx someUTxO
            aliceChain `observesInTime` OnCollectComTx
            -- Head is open with someUTxO

            -- Alice close with the initial snapshot U0
            postTx $ CloseTx InitialSnapshot{initialUTxO = someUTxO}
            waitMatch aliceChain $ \case
              Observation{observedTx = OnCloseTx{snapshotNumber}}
                | snapshotNumber == 0 -> Just ()
              _ -> Nothing

            -- Alice contests with some snapshot U1 -> successful
            let snapshot1 =
                  Snapshot
                    { number = 1
                    , utxo = someUTxO
                    , confirmed = []
                    }
            postTx . ContestTx $
              ConfirmedSnapshot
                { snapshot = snapshot1
                , signatures = aggregate [sign aliceSk snapshot1]
                }
            aliceChain `observesInTime` OnContestTx{snapshotNumber = 1}

            -- Alice contests with some snapshot U2 -> expect fail
            let snapshot2 =
                  Snapshot
                    { number = 2
                    , utxo = someUTxO
                    , confirmed = []
                    }
            let contestAgain =
                  postTx . ContestTx $
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
  deriving (Show, Generic, ToJSON)

data DirectChainTest tx m = DirectChainTest
  { postTx :: PostChainTx tx -> m ()
  , waitCallback :: m (ChainEvent tx)
  }

-- | Wrapper around 'withDirectChain' that threads a 'ChainStateType tx' through
-- 'postTx' and 'waitCallback' calls.
withDirectChainTest ::
  Tracer IO DirectChainLog ->
  ChainConfig ->
  ChainContext ->
  [Party] ->
  (DirectChainTest Tx IO -> IO a) ->
  IO a
withDirectChainTest tracer config ctx otherParties action = do
  eventMVar <- newEmptyTMVarIO
  stateVar <- newTVarIO initialChainState

  let callback = \cont -> do
        cs <- readTVarIO stateVar
        case cont cs of
          Nothing -> pure ()
          Just ev -> atomically $ do
            putTMVar eventMVar ev
            case ev of
              Observation{newChainState} -> writeTVar stateVar newChainState
              _OtherEvent -> pure ()

  wallet <- mkTinyWallet tracer config

  withDirectChain tracer config ctx Nothing wallet otherParties callback $ \Chain{postTx} -> do
    action
      DirectChainTest
        { postTx = \tx -> do
            cs <- readTVarIO stateVar
            postTx cs tx
        , waitCallback = atomically $ takeTMVar eventMVar
        }

hasInitTxWith :: (HasCallStack, IsTx tx) => ContestationPeriod -> [Party] -> OnChainTx tx -> Expectation
hasInitTxWith expectedContestationPeriod expectedParties = \case
  OnInitTx{contestationPeriod, parties} -> do
    expectedContestationPeriod `shouldBe` contestationPeriod
    expectedParties `shouldBe` parties
  tx -> failure ("Unexpected observation: " <> show tx)

observesInTime :: IsTx tx => DirectChainTest tx IO -> OnChainTx tx -> IO ()
observesInTime chain expected =
  observesInTimeSatisfying chain (`shouldBe` expected)

observesInTimeSatisfying :: DirectChainTest tx IO -> (OnChainTx tx -> Expectation) -> IO ()
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
