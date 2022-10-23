{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (
  QueryException (QueryAcquireException),
  QueryPoint (QueryTip),
  buildAddress,
  queryTip,
  queryUTxO,
  waitForUTxO,
 )
import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (tryReadMVar)
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
  ChainStateType,
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct (initialChainState, withDirectChain)
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
import Hydra.Chain.Direct.State ()
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
import Hydra.Crypto (aggregate, sign)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genOneUTxOFor)
import Hydra.Logging (Tracer, nullTracer, showLogsOnFailure)
import Hydra.Options (
  ChainConfig (..),
  toArgNetworkId,
 )
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (
  AcquireFailure (AcquireFailurePointNotOnChain),
 )
import System.Process (proc, readCreateProcess)
import Test.QuickCheck (generate)

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice, Bob, Carol]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Bob, Carol]
            bobChainState <- initialChainState bobChainConfig bob hydraScriptsTxId
            withDirectChainTest nullTracer bobChainConfig bobChainState $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                (aliceCardanoVk, _) <- keysFor Alice
                seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceChain `observesInTime` OnInitTx cperiod [alice, bob, carol]
                bobChain `observesInTime` OnInitTx cperiod [alice, bob, carol]

                postTx $ AbortTx mempty

                aliceChain `observesInTime` OnAbortTx
                bobChain `observesInTime` OnAbortTx

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice, Bob, Carol]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Bob setup
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Bob, Carol]
            bobChainState <- initialChainState bobChainConfig bob hydraScriptsTxId
            withDirectChainTest (contramap (FromDirectChain "bob") tracer) bobChainConfig bobChainState $
              \bobChain@DirectChainTest{} -> do
                -- Scenario
                (aliceCardanoVk, _) <- keysFor Alice
                seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

                let aliceCommitment = 66_000_000
                -- REVIEW(SN): there is still some things unclear (why the seed needs to be here and not further down, after withDirectChain).
                aliceUTxO <- seedFromFaucet node aliceCardanoVk aliceCommitment Normal (contramap FromFaucet tracer)

                postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
                aliceChain `observesInTime` OnInitTx cperiod [alice, bob, carol]
                bobChain `observesInTime` OnInitTx cperiod [alice, bob, carol]

                postTx $ CommitTx alice aliceUTxO

                aliceChain `observesInTime` OnCommitTx alice aliceUTxO
                bobChain `observesInTime` OnCommitTx alice aliceUTxO

                postTx $ AbortTx mempty

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
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice, Carol]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx = alicePostTx} -> do
            -- Bob setup
            -- REVIEW: check if all three keys were used by Bob here
            bobChainConfig <- chainConfigFor Bob tmp nodeSocket [Alice, Bob, Carol]
            bobChainState <- initialChainState bobChainConfig bob hydraScriptsTxId
            withDirectChainTest nullTracer bobChainConfig bobChainState $
              \DirectChainTest{postTx = bobPostTx} -> do
                -- Scenario
                (aliceCardanoVk, _) <- keysFor Alice
                seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

                alicePostTx $ InitTx $ HeadParameters cperiod [alice, carol]
                aliceChain `observesInTime` OnInitTx cperiod [alice, carol]

                bobPostTx (AbortTx mempty)
                  `shouldThrow` (== InvalidStateToPost @Tx (AbortTx mempty))

  it "can commit" $ \tracer ->
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            -- REVIEW: check whether using these here (not before direct chain) works now
            (aliceCardanoVk, _) <- keysFor Alice
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
            aliceUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTime` OnInitTx cperiod [alice]

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
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            (aliceCardanoVk, _) <- keysFor Alice
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTime` OnInitTx cperiod [alice]

            postTx $ CommitTx alice mempty
            aliceChain `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    withTempDir "hydra-cluster" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket, networkId} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        -- Alice setup
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx} -> do
            -- Scenario
            (aliceCardanoVk, _) <- keysFor Alice
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
            someUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)

            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTime` OnInitTx cperiod [alice]

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
        aliceChainConfig <- chainConfigFor Alice tmp nodeSocket [Alice]
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        -- Scenario
        tip <- withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $
          \aliceChain@DirectChainTest{postTx} -> do
            tip <- queryTip networkId nodeSocket
            postTx $ InitTx $ HeadParameters cperiod [alice]
            aliceChain `observesInTime` OnInitTx cperiod [alice]
            pure tip

        let aliceChainConfig' = aliceChainConfig{startChainFrom = Just tip}
        -- REVIEW: It's a bit weird now that we would use the original chain
        -- state here. Does this test even make sense with persistence?
        withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig' aliceChainState $
          \aliceChain@DirectChainTest{} ->
            aliceChain `observesInTime` OnInitTx cperiod [alice]

  it "cannot restart head to an unknown point" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet

        let headerHash = unsafeDeserialiseFromRawBytesBase16 (B8.replicate 64 '0')
        let fakeTip = ChainPoint 42 headerHash
        aliceChainConfig <-
          chainConfigFor Alice tmp nodeSocket [Alice]
            <&> \cfg -> cfg{startChainFrom = Just fakeTip}
        aliceChainState <- initialChainState aliceChainConfig alice hydraScriptsTxId
        let action = withDirectChainTest (contramap (FromDirectChain "alice") tracer) aliceChainConfig aliceChainState $ \_ ->
              threadDelay 5 >> fail "should not execute main action but did?"

        action `shouldThrow` \case
          QueryAcquireException err -> err == AcquireFailurePointNotOnChain
          _ -> False

  it "can publish and query reference scripts in a timely manner" $ \tracer -> do
    withTempDir "direct-chain" $ \tmp -> do
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \RunningNode{nodeSocket, networkId} -> do
        DirectChainConfig{cardanoSigningKey} <- chainConfigFor Faucet tmp nodeSocket []
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

data TestClusterLog
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
  ChainStateType Tx ->
  (DirectChainTest Tx IO -> IO a) ->
  IO a
withDirectChainTest tracer config initialState action = do
  mvar <- newEmptyMVar

  let getState =
        tryReadMVar mvar >>= \case
          Just Observation{newChainState} -> pure $ newChainState
          _nothingOrOtherEvent -> pure $ initialState

  let callback = \cont -> do
        cs <- getState
        case cont cs of
          Nothing -> pure ()
          Just ev -> putMVar mvar ev

  withDirectChain tracer config callback $ \Chain{postTx} -> do
    action
      DirectChainTest
        { postTx = \tx -> do
            cs <- getState
            postTx cs tx
        , waitCallback = takeMVar mvar
        }

observesInTime :: IsTx tx => DirectChainTest tx IO -> OnChainTx tx -> IO ()
observesInTime DirectChainTest{waitCallback} expected =
  failAfter 10 go
 where
  go = do
    e <- waitCallback
    case e of
      Observation{observedTx} -> do
        observedTx `shouldBe` expected
      _TickOrRollback -> go

waitMatch :: DirectChainTest tx IO -> (ChainEvent tx -> Maybe b) -> IO b
waitMatch DirectChainTest{waitCallback} match = go
 where
  go = do
    a <- waitCallback
    case match a of
      Nothing -> go
      Just b -> pure b

delayUntil :: (MonadDelay m, MonadTime m) => UTCTime -> m ()
delayUntil target = do
  now <- getCurrentTime
  threadDelay . realToFrac $ diffUTCTime target now
