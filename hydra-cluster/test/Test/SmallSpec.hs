{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Test.SmallSpec where

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
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
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
  ChainEvent (Observation, Tick),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct (
  IntersectionNotFoundException,
  withDirectChain,
  withIOManager,
 )
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Chain.Direct.ScriptRegistry (queryScriptRegistry)
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
  defaultNetworkId,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Crypto (aggregate, sign)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genOneUTxOFor)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Options (
  ChainConfig (..),
  toArgNetworkId,
 )
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import System.Process (proc, readCreateProcess)
import Test.QuickCheck (generate)

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
          hydraScriptsTxId <- publishHydraScriptsAs node Faucet
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing hydraScriptsTxId (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters cperiod [alice]
            alicesCallback `observesInTime` OnInitTx cperiod [alice]

            aliceUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)
            threadDelay 10
            postTx $ CommitTx alice aliceUTxO

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        cardanoKeys <- fmap fst <$> mapM keysFor [Alice, Bob, Carol]
        withIOManager $ \iocp -> do
          seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
          hydraScriptsTxId <- publishHydraScriptsAs node Faucet
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing hydraScriptsTxId (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
            alicesCallback `observesInTime` OnInitTx cperiod [alice, bob, carol]

            let aliceCommitment = 66_000_000
            aliceUTxO <- seedFromFaucet node aliceCardanoVk aliceCommitment Normal (contramap FromFaucet tracer)
            threadDelay 10
            postTx $ CommitTx alice aliceUTxO

  it "can open, close & fanout a Head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel (contramap FromFaucet tracer)
          hydraScriptsTxId <- publishHydraScriptsAs node Faucet
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing hydraScriptsTxId (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters cperiod [alice]
            alicesCallback `observesInTime` OnInitTx cperiod [alice]

            someUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal (contramap FromFaucet tracer)
            threadDelay 10
            postTx $ CommitTx alice someUTxO

data TestClusterLog
  = FromNode NodeLog
  | FromDirectChain Text DirectChainLog
  | FromFaucet FaucetLog
  deriving (Show, Generic, ToJSON)

observesInTime :: IsTx tx => MVar (ChainEvent tx) -> OnChainTx tx -> Expectation
observesInTime mvar expected =
  failAfter 10 go
 where
  go = do
    e <- takeMVar mvar
    case e of
      Observation obs -> obs `shouldBe` expected
      _ -> go

waitMatch :: MVar a -> (a -> Maybe b) -> IO b
waitMatch mvar match = do
  a <- takeMVar mvar
  case match a of
    Nothing -> waitMatch mvar match
    Just b -> pure b

isIntersectionNotFoundException :: IntersectionNotFoundException -> Bool
isIntersectionNotFoundException _ = True

delayUntil :: (MonadDelay m, MonadTime m) => UTCTime -> m ()
delayUntil target = do
  now <- getCurrentTime
  threadDelay . realToFrac $ diffUTCTime target now
