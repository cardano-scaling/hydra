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
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8 as B8
import Hydra.Cardano.Api (
  ChainPoint (..),
  lovelaceToValue,
  txOutValue,
  unsafeDeserialiseFromRawBytesBase16,
 )
import Hydra.Chain (
  Chain (..),
  ChainEvent (Observation),
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
import Hydra.Cluster.Faucet (
  Marked (Fuel, Normal),
  seedFromFaucet,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (Alice, Bob, Carol),
  alice,
  aliceSk,
  bob,
  carol,
  cperiod,
  defaultNetworkId,
 )
import Hydra.Cluster.Util (keysFor)
import Hydra.Crypto (aggregate, sign)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genOneUTxOFor)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.QuickCheck (generate)

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        bobKeys <- keysFor Bob
        cardanoKeys <- fmap fst <$> mapM keysFor [Alice, Bob, Carol]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer defaultNetworkId iocp nodeSocket bobKeys bob cardanoKeys Nothing (putMVar bobsCallback) $ \_ -> do
              seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel

              postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx cperiod [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx cperiod [alice, bob, carol]

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx
              bobsCallback `observesInTime` OnAbortTx

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        bobKeys <- keysFor Bob
        cardanoKeys <- fmap fst <$> mapM keysFor [Alice, Bob, Carol]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer defaultNetworkId iocp nodeSocket bobKeys bob cardanoKeys Nothing (putMVar bobsCallback) $ \_ -> do
              seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel

              postTx $ InitTx $ HeadParameters cperiod [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx cperiod [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx cperiod [alice, bob, carol]

              let aliceCommitment = 66_000_000
              aliceUTxO <- seedFromFaucet node aliceCardanoVk aliceCommitment Normal
              postTx $ CommitTx alice aliceUTxO

              alicesCallback `observesInTime` OnCommitTx alice aliceUTxO
              bobsCallback `observesInTime` OnCommitTx alice aliceUTxO

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx
              bobsCallback `observesInTime` OnAbortTx

              let aliceAddress = buildAddress aliceCardanoVk defaultNetworkId

              -- Expect that alice got her committed value back
              utxo <- queryUTxO defaultNetworkId nodeSocket QueryTip [aliceAddress]
              let aliceValues = txOutValue <$> toList utxo
              aliceValues `shouldContain` [lovelaceToValue aliceCommitment]

  it "cannot abort a non-participating head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      (carolCardanoVk, _) <- keysFor Carol
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        bobKeys <- keysFor Bob
        let cardanoKeys = [aliceCardanoVk, carolCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            withDirectChain nullTracer defaultNetworkId iocp nodeSocket bobKeys bob cardanoKeys Nothing (putMVar bobsCallback) $ \Chain{postTx = bobPostTx} -> do
              seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel

              alicePostTx $ InitTx $ HeadParameters cperiod [alice, carol]
              alicesCallback `observesInTime` OnInitTx cperiod [alice, carol]

              bobPostTx (AbortTx mempty)
                `shouldThrow` (== InvalidStateToPost @Tx (AbortTx mempty))

  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel

            postTx $ InitTx $ HeadParameters cperiod [alice]
            alicesCallback `observesInTime` OnInitTx cperiod [alice]

            someUTxOA <- generate $ genOneUTxOFor aliceCardanoVk
            someUTxOB <- generate $ genOneUTxOFor aliceCardanoVk

            postTx (CommitTx alice (someUTxOA <> someUTxOB))
              `shouldThrow` (== MoreThanOneUTxOCommitted @Tx)

            postTx (CommitTx alice someUTxOA)
              `shouldThrow` \case
                (CannotSpendInput{} :: PostTxError Tx) -> True
                _ -> False

            aliceUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal
            postTx $ CommitTx alice aliceUTxO
            alicesCallback `observesInTime` OnCommitTx alice aliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel

            postTx $ InitTx $ HeadParameters cperiod [alice]
            alicesCallback `observesInTime` OnInitTx cperiod [alice]

            postTx $ CommitTx alice mempty
            alicesCallback `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel

            postTx $ InitTx $ HeadParameters cperiod [alice]
            alicesCallback `observesInTime` OnInitTx cperiod [alice]

            someUTxO <- seedFromFaucet node aliceCardanoVk 1_000_000 Normal
            postTx $ CommitTx alice someUTxO
            alicesCallback `observesInTime` OnCommitTx alice someUTxO

            postTx $ CollectComTx someUTxO
            alicesCallback `observesInTime` OnCollectComTx

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

            fanoutDelay <-
              alicesCallback `shouldSatisfyInTime` \case
                Observation OnCloseTx{snapshotNumber, remainingContestationPeriod} ->
                  -- FIXME(SN): should assert contestationDeadline > current
                  Just (snapshotNumber == 1, remainingContestationPeriod)
                _ ->
                  Nothing

            threadDelay (toEnum (fromEnum fanoutDelay))
            postTx $
              FanoutTx
                { utxo = someUTxO
                }
            alicesCallback `observesInTime` OnFanoutTx
            failAfter 5 $
              waitForUTxO defaultNetworkId nodeSocket someUTxO

  it "can restart head to point in the past and replay on-chain events" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "direct-chain" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \node@RunningNode{nodeSocket} -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          tip <- withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel
            tip <- queryTip defaultNetworkId nodeSocket
            alicePostTx $ InitTx $ HeadParameters cperiod [alice]
            alicesCallback `observesInTime` OnInitTx cperiod [alice]
            return tip

          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys (Just tip) (putMVar alicesCallback) $ \_ -> do
            alicesCallback `observesInTime` OnInitTx cperiod [alice]

  it "cannot restart head to an unknown point" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "direct-chain" $ \tmp -> do
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromNode tracer) tmp $ \RunningNode{nodeSocket} -> do
        let aliceTrace = contramap (FromDirectChain "alice") tracer
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          let headerHash = unsafeDeserialiseFromRawBytesBase16 (B8.replicate 64 '0')
          let fakeTip = ChainPoint 42 headerHash
          flip shouldThrow isIntersectionNotFoundException $
            withDirectChain aliceTrace defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys (Just fakeTip) (putMVar alicesCallback) $ \_ -> do
              threadDelay 5 >> fail "should not execute main action but did?"

data TestClusterLog
  = FromNode NodeLog
  | FromDirectChain Text DirectChainLog
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

shouldSatisfyInTime :: MVar a -> (a -> Maybe (Bool, b)) -> IO b
shouldSatisfyInTime mvar f =
  failAfter 10 $ do
    a <- takeMVar mvar
    case f a of
      Nothing ->
        fail "predicate failed."
      Just (predicate, b) -> do
        shouldSatisfy predicate identity
        return b

isIntersectionNotFoundException :: IntersectionNotFoundException -> Bool
isIntersectionNotFoundException _ = True
