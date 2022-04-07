{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (
  buildAddress,
  queryTip,
  queryUTxO,
  waitForUTxO,
 )
import CardanoCluster (
  Actor (Alice, Bob, Carol),
  ClusterLog,
  Marked (Fuel, Normal),
  defaultNetworkId,
  keysFor,
  newNodeConfig,
  seedFromFaucet,
  seedFromFaucet_,
  withBFTNode,
 )
import CardanoNode (NodeLog, RunningNode (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Hydra.Cardano.Api (
  lovelaceToValue,
  txOutValue,
 )
import Hydra.Chain (
  Chain (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (..),
 )
import Hydra.Chain.Direct (
  DirectChainLog,
  withDirectChain,
  withIOManager,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (Tx, genOneUTxOFor)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Party (Party, SigningKey, aggregate, deriveParty, generateKey, sign)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.QuickCheck (generate)

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        bobKeys <- keysFor Bob
        cardanoKeys <- fmap fst <$> mapM keysFor [Alice, Bob, Carol]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer defaultNetworkId iocp nodeSocket bobKeys bob cardanoKeys Nothing (putMVar bobsCallback) $ \_ -> do
              seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel

              postTx $ InitTx $ HeadParameters 100 [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx 100 [alice, bob, carol]

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx
              bobsCallback `observesInTime` OnAbortTx

  it "can init and abort a 2-parties head after one party has committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        bobKeys <- keysFor Bob
        cardanoKeys <- fmap fst <$> mapM keysFor [Alice, Bob, Carol]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer defaultNetworkId iocp nodeSocket bobKeys bob cardanoKeys Nothing (putMVar bobsCallback) $ \_ -> do
              seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel

              postTx $ InitTx $ HeadParameters 100 [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx 100 [alice, bob, carol]

              let aliceCommitment = 66_000_000
              aliceUTxO <- seedFromFaucet defaultNetworkId node aliceCardanoVk aliceCommitment Normal
              postTx $ CommitTx alice aliceUTxO

              alicesCallback `observesInTime` OnCommitTx alice aliceUTxO
              bobsCallback `observesInTime` OnCommitTx alice aliceUTxO

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx
              bobsCallback `observesInTime` OnAbortTx

              let aliceAddress = buildAddress aliceCardanoVk defaultNetworkId

              -- Expect that alice got her committed value back
              utxo <- queryUTxO defaultNetworkId nodeSocket [aliceAddress]
              let aliceValues = txOutValue <$> toList utxo
              aliceValues `shouldContain` [lovelaceToValue aliceCommitment]

  it "cannot abort a non-participating head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      (carolCardanoVk, _) <- keysFor Carol
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        bobKeys <- keysFor Bob
        let cardanoKeys = [aliceCardanoVk, carolCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            withDirectChain nullTracer defaultNetworkId iocp nodeSocket bobKeys bob cardanoKeys Nothing (putMVar bobsCallback) $ \Chain{postTx = bobPostTx} -> do
              seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel

              alicePostTx $ InitTx $ HeadParameters 100 [alice, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, carol]

              bobPostTx (AbortTx mempty)
                `shouldThrow` (== InvalidStateToPost @Tx (AbortTx mempty))

  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel

            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUTxOA <- generate $ genOneUTxOFor aliceCardanoVk
            someUTxOB <- generate $ genOneUTxOFor aliceCardanoVk

            postTx (CommitTx alice (someUTxOA <> someUTxOB))
              `shouldThrow` (== MoreThanOneUTxOCommitted @Tx)

            postTx (CommitTx alice someUTxOA)
              `shouldThrow` \case
                (CannotSpendInput{} :: PostTxError Tx) -> True
                _ -> False

            aliceUTxO <- seedFromFaucet defaultNetworkId node aliceCardanoVk 1_000_000 Normal
            postTx $ CommitTx alice aliceUTxO
            alicesCallback `observesInTime` OnCommitTx alice aliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel

            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            postTx $ CommitTx alice mempty
            alicesCallback `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx} -> do
            seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel

            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUTxO <- seedFromFaucet defaultNetworkId node aliceCardanoVk 1_000_000 Normal
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
                , signatures = aggregate [sign aliceSigningKey snapshot]
                }

            alicesCallback `shouldSatisfyInTime` \case
              OnCloseTx{snapshotNumber} ->
                -- FIXME(SN): should assert contestationDeadline > current
                snapshotNumber == 1
              _ ->
                False

            postTx $
              FanoutTx
                { utxo = someUTxO
                }
            alicesCallback `observesInTime` OnFanoutTx
            failAfter 5 $
              waitForUTxO defaultNetworkId nodeSocket someUTxO

  it "cannot restart head to point in the past and replay on-chain events" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "direct-chain" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, _) <- keysFor Alice
      withBFTNode (contramap FromCluster tracer) config $ \node@(RunningNode _ nodeSocket) -> do
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          tip <- withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys Nothing (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel
            tip <- queryTip defaultNetworkId nodeSocket
            alicePostTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]
            return tip

          withDirectChain (contramap (FromDirectChain "alice") tracer) defaultNetworkId iocp nodeSocket aliceKeys alice cardanoKeys (Just tip) (putMVar alicesCallback) $ \_ -> do
            alicesCallback `observesInTime` OnInitTx 100 [alice]

alice, bob, carol :: Party
alice = deriveParty aliceSigningKey
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

aliceSigningKey :: SigningKey
aliceSigningKey = generateKey 10

data TestClusterLog
  = FromCluster ClusterLog
  | FromNode NodeLog
  | FromDirectChain Text DirectChainLog
  deriving (Show)

observesInTime :: IsTx tx => MVar (OnChainTx tx) -> OnChainTx tx -> Expectation
observesInTime mvar expected =
  failAfter 10 $
    takeMVar mvar `shouldReturn` expected

shouldSatisfyInTime :: Show a => MVar a -> (a -> Bool) -> Expectation
shouldSatisfyInTime mvar f =
  failAfter 10 $
    takeMVar mvar >>= flip shouldSatisfy f
