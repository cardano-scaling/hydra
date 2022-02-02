{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (
  buildAddress,
  generatePaymentToCommit,
  postSeedPayment,
  queryProtocolParameters,
  queryUTxO,
  waitForUTxO,
 )
import CardanoCluster (
  ClusterLog,
  availableInitialFunds,
  defaultNetworkId,
  keysFor,
  newNodeConfig,
  withBFTNode,
 )
import CardanoNode (NodeLog, RunningNode (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Hydra.Cardano.Api (
  NetworkId (Testnet),
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
  NetworkMagic (NetworkMagic),
  withDirectChain,
  withIOManager,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (CardanoTx, genOneUTxOFor)
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
      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \(RunningNode _ nodeSocket) -> do
        bobKeys <- keysFor "bob"
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        cardanoKeys <- fmap fst <$> mapM keysFor ["alice", "bob", "carol"]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer magic iocp nodeSocket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \_ -> do
              postSeedPayment (Testnet magic) pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000

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
      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \node@(RunningNode _ nodeSocket) -> do
        bobKeys <- keysFor "bob"
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        cardanoKeys <- fmap fst <$> mapM keysFor ["alice", "bob", "carol"]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer magic iocp nodeSocket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \_ -> do
              postSeedPayment (Testnet magic) pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000

              postTx $ InitTx $ HeadParameters 100 [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx 100 [alice, bob, carol]

              let aliceCommitment = 66_000_000
              aliceUTxO <- generatePaymentToCommit defaultNetworkId node aliceCardanoSk aliceCardanoVk aliceCommitment
              postTx $ CommitTx alice aliceUTxO

              alicesCallback `observesInTime` OnCommitTx alice aliceUTxO
              bobsCallback `observesInTime` OnCommitTx alice aliceUTxO

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx
              bobsCallback `observesInTime` OnAbortTx

              let networkId = Testnet magic
                  aliceAddress = buildAddress aliceCardanoVk networkId

              -- Expect that alice got her committed value back
              utxo <- queryUTxO networkId nodeSocket [aliceAddress]
              let aliceValues = txOutValue <$> toList utxo
              aliceValues `shouldContain` [lovelaceToValue aliceCommitment]

  it "cannot abort a non-participating head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \(RunningNode _ nodeSocket) -> do
        bobKeys <- keysFor "bob"
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        let cardanoKeys = []
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            withDirectChain nullTracer magic iocp nodeSocket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \Chain{postTx = bobPostTx} -> do
              postSeedPayment (Testnet magic) pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000
              alicePostTx $ InitTx $ HeadParameters 100 [alice, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, carol]

              bobPostTx (AbortTx mempty)
                `shouldThrow` (== InvalidStateToPost @CardanoTx (AbortTx mempty))

  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \node@(RunningNode _ nodeSocket) -> do
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postSeedPayment (Testnet magic) pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000

            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUTxOA <- generate $ genOneUTxOFor aliceCardanoVk
            someUTxOB <- generate $ genOneUTxOFor aliceCardanoVk

            postTx (CommitTx alice (someUTxOA <> someUTxOB))
              `shouldThrow` (== MoreThanOneUTxOCommitted @CardanoTx)

            postTx (CommitTx alice someUTxOA)
              `shouldThrow` \case
                (CannotSpendInput{} :: PostTxError CardanoTx) -> True
                _ -> False

            aliceUTxO <- generatePaymentToCommit defaultNetworkId node aliceCardanoSk aliceCardanoVk 1_000_000
            postTx $ CommitTx alice aliceUTxO
            alicesCallback `observesInTime` OnCommitTx alice aliceUTxO

  it "can commit empty UTxO" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \(RunningNode _ nodeSocket) -> do
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postSeedPayment (Testnet magic) pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000

            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            postTx $ CommitTx alice mempty
            alicesCallback `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \node@(RunningNode _ nodeSocket) -> do
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postSeedPayment (Testnet magic) pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000

            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUTxO <- generatePaymentToCommit defaultNetworkId node aliceCardanoSk aliceCardanoVk 1_000_000
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

magic :: NetworkMagic
magic = NetworkMagic 42

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
