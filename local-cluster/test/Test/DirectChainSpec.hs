{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.Keys (VKey (VKey))
import CardanoClient (
  generatePaymentToCommit,
  waitForPayment,
 )
import CardanoCluster (
  ClusterLog,
  defaultNetworkId,
  keysFor,
  newNodeConfig,
  withBFTNode,
 )
import CardanoNode (NodeLog, RunningNode (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (
  Chain (..),
  HeadParameters (..),
  InvalidTxError (..),
  OnChainTx (..),
  PostChainTx (..),
 )
import Hydra.Chain.Direct (
  DirectChainLog,
  NetworkMagic (NetworkMagic),
  withDirectChain,
  withIOManager,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (
  AddressInEra (..),
  AddressTypeInEra (ShelleyAddressInEra),
  CardanoTx,
  ShelleyBasedEra (ShelleyBasedEraAlonzo),
  TxOut (..),
  VerificationKey (PaymentVerificationKey),
  genOneUtxoFor,
  selectLovelace,
  utxoPairs,
 )
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)
import Hydra.Snapshot (Snapshot (..))
import Test.QuickCheck (generate)
import qualified Prelude

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
        aliceKeys <- keysFor "alice"
        bobKeys <- keysFor "bob"
        let cardanoKeys = []
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer magic iocp nodeSocket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \_ -> do
              postTx $ InitTx $ HeadParameters 100 [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx 100 [alice, bob, carol]

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx
              bobsCallback `observesInTime` OnAbortTx

  it "cannot abort a non-participating head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
        aliceKeys <- keysFor "alice"
        bobKeys <- keysFor "bob"
        let cardanoKeys = []
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            withDirectChain nullTracer magic iocp nodeSocket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \Chain{postTx = bobPostTx} -> do
              alicePostTx $ InitTx $ HeadParameters 100 [alice, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, carol]

              bobPostTx $ AbortTx mempty
              bobsCallback `observesInTime` PostTxFailed

  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \node@(RunningNode _ nodeSocket) -> do
        aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUtxoA <- generate $ genOneUtxoFor (PaymentVerificationKey $ VKey aliceCardanoVk)
            someUtxoB <- generate $ genOneUtxoFor (PaymentVerificationKey $ VKey aliceCardanoVk)

            postTx (CommitTx alice (someUtxoA <> someUtxoB))
              `shouldThrow` (== MoreThanOneUtxoCommitted @CardanoTx)

            postTx (CommitTx alice someUtxoA)
              `shouldThrow` \case
                (CannotSpendInput{} :: InvalidTxError CardanoTx) -> True
                _ -> False

            aliceUtxo <- generatePaymentToCommit defaultNetworkId node aliceCardanoSk aliceCardanoVk 1_000_000
            postTx $ CommitTx alice aliceUtxo
            alicesCallback `observesInTime` OnCommitTx alice aliceUtxo

  it "can commit empty UTxO" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
        aliceKeys@(aliceCardanoVk, _) <- keysFor "alice"
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            postTx $ CommitTx alice mempty
            alicesCallback `observesInTime` OnCommitTx alice mempty

  it "can open, close & fanout a Head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \node@(RunningNode _ nodeSocket) -> do
        aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUtxo <- generatePaymentToCommit defaultNetworkId node aliceCardanoSk aliceCardanoVk 1_000_000
            postTx $ CommitTx alice someUtxo
            alicesCallback `observesInTime` OnCommitTx alice someUtxo

            postTx $ CollectComTx someUtxo
            alicesCallback `observesInTime` OnCollectComTx

            postTx . CloseTx $
              Snapshot
                { number = 1
                , utxo = someUtxo
                , confirmed = []
                }
            alicesCallback `shouldSatisfyInTime` \case
              OnCloseTx{snapshotNumber} ->
                -- FIXME(SN): should assert contestationDeadline > current
                snapshotNumber == 1
              _ ->
                False

            postTx $
              FanoutTx
                { utxo = someUtxo
                }
            alicesCallback `observesInTime` OnFanoutTx
            failAfter 5 $
              case snd $ Prelude.head $ utxoPairs someUtxo of
                TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraAlonzo) fanoutAddr) _ _ ->
                  void $
                    waitForPayment
                      defaultNetworkId
                      nodeSocket
                      (selectLovelace $ balance @CardanoTx someUtxo)
                      fanoutAddr
                txOut -> failure $ "Unexpected TxOut " <> show txOut

magic :: NetworkMagic
magic = NetworkMagic 42

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

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
