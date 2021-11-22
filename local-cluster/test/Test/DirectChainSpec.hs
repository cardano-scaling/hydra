{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (UTxO (UTxO), shelleyAddressInEra)
import Cardano.Ledger.Keys (VKey (VKey))
import CardanoClient (build, buildAddress, lovelaceToTxOutValue, queryUtxo, sign, submit, waitForPayment)
import CardanoCluster (
  ClusterLog,
  keysFor,
  newNodeConfig,
  withBFTNode,
 )
import CardanoNode (NodeLog, RunningNode (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import qualified Data.Map as Map
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
import qualified Hydra.Chain.Direct.Util as Cardano
import Hydra.Ledger (IsTx)
import Hydra.Ledger.Cardano (
  CardanoTx,
  Lovelace (Lovelace),
  NetworkId (Testnet),
  SigningKey (PaymentSigningKey),
  TxOut (TxOut),
  TxOutDatum (TxOutDatumNone),
  Utxo,
  VerificationKey (PaymentVerificationKey),
  genOneUtxoFor,
  getVerificationKey,
 )
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)
import Hydra.Snapshot (Snapshot (..))
import Test.QuickCheck (generate)

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

            aliceUtxo <- generatePaymentToCommit node aliceCardanoSk aliceCardanoVk 1_000_000
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
      withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
        aliceKeys@(aliceCardanoVk, _) <- keysFor "alice"
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            someUtxo <- generate $ genOneUtxoFor (PaymentVerificationKey $ VKey aliceCardanoVk)
            postTx $ CommitTx alice someUtxo
            alicesCallback `observesInTime` OnCommitTx alice someUtxo

            postTx $ CollectComTx someUtxo
            alicesCallback `observesInTime` OnCollectComTx

            -- NOTE(SN): This is deliberately wrong and should be caught by OCV
            someOtherUtxo <- generate $ genOneUtxoFor (PaymentVerificationKey $ VKey aliceCardanoVk)
            postTx . CloseTx $
              Snapshot
                { number = 1
                , utxo = someOtherUtxo
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
                { utxo = someOtherUtxo
                }
            alicesCallback `observesInTime` OnFanoutTx

generatePaymentToCommit ::
  RunningNode ->
  Cardano.SigningKey ->
  Cardano.VerificationKey ->
  Natural ->
  IO Utxo
generatePaymentToCommit (RunningNode _ nodeSocket) sk vk lovelace = do
  UTxO availableUtxo <- queryUtxo networkId nodeSocket [spendingAddress]
  let inputs = (,Nothing) <$> Map.keys availableUtxo
  build networkId nodeSocket spendingAddress inputs [] [theOutput] >>= \case
    Left e -> error (show e)
    Right body -> do
      let tx = sign sk body
      submit networkId nodeSocket tx
      convertUtxo <$> waitForPayment networkId nodeSocket amountLovelace receivingAddress
 where
  networkId = Testnet magic

  spendingSigningKey = PaymentSigningKey sk

  receivingVerificationKey = PaymentVerificationKey $ VKey vk

  spendingAddress = buildAddress (getVerificationKey spendingSigningKey) networkId

  receivingAddress = buildAddress receivingVerificationKey networkId

  theOutput = TxOut (shelleyAddressInEra receivingAddress) (lovelaceToTxOutValue amountLovelace) TxOutDatumNone

  convertUtxo = error "convert utxo"

  amountLovelace = Lovelace $ fromIntegral lovelace

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
