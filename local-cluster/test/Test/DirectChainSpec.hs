{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (
  ClusterLog,
  keysFor,
  newNodeConfig,
  withBFTNode,
 )
import CardanoNode (NodeLog, RunningNode (..))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (
  Chain (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
 )
import Hydra.Chain.Direct (
  DirectChainLog,
  NetworkMagic (NetworkMagic),
  withDirectChain,
  withIOManager,
 )
import Hydra.Ledger (Tx)
import Hydra.Ledger.Simple (SimpleTx, utxoRef)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)
import Hydra.Snapshot (Snapshot (..))

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
              postTx $ InitTx @SimpleTx $ HeadParameters 100 [alice, bob, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, bob, carol]
              bobsCallback `observesInTime` OnInitTx 100 [alice, bob, carol]

              postTx $ AbortTx mempty

              alicesCallback `observesInTime` OnAbortTx @SimpleTx
              bobsCallback `observesInTime` OnAbortTx @SimpleTx

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
              alicePostTx $ InitTx @SimpleTx $ HeadParameters 100 [alice, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, carol]

              bobPostTx $ AbortTx @SimpleTx mempty
              bobsCallback `observesInTime` PostTxFailed

  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
        aliceKeys@(aliceCardanoVk, _) <- keysFor "alice"
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx @SimpleTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            -- NOTE(SN): We are committing a SimpleTX UTXO, which is fine as
            -- long there are no on-chain validators checking it
            let someUtxo = utxoRef 42
            postTx $ CommitTx alice someUtxo
            alicesCallback `observesInTime` OnCommitTx alice someUtxo

  it "can open, close & fanout a Head" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      config <- newNodeConfig tmp
      withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
        aliceKeys@(aliceCardanoVk, _) <- keysFor "alice"
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp nodeSocket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx @SimpleTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            -- NOTE(SN): We are committing a SimpleTX UTXO, which is fine as
            -- long there are no on-chain validators checking it
            let someUtxo = utxoRef 42
            postTx $ CommitTx alice someUtxo
            alicesCallback `observesInTime` OnCommitTx alice someUtxo

            postTx $ CollectComTx someUtxo
            alicesCallback `observesInTime` OnCollectComTx

            -- NOTE(SN): This is deliberately wrong and should be caught by OCV
            postTx . CloseTx $
              Snapshot
                { number = 1
                , utxo = utxoRef 123
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
                { utxo = utxoRef 123
                }
            alicesCallback `observesInTime` OnFanoutTx

magic :: NetworkMagic
magic = NetworkMagic 42

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

data TestClusterLog
  = FromCluster ClusterLog
  | FromNode NodeLog
  | FromDirectChain Text (DirectChainLog SimpleTx)
  deriving (Show)

observesInTime :: Tx tx => MVar (OnChainTx tx) -> OnChainTx tx -> Expectation
observesInTime mvar expected =
  failAfter 10 $
    takeMVar mvar `shouldReturn` expected

shouldSatisfyInTime :: Show a => MVar a -> (a -> Bool) -> Expectation
shouldSatisfyInTime mvar f =
  failAfter 10 $
    takeMVar mvar >>= flip shouldSatisfy f
