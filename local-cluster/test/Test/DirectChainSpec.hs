{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (ClusterLog, RunningCluster (..), keysFor, testClusterConfig, withCluster)
import CardanoNode (RunningNode (..))
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

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    bobsCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      let config = testClusterConfig tmp
      withCluster (contramap FromCluster tracer) config $ \cluster@(RunningCluster _ [RunningNode _ node1socket, RunningNode _ node2socket, _]) -> do
        aliceKeys <- keysFor "alice" cluster
        bobKeys <- keysFor "bob" cluster
        let cardanoKeys = []
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp node1socket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            withDirectChain nullTracer magic iocp node2socket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \_ -> do
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
      let config = testClusterConfig tmp
      withCluster (contramap FromCluster tracer) config $ \cluster@(RunningCluster _ [RunningNode _ node1socket, RunningNode _ node2socket, _]) -> do
        aliceKeys <- keysFor "alice" cluster
        bobKeys <- keysFor "bob" cluster
        let cardanoKeys = []
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp node1socket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx = alicePostTx} -> do
            withDirectChain nullTracer magic iocp node2socket bobKeys bob cardanoKeys (putMVar bobsCallback) $ \Chain{postTx = bobPostTx} -> do
              alicePostTx $ InitTx @SimpleTx $ HeadParameters 100 [alice, carol]
              alicesCallback `observesInTime` OnInitTx 100 [alice, carol]

              bobPostTx $ AbortTx @SimpleTx mempty
              bobsCallback `observesInTime` PostTxFailed

  it "can commit" $ \tracer -> do
    alicesCallback <- newEmptyMVar
    withTempDir "hydra-local-cluster" $ \tmp -> do
      let config = testClusterConfig tmp
      withCluster (contramap FromCluster tracer) config $ \cluster@(RunningCluster _ [RunningNode _ node1socket, _, _]) -> do
        aliceKeys@(aliceCardanoVk, _) <- keysFor "alice" cluster
        let cardanoKeys = [aliceCardanoVk]
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp node1socket aliceKeys alice cardanoKeys (putMVar alicesCallback) $ \Chain{postTx} -> do
            postTx $ InitTx @SimpleTx $ HeadParameters 100 [alice]
            alicesCallback `observesInTime` OnInitTx 100 [alice]

            -- NOTE(SN): We are committing a SimpleTX UTXO, which is fine as
            -- long there are no on-chain validators checking it
            let someUtxo = utxoRef 42
            postTx $ CommitTx alice someUtxo
            alicesCallback `observesInTime` OnCommitTx alice someUtxo

magic :: NetworkMagic
magic = NetworkMagic 42

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

data TestClusterLog
  = FromCluster ClusterLog
  | FromDirectChain Text (DirectChainLog SimpleTx)
  deriving (Show)

observesInTime :: Tx tx => MVar (OnChainTx tx) -> OnChainTx tx -> Expectation
observesInTime mvar expected =
  failAfter 10 $
    takeMVar mvar `shouldReturn` expected
