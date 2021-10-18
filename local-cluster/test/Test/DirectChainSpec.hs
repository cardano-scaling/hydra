{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (ClusterConfig (..), ClusterLog, RunningCluster (..), testClusterConfig,keysFor, withCluster)
import CardanoNode (RunningNode (..))
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (
  Chain (..),
  HeadParameters (HeadParameters),
  OnChainTx (OnAbortTx, OnInitTx),
  PostChainTx (AbortTx, InitTx),
 )
import Hydra.Chain.Direct (
  DirectChainLog,
  NetworkMagic (NetworkMagic),
  withDirectChain,
  withIOManager,
 )
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    calledBackAlice <- newEmptyMVar
    calledBackBob <- newEmptyMVar
    let magic = NetworkMagic 42
    withTempDir "hydra-local-cluster" $ \tmp -> do
      let config = testClusterConfig tmp
      withCluster (contramap FromCluster tracer) config $ \cluster@(RunningCluster _ [RunningNode _ node1socket, RunningNode _ node2socket, _]) -> do
        aliceKeys <- keysFor "alice" cluster
        bobKeys <- keysFor "bob" cluster
        withIOManager $ \iocp -> do
          withDirectChain (contramap (FromDirectChain "alice") tracer) magic iocp node1socket aliceKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
            withDirectChain nullTracer magic iocp node2socket bobKeys (putMVar calledBackBob) $ \_ -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              threadDelay 2

              postTx $ InitTx @SimpleTx parameters
              failAfter 5 $
                takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]
              failAfter 5 $
                takeMVar calledBackBob `shouldReturn` OnInitTx 100 [alice, bob, carol]

              postTx $ AbortTx mempty

              failAfter 5 $
                takeMVar calledBackAlice `shouldReturn` OnAbortTx @SimpleTx
              failAfter 5 $
                takeMVar calledBackBob `shouldReturn` OnAbortTx @SimpleTx

data TestClusterLog
  = FromCluster ClusterLog
  | FromDirectChain Text (DirectChainLog SimpleTx)
  deriving (Show)

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30
