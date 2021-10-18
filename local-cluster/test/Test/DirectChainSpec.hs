{-# LANGUAGE TypeApplications #-}

module Test.DirectChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (RunningCluster (RunningCluster), testClusterConfig, withCluster)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (
  Chain (..),
  HeadParameters (HeadParameters),
  OnChainTx (OnAbortTx, OnInitTx),
  PostChainTx (AbortTx, InitTx),
 )
import Hydra.Chain.Direct (NetworkMagic (NetworkMagic), withDirectChain, withIOManager)
import Hydra.Chain.Direct.Wallet (generateKeyPair)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)
import System.FilePath ((</>))

spec :: Spec
spec = around showLogsOnFailure $ do
  it "can init and abort a head given nothing has been committed" $ \tracer -> do
    calledBackAlice <- newEmptyMVar
    calledBackBob <- newEmptyMVar
    let magic = NetworkMagic 42
    withTempDir "hydra-local-cluster" $ \tmp -> do
      let config = ClusterConfig tmp
      withCluster tracer config $ \cluster@(RunningCluster _ _) -> do
        aliceKeys <- keysForAlice cluster
        bobKeys <- keysForBob cluster
        withIOManager $ \iocp -> do
          let node1socket = tmp </> "node1" </> "node.socket"
          let node2socket = tmp </> "node2" </> "node.socket"
          withDirectChain nullTracer magic iocp node1socket aliceKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
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

keysForAlice :: RunningCluster -> IO (VerificationKey, SigningKey)
keysForAlice (RunningCluster (ClusterConfig directory) _) = error "read keys from file"

keysForBob :: RunningCluster -> IO (VerificationKey, SigningKey)
keysForBob (RunningCluster (ClusterConfig directory) _) = error "read keys from file"

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30
