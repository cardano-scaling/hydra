{-# LANGUAGE TypeApplications #-}

-- | Integration tests for the Direct chain component which does interact with a
-- single mocked "node". For tests spinning up real cardano-nodes see
-- 'EndToEndSpec'.
module Hydra.Chain.DirectSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Chain (
  Chain (..),
  HeadParameters (HeadParameters),
  OnChainTx (OnAbortTx, OnInitTx),
  PostChainTx (AbortTx, InitTx),
 )
import Hydra.Chain.Direct (withDirectChain)
import Hydra.Chain.Direct.MockServer (withMockServer)
import Hydra.Chain.Direct.Wallet (generateKeyPair)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.Party (Party, deriveParty, generateKey)

spec :: Spec
spec = parallel $ do
  it "publishes init tx and observes it also" $ do
    calledBackAlice <- newEmptyMVar
    calledBackBob <- newEmptyMVar
    aliceKeys <- generateKeyPair
    bobKeys <- generateKeyPair

    withMockServer $ \networkMagic iocp socket _ -> do
      withDirectChain nullTracer networkMagic iocp socket aliceKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
        withDirectChain nullTracer networkMagic iocp socket bobKeys (putMVar calledBackBob) $ \_ -> do
          let parameters = HeadParameters 100 [alice, bob, carol]
          postTx $ InitTx @SimpleTx parameters
          failAfter 5 $
            takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]
          failAfter 5 $
            takeMVar calledBackBob `shouldReturn` OnInitTx @SimpleTx 100 [alice, bob, carol]

  it "can init and abort a head given nothing has been committed" $ do
    calledBackAlice <- newEmptyMVar
    calledBackBob <- newEmptyMVar
    aliceKeys <- generateKeyPair
    bobKeys <- generateKeyPair
    withMockServer $ \networkMagic iocp socket _ -> do
      withDirectChain nullTracer networkMagic iocp socket aliceKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
        withDirectChain nullTracer networkMagic iocp socket bobKeys (putMVar calledBackBob) $ \_ -> do
          let parameters = HeadParameters 100 [alice, bob, carol]
          postTx $ InitTx @SimpleTx parameters
          failAfter 5 $
            takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]

          postTx $ AbortTx mempty

          failAfter 5 $
            takeMVar calledBackBob `shouldReturn` OnAbortTx @SimpleTx

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30
