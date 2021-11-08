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
import Hydra.Chain.Direct.WalletSpec (genPaymentTo)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.Party (Party, deriveParty, generateKey)
import Test.QuickCheck (generate)

spec :: Spec
spec = do
  it "can init and abort a head given nothing has been committed" $ do
    calledBackAlice <- newEmptyMVar
    calledBackBob <- newEmptyMVar
    aliceKeys@(aliceVk, _) <- generateKeyPair
    bobKeys@(bobVk, _) <- generateKeyPair
    withMockServer $ \networkMagic iocp socket submitTx -> do
      let cardanoKeys = [] -- TODO(SN): this should matter
      withDirectChain nullTracer networkMagic iocp socket aliceKeys alice cardanoKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
        withDirectChain nullTracer networkMagic iocp socket bobKeys bob cardanoKeys (putMVar calledBackBob) $ \_ -> do
          let parameters = HeadParameters 100 [alice, bob, carol]
          generate (genPaymentTo aliceVk) >>= submitTx
          generate (genPaymentTo bobVk) >>= submitTx
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

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30
