{-# LANGUAGE TypeApplications #-}

-- | Integration tests for the Direct chain component which does interact with a
-- single mocked "node". For tests spinning up real cardano-nodes see
-- 'EndToEndSpec'.
module Hydra.Chain.DirectSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall)
import Hydra.Chain (
  Chain (..),
  HeadParameters (HeadParameters),
  OnChainTx (OnAbortTx, OnInitTx),
  PostChainTx (AbortTx, InitTx),
 )
import Hydra.Chain.Direct (DirectChainLog, withDirectChain)
import Hydra.Chain.Direct.MockServer (withMockServer)
import Hydra.Chain.Direct.Util (retrying)
import Hydra.Chain.Direct.Wallet (generateKeyPair)
import Hydra.Chain.Direct.WalletSpec (genPaymentTo)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)
import Test.QuickCheck (generate)

spec :: Spec
spec = do
  it "can init and abort a head given nothing has been committed" $
    showLogsOnFailure $ \tracer -> do
      calledBackAlice <- newEmptyMVar
      calledBackBob <- newEmptyMVar
      aliceKeys@(aliceVk, _) <- generateKeyPair
      bobKeys@(bobVk, _) <- generateKeyPair
      withMockServer $ \magic iocp socket submitTx -> do
        let cardanoKeys = [] -- TODO(SN): this should matter
        withDirectChain (contramap FromAlice tracer) magic iocp socket aliceKeys alice cardanoKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
          withDirectChain (contramap FromBob tracer) magic iocp socket bobKeys bob cardanoKeys (putMVar calledBackBob) $ \_ -> do
            let parameters = HeadParameters 100 [alice, bob, carol]
            generate (genPaymentTo magic aliceVk) >>= submitTx
            generate (genPaymentTo magic bobVk) >>= submitTx

            failAfter 5 $
              retrying @ErrorCall $ postTx $ InitTx parameters
            failAfter 5 $
              takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]
            failAfter 5 $
              takeMVar calledBackBob `shouldReturn` OnInitTx 100 [alice, bob, carol]

            postTx $ AbortTx mempty

            failAfter 5 $
              takeMVar calledBackAlice `shouldReturn` OnAbortTx
            failAfter 5 $
              takeMVar calledBackBob `shouldReturn` OnAbortTx

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

data Log
  = FromAlice DirectChainLog
  | FromBob DirectChainLog
  deriving (Show)
