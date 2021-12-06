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
  InvalidTxError (NoSeedInput),
  OnChainTx (OnAbortTx, OnInitTx),
  PostChainTx (AbortTx, InitTx),
 )
import Hydra.Chain.Direct (DirectChainLog, withDirectChain)
import Hydra.Chain.Direct.MockServer (withMockServer)
import Hydra.Chain.Direct.Util (retrying)
import Hydra.Chain.Direct.Wallet (generateKeyPair)
import Hydra.Chain.Direct.WalletSpec (genPaymentTo)
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Party (Party, deriveParty, generateKey)
import Test.QuickCheck (generate)

spec :: Spec
spec = do
  it "can init and abort a head given nothing has been committed" $
    failAfter 10 $
      showLogsOnFailure $ \tracer -> do
        calledBackAlice <- newEmptyMVar
        calledBackBob <- newEmptyMVar
        aliceKeys@(aliceVk, _) <- generateKeyPair
        bobKeys <- generateKeyPair
        withMockServer $ \magic iocp socket submitTx -> do
          let cardanoKeys = [] -- TODO(SN): this should matter
          withDirectChain (contramap FromAlice tracer) magic iocp socket aliceKeys alice cardanoKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
            withDirectChain (contramap FromBob tracer) magic iocp socket bobKeys bob cardanoKeys (putMVar calledBackBob) $ \_ -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              -- An output to use as "seed input"
              generate (genPaymentTo magic aliceVk) >>= submitTx
              -- Some funds to spend
              generate (genPaymentTo magic aliceVk) >>= submitTx

              race_
                (retrying (== NoSeedInput @CardanoTx) $ postTx $ InitTx parameters)
                (takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol])
              takeMVar calledBackBob `shouldReturn` OnInitTx 100 [alice, bob, carol]

              postTx $ AbortTx mempty

              takeMVar calledBackAlice `shouldReturn` OnAbortTx
              takeMVar calledBackBob `shouldReturn` OnAbortTx

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

data Log
  = FromAlice DirectChainLog
  | FromBob DirectChainLog
  deriving (Show)
