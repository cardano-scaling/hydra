{-# LANGUAGE TypeApplications #-}

-- | Integration tests for the Direct chain component which does interact with a
-- single mocked "node". For tests spinning up real cardano-nodes see
-- 'EndToEndSpec'.
--
-- XXX(AB): Are those tests really useful? What are they really testing? It seems to me
-- we need either to turn those into proper Unit tests, eg. not requiring a full-blown mock server to run,
-- or drop them altogether in favor of `DirectChainSpec` tests which test the real thing.
module Hydra.Chain.DirectSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Hydra.Cardano.Api (NetworkMagic, PaymentKey, VerificationKey)
import Hydra.Chain (
  Chain (..),
  HeadParameters (HeadParameters),
  OnChainTx (OnAbortTx, OnInitTx),
  PostChainTx (AbortTx, InitTx),
  PostTxError (NoSeedInput),
 )
import Hydra.Chain.Direct (DirectChainLog, withDirectChain)
import Hydra.Chain.Direct.MockServer (withMockServer)
import Hydra.Chain.Direct.Util (Era, retry)
import Hydra.Chain.Direct.WalletSpec (genPaymentTo)
import Hydra.Ledger.Cardano (Tx, genKeyPair)
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
        aliceKeys@(aliceVk, _) <- generate genKeyPair
        bobKeys <- generate genKeyPair
        carolKeys <- generate genKeyPair
        let cardanoKeys = [aliceVk, fst bobKeys, fst carolKeys]
        withMockServer $ \magic iocp socket submitTx -> do
          withDirectChain (contramap FromAlice tracer) magic iocp socket aliceKeys alice cardanoKeys (putMVar calledBackAlice) $ \Chain{postTx} -> do
            withDirectChain (contramap FromBob tracer) magic iocp socket bobKeys bob cardanoKeys (putMVar calledBackBob) $ \_ -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              mkSeedPayment magic aliceVk submitTx

              retry (== NoSeedInput @Tx) $ postTx $ InitTx parameters
              takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]
              takeMVar calledBackBob `shouldReturn` OnInitTx 100 [alice, bob, carol]

              postTx $ AbortTx mempty
              takeMVar calledBackAlice `shouldReturn` OnAbortTx
              takeMVar calledBackBob `shouldReturn` OnAbortTx

mkSeedPayment :: NetworkMagic -> VerificationKey PaymentKey -> (ValidatedTx Era -> IO ()) -> IO ()
mkSeedPayment magic vk submitTx =
  generate (genPaymentTo magic vk) >>= submitTx

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

data Log
  = FromAlice DirectChainLog
  | FromBob DirectChainLog
  deriving (Show)
