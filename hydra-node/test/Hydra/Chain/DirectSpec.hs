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
import Hydra.Cardano.Api (NetworkId (..), PaymentKey, VerificationKey)
import Hydra.Chain (
  Chain (..),
  HeadParameters (HeadParameters),
  OnChainTx (..),
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
        withMockServer $ \networkId iocp socket submitTx -> do
          withDirectChain (contramap FromAlice tracer) networkId iocp socket aliceKeys alice cardanoKeys Nothing (putMVar calledBackAlice) $ \Chain{postTx} -> do
            withDirectChain (contramap FromBob tracer) networkId iocp socket bobKeys bob cardanoKeys Nothing (putMVar calledBackBob) $ \_ -> do
              let parameters = HeadParameters 100 [alice, bob, carol]
              mkSeedPayment networkId aliceVk submitTx

              retry (== NoSeedInput @Tx) $ postTx $ InitTx parameters
              takeMVar calledBackAlice
                >>= ( `shouldSatisfy`
                        \case
                          OnInitTx{contestationPeriod, parties} ->
                            contestationPeriod == 100 && parties == [alice, bob, carol]
                          _ ->
                            False
                    )
              takeMVar calledBackBob
                >>= ( `shouldSatisfy`
                        \case
                          OnInitTx{contestationPeriod, parties} ->
                            contestationPeriod == 100 && parties == [alice, bob, carol]
                          _ ->
                            False
                    )

              postTx $ AbortTx mempty
              takeMVar calledBackAlice `shouldReturn` OnAbortTx
              takeMVar calledBackBob `shouldReturn` OnAbortTx

mkSeedPayment :: NetworkId -> VerificationKey PaymentKey -> (ValidatedTx Era -> IO ()) -> IO ()
mkSeedPayment networkId vk submitTx =
  generate (genPaymentTo networkId vk) >>= submitTx

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30

data Log
  = FromAlice DirectChainLog
  | FromBob DirectChainLog
  deriving (Show)
