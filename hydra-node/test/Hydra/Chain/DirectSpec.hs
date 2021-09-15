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
  OnChainTx (OnInitTx),
  PostChainTx (InitTx),
 )
import Hydra.Chain.Direct (withDirectChain)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.Party (Party, deriveParty, generateKey)

spec :: Spec
spec = parallel $ do
  it "publishes init tx and observes it also" $ do
    calledBackAlice <- newEmptyMVar
    -- calledBackBob <- newEmptyMVar
    withDirectChain nullTracer (putMVar calledBackAlice) $ \Chain{postTx} -> do
      let parameters = HeadParameters 100 [alice, bob, carol]
      postTx $ InitTx @SimpleTx parameters
      takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30
