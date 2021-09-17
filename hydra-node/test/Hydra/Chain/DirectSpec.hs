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
import Hydra.Chain.Direct (
  NetworkMagic (..),
  withDirectChain,
  withMockServer,
 )
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import Hydra.Party (Party, deriveParty, generateKey)
import Ouroboros.Network.Channel (Channel (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

spec :: Spec
spec = parallel $ do
  it "publishes init tx and observes it also" $ do
    let networkMagic = NetworkMagic 42
    withSystemTempDirectory "hydra-direct-spec" $ \dir -> do
      let socket = dir </> "node.socket"
      withMockServer networkMagic socket $ do
        calledBackAlice <- newEmptyMVar
        withDirectChain nullTracer networkMagic socket (putMVar calledBackAlice) $ \Chain{postTx} -> do
          calledBackBob <- newEmptyMVar
          withDirectChain nullTracer networkMagic socket (putMVar calledBackBob) $ \_ -> do
            let parameters = HeadParameters 100 [alice, bob, carol]
            postTx $ InitTx @SimpleTx parameters
            failAfter 5 $
              takeMVar calledBackAlice `shouldReturn` OnInitTx 100 [alice, bob, carol]
            failAfter 5 $
              takeMVar calledBackBob `shouldReturn` OnInitTx @SimpleTx 100 [alice, bob, carol]

-- | Mock implementation of for a Node-to-Client protocol server which should be
-- accepting transactions and responding with new blocks containing them.
withTestNodeToClientServer :: (IO (Channel IO LByteString) -> IO ()) -> IO ()
withTestNodeToClientServer action = do
  action connect
 where
  -- Provide a channel to a newly connected "component"
  connect =
    pure $
      Channel
        { send = const $ pure ()
        , recv = pure Nothing
        }

alice, bob, carol :: Party
alice = deriveParty $ generateKey 10
bob = deriveParty $ generateKey 20
carol = deriveParty $ generateKey 30
