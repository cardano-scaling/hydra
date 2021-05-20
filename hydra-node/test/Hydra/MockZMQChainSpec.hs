module Hydra.MockZMQChainSpec where

import Cardano.Prelude
import qualified Data.Set as Set
import Data.String (String)
import Hydra.Logic (OnChainTx (InitTx), ParticipationToken (..), Party (..))
import Hydra.MockZMQChain (mockChainClient, runChainSync, startChain)
import System.Timeout (timeout)
import Test.Hspec (Spec, around, describe, it, shouldReturn)

spec :: Spec
spec =
  around withMockZMQChain $
    describe "Mock 0MQ-Based Chain" $ do
      it "publish transactions received from a client" $ \(syncAddress, postAddress) -> do
        let tx = InitTx (Set.fromList [ParticipationToken 2 (Party 1), ParticipationToken 2 (Party 2)])
        mvar <- newEmptyMVar
        void $
          concurrently
            (mockChainClient postAddress tx)
            (timeout 1_000_000 $ runChainSync syncAddress (putMVar mvar))

        timeout 1_000_000 (takeMVar mvar) `shouldReturn` Just tx

withMockZMQChain :: ((String, String) -> IO ()) -> IO ()
withMockZMQChain action =
  withAsync (startChain syncAddress postAddress) $ \_ -> do
    threadDelay 500_000 -- we lack proper synchronisation so better give clients time to join the party
    action (syncAddress, postAddress)
 where
  syncAddress = "tcp://127.0.0.1:54321"
  postAddress = "tcp://127.0.0.1:54322"
