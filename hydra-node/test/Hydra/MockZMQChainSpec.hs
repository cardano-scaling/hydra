{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.MockZMQChainSpec where

import Cardano.Prelude
import qualified Data.Set as Set
import Data.String (String)
import Hydra.Logic (OnChainTx (InitTx), ParticipationToken (..))
import Hydra.MockZMQChain (mockChainClient, runChainSync, startChain)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec =
  describe "Mock 0MQ-Based Chain" $ do
    let tx = InitTx (Set.fromList [ParticipationToken 2 1, ParticipationToken 2 2])

    it "publish transactions received from a client given chain is started" $ do
      withMockZMQChain 54321 54322 54323 $ \syncAddress catchUpAddress postAddress -> do
        mvar <- newEmptyMVar
        void $
          concurrently
            (mockChainClient postAddress tx)
            (within1second $ runChainSync catchUpAddress syncAddress (putMVar mvar))

        within1second (takeMVar mvar) `shouldReturn` Just tx

    it "chain sync catches up with mock chain when starting up" $ do
      chan <- newChan
      withMockZMQChain 54324 54325 54326 $ \syncAddress catchUpAddress postAddress -> do
        forM_ [1 .. 3 :: Int] $ const $ mockChainClient postAddress tx
        transactions <- within1second $
          withAsync (runChainSync catchUpAddress syncAddress (writeChan chan)) $ \_ ->
            getChanContents chan

        transactions `shouldBe` Just [tx, tx, tx]

withMockZMQChain :: Int -> Int -> Int -> (String -> String -> String -> IO ()) -> IO ()
withMockZMQChain syncPort catchUpPort postPort action =
  withAsync (startChain syncAddress catchUpAddress postAddress) $ \_ -> do
    threadDelay 500_000 -- we lack proper synchronisation so better give clients time to join the party
    action syncAddress catchUpAddress postAddress
 where
  syncAddress = "tcp://127.0.0.1:" <> show syncPort
  catchUpAddress = "tcp://127.0.0.1:" <> show catchUpPort
  postAddress = "tcp://127.0.0.1:" <> show postPort

within1second :: IO a -> IO (Maybe a)
within1second = timeout 1_000_000
