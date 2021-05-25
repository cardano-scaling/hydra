{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.MockZMQChainSpec where

import Cardano.Prelude
import qualified Data.Set as Set
import Data.String (String)
import Hydra.Logic (OnChainTx (InitTx), ParticipationToken (..))
import Hydra.MockZMQChain (catchUpTransactions, mockChainClient, runChainSync, startChain)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec =
  describe "Mock 0MQ-Based Chain" $ do
    let tx = InitTx (Set.fromList [ParticipationToken 2 1, ParticipationToken 2 2])
        numberOfTxs :: Int
        numberOfTxs = 3

    it "publish transactions received from a client given chain is started" $ do
      withMockZMQChain 54321 54322 54323 $ \syncAddress _catchUpAddress postAddress -> do
        mvar <- newEmptyMVar
        void $
          concurrently
            ( -- we lack proper synchronisation so better give chain sync time to join the party
              threadDelay 500_000 >> mockChainClient postAddress tx
            )
            (within3second $ runChainSync syncAddress (putMVar mvar))

        within3second (takeMVar mvar) `shouldReturn` Just tx

    it "catches up transacions with mock chain" $ do
      chan <- newChan
      withMockZMQChain 54324 54325 54326 $ \_syncAddress catchUpAddress postAddress -> do
        forM_ [1 .. numberOfTxs] $ const $ mockChainClient postAddress tx
        catchUpTransactions catchUpAddress (writeChan chan)
        within3second (forM [1 .. numberOfTxs] (const $ readChan chan)) `shouldReturn` Just [tx, tx, tx]

withMockZMQChain :: Int -> Int -> Int -> (String -> String -> String -> IO ()) -> IO ()
withMockZMQChain syncPort catchUpPort postPort action =
  withAsync (startChain syncAddress catchUpAddress postAddress) $ \_ -> do
    action syncAddress catchUpAddress postAddress
 where
  syncAddress = "tcp://127.0.0.1:" <> show syncPort
  catchUpAddress = "tcp://127.0.0.1:" <> show catchUpPort
  postAddress = "tcp://127.0.0.1:" <> show postPort

within3second :: IO a -> IO (Maybe a)
within3second = timeout 3_000_000
