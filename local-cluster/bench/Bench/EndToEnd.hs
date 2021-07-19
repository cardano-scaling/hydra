{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Bench.EndToEnd where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Control.Lens ((^?))
import Control.Monad.Class.MonadSTM (
  MonadSTM (readTVarIO),
  modifyTVar,
  newTVarIO,
 )
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Lens (key, _Array, _Number)
import Data.ByteString.Lazy (hPut)
import qualified Data.Map as Map
import Data.Scientific (floatingOrInteger)
import Hydra.Ledger (TxId)
import Hydra.Ledger.Simple (SimpleTx)
import HydraNode (
  HydraClient,
  failAfter,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraNode,
  withMockChain,
 )

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

data Event = Event
  { submittedAt :: UTCTime
  , confirmedAt :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show, ToJSON)

bench :: IO ()
bench = do
  registry <- newTVarIO mempty :: IO (TVar IO (Map.Map (TxId SimpleTx) Event))

  failAfter 30 $
    withMockChain $ \chainPorts ->
      withHydraNode chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
        withHydraNode chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
          withHydraNode chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
            waitForNodesConnected [n1, n2, n3]
            let contestationPeriod = 10 -- TODO: Should be part of init
            send n1 $ input "init" []
            waitFor 3 [n1, n2, n3] $
              output "readyToCommit" ["parties" .= [int 10, 20, 30]]
            send n1 $ input "commit" ["utxo" .= [int 1]]
            send n2 $ input "commit" ["utxo" .= [int 2]]
            send n3 $ input "commit" ["utxo" .= [int 3]]

            waitFor 3 [n1, n2, n3] $ output "headIsOpen" ["utxo" .= [int 1, 2, 3]]

            let txId = 42
            tx <- newTx registry n1 txId [1] [4]

            txs <- waitMatch n1 $ \v -> do
              guard (v ^? key "output" == Just "snapshotConfirmed")
              v ^? key "snapshot" . key "confirmedTransactions" . _Array

            mapM_ (confirmTx registry) txs

            send n1 $ input "getUtxo" []
            waitFor 10 [n1] $ output "utxo" ["utxo" .= [int 2, 3, 4]]

            send n1 $ input "close" []
            waitFor 3 [n1] $
              output
                "headIsClosed"
                [ "contestationPeriod" .= contestationPeriod
                , "latestSnapshot"
                    .= object
                      [ "snapshotNumber" .= int 1
                      , "utxo" .= [int 2, 3, 4]
                      , "confirmedTransactions" .= [tx]
                      ]
                ]
            waitFor (contestationPeriod + 3) [n1] $ output "headIsFinalized" ["utxo" .= [int 2, 3, 4]]

  hPut stderr . encode . mapMaybe analyze . Map.toList =<< readTVarIO registry

--
-- Helpers
--

int :: Int -> Int
int = id

type TransactionId = Integer
type TransactionInput = Int
type TransactionOutput = Int

newTx ::
  TVar IO (Map.Map (TxId SimpleTx) Event) ->
  HydraClient ->
  TransactionId ->
  [TransactionInput] ->
  [TransactionOutput] ->
  IO Value
newTx registry client txId inputs outputs = do
  now <- getCurrentTime
  atomically $
    modifyTVar registry $
      Map.insert txId $
        Event
          { submittedAt = now
          , confirmedAt = Nothing
          }
  let tx = object ["id" .= txId, "inputs" .= inputs, "outputs" .= outputs]
  send client $ input "newTransaction" ["transaction" .= tx]
  pure tx

confirmTx ::
  TVar IO (Map.Map (TxId SimpleTx) Event) ->
  Value ->
  IO ()
confirmTx registry tx = do
  case floatingOrInteger @Double <$> tx ^? key "id" . _Number of
    Just (Right txId) -> do
      now <- getCurrentTime
      atomically $
        modifyTVar registry $
          Map.adjust (\e -> e{confirmedAt = Just now}) txId
    _ -> error $ "incorrect Txid" <> show tx

analyze :: (TxId SimpleTx, Event) -> Maybe (UTCTime, NominalDiffTime)
analyze = \case
  (_, Event{submittedAt, confirmedAt = Just conf}) -> Just (submittedAt, conf `diffUTCTime` submittedAt)
  _ -> Nothing
