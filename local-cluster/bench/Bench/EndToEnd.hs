module Bench.EndToEnd where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Control.Monad.Class.MonadSTM (
  newTVarIO,
  modifyTVar,
 )
import Data.Aeson (object, (.=), Value)
import qualified Data.Map as Map
import HydraNode (
  failAfter,
  HydraClient,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  withHydraNode,
  withMockChain,
 )
import Hydra.Ledger(TxId)
import Hydra.Ledger.Simple(SimpleTx)

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

            waitFor 10 [n1, n2, n3] $ output "transactionSeen" ["transaction" .= tx]
            waitFor 10 [n1, n2, n3] $
              output
                "snapshotConfirmed"
                [ "snapshot"
                    .= object
                      [ "confirmedTransactions" .= [tx]
                      , "snapshotNumber" .= int 1
                      , "utxo" .= [int 2, 3, 4]
                      ]
                ]

            confirmTx registry txId

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

--
-- Helpers
--

int :: Int -> Int
int = id

newTx
  :: TVar IO (Map.Map (TxId SimpleTx) Event)
  -> HydraClient
  -> Integer -- | Transaction Id
  -> [Int] -- | Transaction inputs
  -> [Int] -- | Transaction outputs
  -> IO Value
newTx registry client txId inputs outputs = do
  now <- getCurrentTime
  atomically $ modifyTVar registry $ Map.insert txId $ Event
    { submittedAt = now
    , confirmedAt = Nothing
    }
  let tx = object ["id" .= txId, "inputs" .= inputs, "outputs" .= outputs]
  send client $ input "newTransaction" ["transaction" .= tx]
  pure tx

confirmTx ::  TVar IO (Map.Map (TxId SimpleTx) Event)
  -> Integer -- | Transaction Id
  -> IO ()
confirmTx registry txId = do
  now <- getCurrentTime
  atomically $ modifyTVar registry $
    Map.adjust (\e -> e { confirmedAt = Just now}) txId
