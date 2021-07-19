module Bench.EndToEnd where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Control.Monad.Class.MonadSTM (
  TVar,
  newTVarIO,
 )
import Data.Aeson (object, (.=))
import Data.Time.Clock (
  getCurrentTime,
 )
import HydraNode (
  failAfter,
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
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

bench :: IO ()
bench = do
  registry <- newTVarIO mempty :: IO (TVar IO (TxId SimpleTx) Event)
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

            newTx registry n1 42 [1] [4]

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
  :: TVar IO (Map (TxId SimpleTx) Event)
  -> HydraClient
  -> Integer -- | Transaction Id
  -> [Int] -- | Transaction inputs
  -> [Int] -- | Transaction outputs
  -> IO ()
newTx registry client txId inputs outputs = do
  now <- getCurrentTime
  modifyTVar registry $ Map.insert txId $ Event
    { submittedAt = now
    , confirmedAt = Nothing
    }
  let tx = object ["id" .= txId, "inputs" .= inputs, "outputs" .= outputs]
  send client $ input "newTransaction" ["transaction" .= tx]
