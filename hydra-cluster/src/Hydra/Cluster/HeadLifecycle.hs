-- | Head lifecycle shared by benchmarks and scripted demonstrations.
module Hydra.Cluster.HeadLifecycle where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (HydraNodeLog, runBackend)
import Control.Lens ((^..), (^?))
import Control.Tracer.JSON (Tracer)
import Data.Aeson ((.=))
import Data.Aeson.Lens (key, members, _JSON)
import Data.Aeson.Types (parseMaybe)
import Data.Set qualified as Set
import Hydra.Cardano.Api (UTxO, lovelaceToValue)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Cluster.Faucet (FaucetLog (..), seedFromFaucet)
import Hydra.Cluster.Util (Timing (..), depositTimeout)
import Hydra.Generator (ClientDataset (..), Dataset (..))
import Hydra.Options (ChainBackendOptions)
import Hydra.Tx (HeadId, txId)
import Hydra.Tx.Crypto (getVerificationKey, signTx)
import HydraNode (HydraClient, getSnapshotUTxO, input, output, requestCommitTx, send, waitFor, waitForAllMatch, waitMatch)
import Test.Hydra.Prelude (shouldBe)

-- | Fund the dataset and node fuel, optionally reserving incremental deposits.
fundDataset :: ChainBackendOptions -> Dataset -> Bool -> Tracer IO FaucetLog -> IO [UTxO]
fundDataset opts Dataset{fundingTransaction, hydraNodeKeys, clientDatasets} incremental tracer = do
  runBackend opts $ submitTransaction fundingTransaction
  forM_ hydraNodeKeys $ \sk -> do
    let vk = getVerificationKey sk
    void $ runBackend opts (awaitTransaction fundingTransaction vk)
    void $ seedFromFaucet opts vk (lovelaceToValue 100_000_000) tracer
  if incremental
    then forM clientDatasets $ \ClientDataset{paymentKey} ->
      seedFromFaucet opts (getVerificationKey paymentKey) (lovelaceToValue 10_000_000) tracer
    else pure []

-- | Open an empty Head and wait for every dataset deposit to settle everywhere.
openHead :: Timing -> ChainBackendOptions -> [ClientDataset] -> NonEmpty HydraClient -> IO HeadId
openHead timing@Timing{blockTime} opts datasets nodes = do
  let leader = head nodes
      clients = toList nodes
  unless (length clients == length datasets) $
    fail "One client dataset is required per participant"
  putTextLn "Initializing Head"
  send leader $ input "Init" []
  headId <- waitForAllMatch (60 + 5 * blockTime) clients $ \v -> do
    guard $ v ^? key "tag" == Just "HeadIsOpen"
    v ^? key "headId" . _JSON
  putTextLn "Depositing initialUTxO from datasets"
  deposits <- forM (zip clients datasets) $ \(client, ClientDataset{initialUTxO, paymentKey}) -> do
    tx <- signTx paymentKey <$> requestCommitTx client initialUTxO
    runBackend opts $ submitTransaction tx
    pure tx
  observed <- replicateM (length deposits) $
    waitForAllMatch (depositTimeout timing * fromIntegral (length clients) * 30) clients $ \v -> do
      guard $ v ^? key "tag" == Just "CommitFinalized"
      guard $ v ^? key "headId" == Just (toJSON headId)
      v ^? key "depositTxId" >>= parseMaybe parseJSON
  Set.fromList observed `shouldBe` Set.fromList (txId <$> deposits)
  putTextLn "HeadIsOpen with deposits finalized"
  pure headId

-- | Return the observed contestation deadline, allowing reporting before fanout.
closeHead :: Timing -> HeadId -> HydraClient -> IO UTCTime
closeHead Timing{blockTime} headId leader = do
  putTextLn "Closing the Head"
  send leader $ input "Close" []
  waitMatch (60 + 20 * blockTime) leader $ \v -> do
    guard $ v ^? key "tag" == Just "HeadIsClosed"
    guard $ v ^? key "headId" == Just (toJSON headId)
    v ^? key "contestationDeadline" . _JSON

-- | Wait for the deadline and require successful finalization. Callers decide
-- whether a failure is fatal (scenarios) or belongs in a benchmark report.
fanoutHead :: Tracer IO HydraNodeLog -> UTCTime -> HeadId -> HydraClient -> IO Int
fanoutHead tracer deadline headId leader = do
  remaining <- diffUTCTime deadline <$> getCurrentTime
  waitFor tracer (max 0 remaining + 3) [leader] $
    output "ReadyToFanout" ["headId" .= headId]
  putTextLn "Finalizing the Head"
  headSize <- UTxO.size <$> getSnapshotUTxO leader
  send leader $ input "Fanout" []
  waitMatch (fromIntegral $ 100 + headSize `div` 2) leader $ \v -> do
    guard $ v ^? key "tag" == Just "HeadIsFinalized"
    guard $ v ^? key "headId" == Just (toJSON headId)
    finalized <- v ^? key "finalizedUTxO"
    pure $ length (finalized ^.. members)
