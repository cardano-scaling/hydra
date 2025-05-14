module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (QueryPoint (QueryTip), buildTransaction, mkGenesisTx, queryUTxOFor)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Cluster.Faucet (FaucetException (..))
import Hydra.Cluster.Fixture (availableInitialFunds)
import Hydra.Ledger.Cardano (mkTransferTx)
import Test.Hydra.Tx.Gen (genSigningKey)
import Test.QuickCheck (choose, generate, sized)

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

-- | A 'Dataset' that can be run for testing purpose. Each `Dataset` represents
-- a complete scenario where several `ClientDataset` are run concurrently
-- against one or more `HydraNode`s. A dataset can optionally have a `title` and
-- `description` which will be used to report results.
data Dataset = Dataset
  { fundingTransaction :: Tx
  , hydraNodeKeys :: [SigningKey PaymentKey]
  -- ^ Cardano signing keys that will hold fuel.
  , clientDatasets :: [ClientDataset]
  , title :: Maybe Text
  , description :: Maybe Text
  }
  deriving stock (Show, Generic)

-- NOTE: Hand-written ToJSON and FromJSON instances to deliberately serialize
-- signing keys.

instance ToJSON Dataset where
  toJSON Dataset{fundingTransaction, hydraNodeKeys, clientDatasets, title, description} =
    object
      [ "fundingTransaction" .= fundingTransaction
      , "hydraNodeKeys" .= (serialiseToTextEnvelope (Just "hydraNodeKey") <$> hydraNodeKeys)
      , "clientDatasets" .= clientDatasets
      , "title" .= title
      , "description" .= description
      ]

instance FromJSON Dataset where
  parseJSON = withObject "Dataset" $ \o -> do
    fundingTransaction <- o .: "fundingTransaction"
    hydraNodeKeys <- o .: "hydraNodeKeys" >>= mapM parseSigningKey
    clientDatasets <- o .: "clientDatasets"
    title <- o .: "title"
    description <- o .: "description"
    pure Dataset{fundingTransaction, hydraNodeKeys, clientDatasets, title, description}
   where
    parseSigningKey =
      either (fail . show) pure . deserialiseFromTextEnvelope

instance Arbitrary Dataset where
  arbitrary = sized $ \n -> do
    sk <- genSigningKey
    generateConstantUTxODataset sk (n `div` 10) n

data ClientDataset = ClientDataset
  { paymentKey :: SigningKey PaymentKey
  , initialUTxO :: UTxO
  , txSequence :: [Tx]
  }
  deriving stock (Show, Generic)

instance ToJSON ClientDataset where
  toJSON ClientDataset{paymentKey, initialUTxO, txSequence} =
    object
      [ "paymentKey" .= serialiseToTextEnvelope (Just "paymentKey") paymentKey
      , "initialUTxO" .= initialUTxO
      , "txSequence" .= txSequence
      ]

instance FromJSON ClientDataset where
  parseJSON =
    withObject "ClientDataset" $ \o -> do
      paymentKey <- o .: "paymentKey" >>= parseSigningKey
      initialUTxO <- o .: "initialUTxO"
      txSequence <- o .: "txSequence"
      pure ClientDataset{paymentKey, initialUTxO, txSequence}
   where
    parseSigningKey =
      either (fail . show) pure . deserialiseFromTextEnvelope

-- | Generate a 'Dataset' which does not grow the per-client UTXO set over time.
-- This version provided faucet key owns funds on the initial funds of the
-- devnet (See 'availableInitialFunds' and 'genesis-shelley.json'). Then for a
-- given number of clients a number of transactions are generated.
generateConstantUTxODataset ::
  -- | Faucet signing key
  SigningKey PaymentKey ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  Gen Dataset
generateConstantUTxODataset faucetSk nClients nTxs = do
  hydraNodeKeys <- replicateM nClients genSigningKey
  allPaymentKeys <- replicateM nClients genSigningKey
  -- Prepare funding transaction which will give every client's
  -- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
  -- funded in the beginning of the benchmark run.
  clientFunds <- genClientFunds allPaymentKeys availableInitialFunds
  let fundingTransaction =
        mkGenesisTx
          networkId
          faucetSk
          (Coin availableInitialFunds)
          clientFunds
  clientDatasets <- forM allPaymentKeys (generateClientDataset networkId fundingTransaction nTxs)
  pure Dataset{fundingTransaction, hydraNodeKeys, clientDatasets, title = Nothing, description = Nothing}

-- | Generate a 'Dataset' from an already running network by quering available
-- funds of the well-known 'faucet.sk' and assuming the hydra-nodes we connect
-- to have fuel available. Then for a given number of clients a number of
-- transactions are generated.
generateDemoUTxODataset ::
  NetworkId ->
  SocketPath ->
  -- | Faucet signing key
  SigningKey PaymentKey ->
  -- | Number of clients.
  Int ->
  -- | Number of transactions
  Int ->
  IO Dataset
generateDemoUTxODataset network nodeSocket faucetSk nClients nTxs = do
  -- Query available funds
  faucetUTxO <- queryUTxOFor network nodeSocket QueryTip faucetVk
  let (Coin fundsAvailable) = foldMap (selectLovelace . txOutValue) faucetUTxO
  -- Generate client datasets
  allPaymentKeys <- generate $ replicateM nClients genSigningKey
  clientFunds <- generate $ genClientFunds allPaymentKeys fundsAvailable
  -- XXX: DRY with 'seedFromFaucet'
  fundingTransaction <- do
    let recipientOutputs =
          flip map clientFunds $ \(vk, ll) ->
            TxOut
              (mkVkAddress network vk)
              (lovelaceToValue ll)
              TxOutDatumNone
              ReferenceScriptNone
    buildTransaction network nodeSocket faucetAddress faucetUTxO [] recipientOutputs >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right tx -> pure $ signTx faucetSk tx
  generate $ do
    clientDatasets <- forM allPaymentKeys (generateClientDataset network fundingTransaction nTxs)
    pure
      Dataset
        { fundingTransaction
        , hydraNodeKeys = [] -- Not needed as we won't start nodes
        , clientDatasets
        , title = Nothing
        , description = Nothing
        }
 where
  faucetVk = getVerificationKey faucetSk

  faucetAddress = mkVkAddress network faucetVk

-- * Helpers

withInitialUTxO :: SigningKey PaymentKey -> Tx -> UTxO
withInitialUTxO externalSigningKey fundingTransaction =
  let vk = getVerificationKey externalSigningKey
   in -- NOTE: The initialUTxO must all UTXO we will later commit. We assume
      -- that everything owned by the externalSigningKey will get committed
      -- into the head.
      utxoProducedByTx fundingTransaction
        & UTxO.filter ((== mkVkAddress networkId vk) . txOutAddress)

genClientFunds :: [SigningKey PaymentKey] -> Integer -> Gen [(VerificationKey PaymentKey, Coin)]
genClientFunds paymentKeys availableFunds =
  forM paymentKeys $ \paymentKey -> do
    amount <- Coin <$> choose (1, availableFunds `div` fromIntegral nClients)
    pure (getVerificationKey paymentKey, amount)
 where
  nClients = length paymentKeys

generateClientDataset ::
  NetworkId ->
  Tx ->
  Int ->
  SigningKey PaymentKey ->
  Gen ClientDataset
generateClientDataset network fundingTransaction nTxs paymentKey = do
  let initialUTxO = withInitialUTxO paymentKey fundingTransaction
  (_, txs) <- foldM (go paymentKey) (initialUTxO, []) [1 .. nTxs]
  pure ClientDataset{paymentKey, initialUTxO, txSequence = reverse txs}
 where
  go sk (utxo, txs) _ = do
    case mkTransferTx network utxo sk (getVerificationKey sk) of
      Left err -> error $ "mkTransferTx failed: " <> err
      Right tx -> pure (utxoFromTx tx, tx : txs)
