module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import Cardano.Api.Ledger (PParams)
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (buildTransaction, mkGenesisTx, sign)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Default (def)
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.CardanoClient (QueryPoint (..), queryUTxOFor)
import Hydra.Cluster.Faucet (FaucetException (..))
import Hydra.Cluster.Fixture (Actor (Faucet), availableInitialFunds)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger (balance)
import Hydra.Ledger.Cardano (genSigningKey, generateOneTransfer)
import Test.QuickCheck (choose, generate, sized, vectorOf)

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

-- | A 'Dataset' that can be run for testing purpose.
-- Each `Dataset` represents a complete scenario where several `ClientDataset` are run concurrently
-- against one or more `HydraNode`s. A dataset can optionally have a `title` and `description`
-- which will be used to report results.
data Dataset = Dataset
  { fundingTransaction :: Tx
  , clientDatasets :: [ClientDataset]
  , title :: Maybe Text
  , description :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary Dataset where
  arbitrary = sized $ \n -> do
    faucetSk <- genSigningKey
    let nClients = max 1 (min maximumNumberOfParties (n `div` 10))
    clientKeys <- vectorOf nClients arbitrary
    fundingTransaction <- makeGenesisFundingTx faucetSk clientKeys
    genDatasetConstantUTxO clientKeys n fundingTransaction

data ClientKeys = ClientKeys
  { signingKey :: SigningKey PaymentKey
  -- ^ Key used by the hydra-node to authorize hydra transactions and holding fuel.
  , externalSigningKey :: SigningKey PaymentKey
  -- ^ Key holding funds to commit.
  }
  deriving stock (Show)

instance ToJSON ClientKeys where
  toJSON ClientKeys{signingKey, externalSigningKey} =
    object
      [ "signingKey" .= serialiseToTextEnvelope (Just "signingKey") signingKey
      , "externalSigningKey" .= serialiseToTextEnvelope (Just "externalSigningKey") externalSigningKey
      ]

instance FromJSON ClientKeys where
  parseJSON =
    withObject "ClientKeys" $ \o ->
      ClientKeys
        <$> (decodeSigningKey =<< o .: "signingKey")
        <*> (decodeSigningKey =<< o .: "externalSigningKey")
   where
    decodeSigningKey v = do
      envelope <- parseJSON v
      deserialiseFromTextEnvelope (AsSigningKey AsPaymentKey) envelope
        & either (fail . show) pure

instance Arbitrary ClientKeys where
  arbitrary = ClientKeys <$> genSigningKey <*> genSigningKey

data ClientDataset = ClientDataset
  { clientKeys :: ClientKeys
  , initialUTxO :: UTxO
  , txSequence :: [Tx]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultProtocolParameters :: PParams LedgerEra
defaultProtocolParameters = def

-- | Generate 'Dataset' which does not grow the per-client UTXO set over time.
-- The sequence of transactions generated consist only of simple payments from
-- and to arbitrary keys controlled by the individual clients.
generateConstantUTxODataset ::
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  IO Dataset
generateConstantUTxODataset nClients nTxs = do
  (_, faucetSk) <- keysFor Faucet
  clientKeys <- generate $ replicateM nClients arbitrary
  fundingTransaction <- generate $ makeGenesisFundingTx faucetSk clientKeys
  generate $ genDatasetConstantUTxO clientKeys nTxs fundingTransaction

genDatasetConstantUTxO ::
  -- | Clients
  [ClientKeys] ->
  -- | Number of transactions
  Int ->
  Tx ->
  Gen Dataset
genDatasetConstantUTxO allClientKeys nTxs fundingTransaction = do
  clientDatasets <- forM allClientKeys generateClientDataset
  pure Dataset{fundingTransaction, clientDatasets, title = Nothing, description = Nothing}
 where
  generateClientDataset clientKeys@ClientKeys{externalSigningKey} = do
    let vk = getVerificationKey externalSigningKey
        keyPair = (vk, externalSigningKey)
        -- NOTE: The initialUTxO must all UTXO we will later commit. We assume
        -- that everything owned by the externalSigningKey will get committed
        -- into the head.
        initialUTxO =
          utxoProducedByTx fundingTransaction
            & UTxO.filter ((== mkVkAddress networkId vk) . txOutAddress)
    txSequence <-
      reverse
        . thrd
        <$> foldM (generateOneTransfer networkId) (initialUTxO, keyPair, []) [1 .. nTxs]
    pure ClientDataset{clientKeys, initialUTxO, txSequence}

  thrd (_, _, c) = c

-- Prepare funding transaction which will give every client's
-- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
-- funded in the beginning of the benchmark run.
makeGenesisFundingTx :: SigningKey PaymentKey -> [ClientKeys] -> Gen Tx
makeGenesisFundingTx faucetSk clientKeys = do
  let nClients = length clientKeys
  clientFunds <- forM clientKeys $ \ClientKeys{externalSigningKey} -> do
    amount <- Coin <$> choose (1, availableInitialFunds `div` fromIntegral nClients)
    -- NOTE: It seems if this becomes choose (1,1) we get funny signingKeys;
    -- i.e. like "0001010100010001000000010100000001010001000101000000010101010001".
    pure (getVerificationKey externalSigningKey, amount)
  let fundingTransaction =
        mkGenesisTx
          networkId
          faucetSk
          (Coin availableInitialFunds)
          clientFunds
  pure fundingTransaction

genDatasetConstantUTxODemo ::
  -- | The faucet keys
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  -- | Clients
  [ClientKeys] ->
  -- | Number of transactions
  Int ->
  NetworkId ->
  SocketPath ->
  IO Dataset
genDatasetConstantUTxODemo (faucetVk, faucetSk) allClientKeys nTxs networkId' nodeSocket = do
  let nClients = length allClientKeys
  faucetUTxO <- queryUTxOFor networkId nodeSocket QueryTip faucetVk
  let (Coin fundsAvailable) = selectLovelace (balance @Tx faucetUTxO)
  -- Prepare funding transaction which will give every client's
  -- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
  -- funded in the beginning of the benchmark run.
  clientFunds <- forM allClientKeys $ \ClientKeys{externalSigningKey} -> do
    amount <- Coin <$> generate (choose (1, fundsAvailable `div` fromIntegral nClients))
    pure (getVerificationKey externalSigningKey, amount)

  let recipientOutputs =
        flip map clientFunds $ \(vk, ll) ->
          TxOut
            (mkVkAddress networkId' vk)
            (lovelaceToValue ll)
            TxOutDatumNone
            ReferenceScriptNone
  let changeAddress = mkVkAddress networkId' faucetVk
  fundingTransaction <-
    buildTransaction networkId' nodeSocket changeAddress faucetUTxO [] recipientOutputs >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        let signedTx = sign faucetSk body
        pure signedTx
  generate $ genDatasetConstantUTxO allClientKeys nTxs fundingTransaction
