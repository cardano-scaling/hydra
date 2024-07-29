module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import Cardano.Api.Ledger (PParams)
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (mkGenesisTx)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Default (def)
import Hydra.Cluster.Fixture (Actor (Faucet), availableInitialFunds)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano (genSigningKey, generateOneTransfer)
import Test.QuickCheck (choose, generate, sized)

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
    sk <- genSigningKey
    genDatasetConstantUTxO sk (n `div` 10) n

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
  generate $ genDatasetConstantUTxO faucetSk nClients nTxs

genDatasetConstantUTxO ::
  -- | The faucet signing key
  SigningKey PaymentKey ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  Gen Dataset
genDatasetConstantUTxO faucetSk nClients nTxs = do
  clientKeys <- replicateM nClients arbitrary
  -- Prepare funding transaction which will give every client's
  -- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
  -- funded in the beginning of the benchmark run.
  clientFunds <- forM clientKeys $ \ClientKeys{externalSigningKey} -> do
    amount <- Coin <$> choose (1, availableInitialFunds `div` fromIntegral nClients)
    pure (getVerificationKey externalSigningKey, amount)
  let fundingTransaction =
        mkGenesisTx
          networkId
          faucetSk
          (Coin availableInitialFunds)
          clientFunds
  clientDatasets <- forM clientKeys (generateClientDataset fundingTransaction)
  pure Dataset{fundingTransaction, clientDatasets, title = Nothing, description = Nothing}
 where
  generateClientDataset fundingTransaction clientKeys@ClientKeys{externalSigningKey} = do
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
