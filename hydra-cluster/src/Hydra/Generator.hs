module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (mkGenesisTx)
import Control.Monad (foldM)
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

instance ToCBOR Dataset where
  toCBOR Dataset{fundingTransaction, clientDatasets, title, description} =
    mconcat
      [ toCBOR fundingTransaction
      , toCBOR clientDatasets
      , toCBOR title
      , toCBOR description
      ]

instance FromCBOR Dataset where
  fromCBOR = Dataset <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

instance Arbitrary Dataset where
  arbitrary = sized $ \n -> do
    sk <- genSigningKey
    genDatasetConstantUTxO sk defaultProtocolParameters (n `div` 10) n

data ClientKeys = ClientKeys
  { signingKey :: SigningKey PaymentKey
  -- ^ Key used by the hydra-node to authorize hydra transactions and holding fuel.
  , externalSigningKey :: SigningKey PaymentKey
  -- ^ Key holding funds to commit.
  }
  deriving stock (Show)

instance Arbitrary ClientKeys where
  arbitrary = ClientKeys <$> genSigningKey <*> genSigningKey

instance ToCBOR ClientKeys where
  toCBOR ClientKeys{signingKey, externalSigningKey} =
    mconcat
      [ toCBOR signingKey
      , toCBOR externalSigningKey
      ]

instance FromCBOR ClientKeys where
  fromCBOR = ClientKeys <$> fromCBOR <*> fromCBOR

data ClientDataset = ClientDataset
  { clientKeys :: ClientKeys
  , initialUTxO :: UTxO
  , txSequence :: [Tx]
  }
  deriving stock (Show, Generic)

instance ToCBOR ClientDataset where
  toCBOR ClientDataset{clientKeys, initialUTxO, txSequence} =
    mconcat
      [ toCBOR clientKeys
      , toCBOR initialUTxO
      , toCBOR txSequence
      ]

instance FromCBOR ClientDataset where
  fromCBOR = ClientDataset <$> fromCBOR <*> fromCBOR <*> fromCBOR

defaultProtocolParameters :: ProtocolParameters
defaultProtocolParameters = fromLedgerPParams ShelleyBasedEraShelley def

-- | Generate 'Dataset' which does not grow the per-client UTXO set over time.
-- The sequence of transactions generated consist only of simple payments from
-- and to arbitrary keys controlled by the individual clients.
generateConstantUTxODataset ::
  ProtocolParameters ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  IO Dataset
generateConstantUTxODataset pparams nClients nTxs = do
  (_, faucetSk) <- keysFor Faucet
  generate $ genDatasetConstantUTxO faucetSk pparams nClients nTxs

genDatasetConstantUTxO ::
  -- | The faucet signing key
  SigningKey PaymentKey ->
  ProtocolParameters ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  Gen Dataset
genDatasetConstantUTxO faucetSk pparams nClients nTxs = do
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
          pparams
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
