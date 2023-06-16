{-# LANGUAGE DeriveAnyClass #-}

module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import qualified Cardano.Api.UTxO as UTxO
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
-- The 'transactionSequence' is guaranteed to be applicable, in sequence, to the 'initialUTxO'
-- set.
data Dataset = Dataset
  { fundingTransaction :: Tx
  , clientDatasets :: [ClientDataset]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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
  deriving (Show)

instance ToJSON ClientKeys where
  toJSON ClientKeys{signingKey, externalSigningKey} =
    object
      [ "signingKey" .= serialiseToBech32 signingKey
      , "externalSigningKey" .= serialiseToBech32 externalSigningKey
      ]

instance FromJSON ClientKeys where
  parseJSON =
    withObject "ClientKeys" $ \o ->
      ClientKeys
        <$> (decodeSigningKey =<< o .: "signingKey")
        <*> (decodeSigningKey =<< o .: "externalSigningKey")
   where
    decodeSigningKey =
      either (fail . show) pure . deserialiseFromBech32 (AsSigningKey AsPaymentKey)

instance Arbitrary ClientKeys where
  arbitrary = ClientKeys <$> genSigningKey <*> genSigningKey

data ClientDataset = ClientDataset
  { clientKeys :: ClientKeys
  , initialUTxO :: UTxO
  , txSequence :: [Tx]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

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

  -- TODO: benchmark does fuelWith100Ada, could remove the signing key here again
  clientFunds <- fmap concat . forM clientKeys $ \ClientKeys{signingKey, externalSigningKey} -> do
    amount <- Lovelace <$> choose (1, availableNonFuelFunds `div` fromIntegral nClients)
    pure [(signingKey, Lovelace fuelAmount), (externalSigningKey, amount)]

  -- Prepare funding transaction as it will be posted
  let fundingTransaction =
        mkGenesisTx
          networkId
          pparams
          faucetSk
          (Lovelace availableInitialFunds)
          (first getVerificationKey <$> clientFunds)
  clientDatasets <- forM clientKeys (generateClientDataset fundingTransaction)
  pure Dataset{fundingTransaction, clientDatasets}
 where
  fuelAmount = 20_000_000

  availableNonFuelFunds = availableInitialFunds - fromIntegral nClients * fuelAmount

  thrd (_, _, c) = c

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
      reverse . thrd
        <$> foldM (generateOneTransfer networkId) (initialUTxO, keyPair, []) [1 .. nTxs]
    pure ClientDataset{clientKeys, initialUTxO, txSequence}
