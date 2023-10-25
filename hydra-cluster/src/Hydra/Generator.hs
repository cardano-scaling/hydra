{-# LANGUAGE DeriveAnyClass #-}

module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (CardanoSKey, cardanoVKeyFromSKey, mkGenesisTx, verificatioKeyFromSKey)
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
  deriving (Show, Generic, ToJSON, FromJSON)

instance Arbitrary Dataset where
  arbitrary = sized $ \n -> do
    sk <- genSigningKey
    genDatasetConstantUTxO sk defaultProtocolParameters (n `div` 10) n

data ClientKeys = ClientKeys
  { signingKey :: CardanoSKey
  -- ^ Key used by the hydra-node to authorize hydra transactions and holding fuel.
  , externalSigningKey :: CardanoSKey
  -- ^ Key holding funds to commit.
  }
  deriving (Show)

instance ToJSON ClientKeys where
  toJSON ClientKeys{signingKey, externalSigningKey} = do
    -- TODO: dry this
    case (signingKey, externalSigningKey) of
      (Left sk, Left esk) ->
        object
          [ "signingKey" .= serialiseToBech32 sk
          , "externalSigningKey" .= serialiseToBech32 esk
          ]
      (Right sk, Right esk) ->
        object
          [ "signingKey" .= serialiseToBech32 sk
          , "externalSigningKey" .= serialiseToBech32 esk
          ]
      (Left sk, Right esk) ->
        object
          [ "signingKey" .= serialiseToBech32 sk
          , "externalSigningKey" .= serialiseToBech32 esk
          ]
      (Right sk, Left esk) ->
        object
          [ "signingKey" .= serialiseToBech32 sk
          , "externalSigningKey" .= serialiseToBech32 esk
          ]

instance FromJSON ClientKeys where
  parseJSON =
    withObject "ClientKeys" $ \o ->
      ClientKeys
        <$> fmap Left (decodeSigningKey =<< o .: "signingKey")
        <*> fmap Left (decodeSigningKey =<< o .: "externalSigningKey")
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
  generate $ genDatasetConstantUTxO (Left faucetSk) pparams nClients nTxs

genDatasetConstantUTxO ::
  -- | The faucet signing key
  CardanoSKey ->
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
    amount <- Lovelace <$> choose (1, availableInitialFunds `div` fromIntegral nClients)
    pure (cardanoVKeyFromSKey externalSigningKey, amount)
  let fundingTransaction =
        mkGenesisTx
          networkId
          pparams
          faucetSk
          (Lovelace availableInitialFunds)
          clientFunds
  clientDatasets <- forM clientKeys (generateClientDataset fundingTransaction)
  pure Dataset{fundingTransaction, clientDatasets, title = Nothing, description = Nothing}
 where
  generateClientDataset fundingTransaction clientKeys@ClientKeys{externalSigningKey} = do
    let keyPair = case externalSigningKey of
          Left sk' -> Left (verificatioKeyFromSKey externalSigningKey, sk')
          Right sk' -> Right (getVerificationKey sk', sk')
        -- NOTE: The initialUTxO must all UTXO we will later commit. We assume
        -- that everything owned by the externalSigningKey will get committed
        -- into the head.
        initialUTxO =
          utxoProducedByTx fundingTransaction
            & UTxO.filter ((== mkVkAddress networkId (cardanoVKeyFromSKey externalSigningKey)) . txOutAddress)
    txSequence <-
      reverse . thrd
        <$> foldM (generateOneTransfer networkId) (initialUTxO, keyPair, []) [1 .. nTxs]
    pure ClientDataset{clientKeys, initialUTxO, txSequence}

  thrd (_, _, c) = c
