module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import Cardano.Api.Ledger (PParams)
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (QueryPoint (QueryTip), buildRawTransaction, buildTransaction, queryUTxOFor, sign)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Default (def)
import Hydra.Cluster.Faucet (FaucetException (..))
import Hydra.Cluster.Fixture (Actor (..), availableInitialFunds)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger.Cardano (
  generateOneRandomTransfer,
  generateOneSelfTransfer,
 )
import Hydra.Tx (balance)
import Test.Hydra.Tx.Gen (
  genSigningKey,
 )
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
    generateConstantUTxODataset sk (n `div` 10) n

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
  -- | Faucet signing key
  SigningKey PaymentKey ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  Gen Dataset
generateConstantUTxODataset faucetSk nClients nTxs = do
  allClientKeys <- replicateM nClients arbitrary
  -- Prepare funding transaction which will give every client's
  -- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
  -- funded in the beginning of the benchmark run.
  clientFunds <- genClientFunds allClientKeys availableInitialFunds
  let fundingTransaction =
        buildRawTransaction
          networkId
          initialInput
          faucetSk
          (Coin availableInitialFunds)
          clientFunds
  let dataset clientKeys =
        generateClientDataset networkId fundingTransaction clientKeys nTxs generateOneRandomTransfer
  clientDatasets <- forM allClientKeys dataset
  pure Dataset{fundingTransaction, clientDatasets, title = Nothing, description = Nothing}
 where
  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (unsafeCastHash $ verificationKeyHash $ getVerificationKey faucetSk)

-- | Generate 'Dataset' which does not grow the per-client UTXO set over time.
-- This queries the network to fetch the current funds available in the faucet
-- to be distributed among the peers.
-- The sequence of transactions generated consist only of simple self payments.
generateDemoUTxODataset ::
  NetworkId ->
  SocketPath ->
  -- | Number of clients
  [ClientKeys] ->
  -- | Number of transactions
  Int ->
  IO Dataset
generateDemoUTxODataset network nodeSocket allClientKeys nTxs = do
  (faucetVk, faucetSk) <- keysFor Faucet
  faucetUTxO <- queryUTxOFor network nodeSocket QueryTip faucetVk
  let (Coin fundsAvailable) = selectLovelace (balance @Tx faucetUTxO)
  -- Prepare funding transaction which will give every client's
  -- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
  -- funded in the beginning of the benchmark run.
  clientFunds <- generate $ genClientFunds allClientKeys fundsAvailable
  fundingTransaction <- do
    let changeAddress = mkVkAddress network faucetVk
    let recipientOutputs =
          flip map clientFunds $ \(vk, ll) ->
            TxOut
              (mkVkAddress network vk)
              (lovelaceToValue ll)
              TxOutDatumNone
              ReferenceScriptNone
    buildTransaction network nodeSocket changeAddress faucetUTxO [] recipientOutputs >>= \case
      Left e -> throwIO $ FaucetFailedToBuildTx{reason = e}
      Right body -> do
        let signedTx = sign faucetSk body
        pure signedTx
  let dataset clientKeys =
        generateClientDataset network fundingTransaction clientKeys nTxs generateOneSelfTransfer
  generate $ do
    clientDatasets <- forM allClientKeys dataset
    pure Dataset{fundingTransaction, clientDatasets, title = Nothing, description = Nothing}

-- * Helpers
thrd :: (a, b, c) -> c
thrd (_, _, c) = c

withInitialUTxO :: SigningKey PaymentKey -> Tx -> UTxO
withInitialUTxO externalSigningKey fundingTransaction =
  let vk = getVerificationKey externalSigningKey
   in -- NOTE: The initialUTxO must all UTXO we will later commit. We assume
      -- that everything owned by the externalSigningKey will get committed
      -- into the head.
      utxoProducedByTx fundingTransaction
        & UTxO.filter ((== mkVkAddress networkId vk) . txOutAddress)

genClientFunds :: [ClientKeys] -> Integer -> Gen [(VerificationKey PaymentKey, Coin)]
genClientFunds clientKeys availableFunds =
  forM clientKeys $ \ClientKeys{externalSigningKey} -> do
    amount <- Coin <$> choose (1, availableFunds `div` fromIntegral nClients)
    pure (getVerificationKey externalSigningKey, amount)
 where
  nClients = length clientKeys

generateClientDataset ::
  NetworkId ->
  Tx ->
  ClientKeys ->
  Int ->
  (NetworkId -> (UTxO, SigningKey PaymentKey, [Tx]) -> Int -> Gen (UTxO, SigningKey PaymentKey, [Tx])) ->
  Gen ClientDataset
generateClientDataset network fundingTransaction clientKeys@ClientKeys{externalSigningKey} nTxs action = do
  let initialUTxO = withInitialUTxO externalSigningKey fundingTransaction
  txSequence <-
    reverse
      . thrd
      <$> foldM (action network) (initialUTxO, externalSigningKey, []) [1 .. nTxs]
  pure ClientDataset{clientKeys, initialUTxO, txSequence}
