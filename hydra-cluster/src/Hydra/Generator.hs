module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import Cardano.Api.Ledger (PParams)
import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (QueryPoint (QueryTip), buildRawTransaction, buildTransaction, queryProtocolParameters, queryUTxOFor, sign)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Default (def)
import Data.List qualified as List
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Cluster.Faucet (FaucetException (..))
import Hydra.Cluster.Fixture (Actor (..), availableInitialFunds, defaultNetworkId)
import Hydra.Cluster.Util (keysFor)
import Hydra.Ledger (balance)
import Hydra.Ledger.Cardano (genSigningKey, generateOneRandomTransfer, generateOneSelfTransfer)
import Test.QuickCheck (choose, generate, sized)

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

-- | A 'Dataset' that can be run for testing purpose.
-- Each `Dataset` represents a complete scenario where several `ClientDataset` are run concurrently
-- against one or more `HydraNode`s. A dataset can optionally have a `title` and `description`
-- which will be used to report results.
data Dataset = Dataset
  { fundingTransaction :: Maybe Tx
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
        buildRawTransaction
          networkId
          initialInput
          faucetSk
          (Coin availableInitialFunds)
          clientFunds
  clientDatasets <- forM clientKeys (generateClientDataset fundingTransaction)
  pure Dataset{fundingTransaction = Just fundingTransaction, clientDatasets, title = Nothing, description = Nothing}
 where
  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (unsafeCastHash $ verificationKeyHash $ getVerificationKey faucetSk)

  generateClientDataset fundingTransaction clientKeys@ClientKeys{externalSigningKey} = do
    let initialUTxO = withInitialUTxO externalSigningKey fundingTransaction
    txSequence <-
      reverse
        . thrd
        <$> foldM (generateOneRandomTransfer networkId) (initialUTxO, externalSigningKey, []) [1 .. nTxs]
    pure ClientDataset{clientKeys, initialUTxO, txSequence}

generateDemoUTxODataset ::
  Int ->
  SocketPath ->
  IO Dataset
generateDemoUTxODataset nTxns nodeSocket = do
  -- FIXME: Client keys hard-coded; could read from arguments.
  clientKeys <- do
    let actors = [(Alice, AliceFunds), (Bob, BobFunds), (Carol, CarolFunds)]
    let toClientKeys (actor, funds) = do
          sk <- snd <$> keysFor actor
          fundsSk <- snd <$> keysFor funds
          pure $ ClientKeys sk fundsSk
    forM actors toClientKeys
  clientDatasets <- forM clientKeys generateClientDataset
  pure $ Dataset{fundingTransaction = Nothing, clientDatasets = clientDatasets, title = Nothing, description = Nothing}
 where
  generateClientDataset :: ClientKeys -> IO ClientDataset
  generateClientDataset clientKeys@ClientKeys{signingKey, externalSigningKey} = do
    -- TODO: Missing witnesses.
    -- bench-e2e: SubmitTxValidationError (TxValidationErrorInCardanoMode (ShelleyTxValidationError ShelleyBasedEraBabbage (Appl6ace848155a0e967af64f4d00cf8acee8adc95a6b0d"}])))) :| []))))
    let clientVk = getVerificationKey signingKey
    initialUTxO <- queryUTxOFor networkId nodeSocket QueryTip clientVk
    -- FIXME: Improve error
    when (null initialUTxO) $ do
      error "No initial UTxOs. Did you seed your devnet?"
    generate $ do
      txSequence <-
        reverse . thrd <$> foldM (generateOneRandomTransfer networkId) (initialUTxO, externalSigningKey, mempty) [1 .. nTxns]
      pure ClientDataset{clientKeys, initialUTxO, txSequence}

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
