module Hydra.Generator where

import Hydra.Cardano.Api hiding (getVerificationKey, signTx)
import Hydra.Prelude hiding (size)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (QueryPoint (QueryTip), localNodeConnectInfo, mkGenesisTx, queryUTxOFor)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.List qualified as List
import Hydra.Chain.Backend (buildTransaction)
import Hydra.Chain.Direct (runDirectBackend)
import Hydra.Cluster.Faucet (FaucetException (..))
import Hydra.Cluster.Fixture (availableInitialFunds)
import Hydra.Ledger.Cardano (mkSimpleTx, mkTransferTx)
import Hydra.Options qualified as Options
import Hydra.Tx.Crypto (getVerificationKey, signTx)
import Hydra.Tx.Secret (Secret, mkSecret, withSecret)
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
  , hydraNodeKeys :: [Secret (SigningKey PaymentKey)]
  -- ^ Cardano signing keys that will hold fuel.
  , clientDatasets :: [ClientDataset]
  , title :: Maybe Text
  , description :: Maybe Text
  }
  deriving stock (Show, Generic)

-- NOTE: Hand-written ToJSON and FromJSON instances to deliberately
-- serialize signing keys. The 'withSecret' / 'mkSecret' bookends make
-- the serialisation an explicit, grep-able escape from the 'Secret'
-- wrapper. These datasets are written to disk by the benchmark
-- machinery (see 'hydra-cluster/bench/'), and that is the one place
-- where we knowingly let signing-key bytes leave the in-memory wrapper.

instance ToJSON Dataset where
  toJSON Dataset{fundingTransaction, hydraNodeKeys, clientDatasets, title, description} =
    object
      [ "fundingTransaction" .= fundingTransaction
      , "hydraNodeKeys" .= (withSecret `flip` serialiseToTextEnvelope (Just "hydraNodeKey") <$> hydraNodeKeys)
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
      fmap mkSecret . either (fail . show) pure . deserialiseFromTextEnvelope

instance Arbitrary Dataset where
  arbitrary = sized $ \n -> do
    sk <- genSigningKey
    generateConstantUTxODataset (mkSecret sk) (n `div` 10) n

data ClientDataset = ClientDataset
  { paymentKey :: Secret (SigningKey PaymentKey)
  , initialUTxO :: UTxO
  , txSequence :: [Tx]
  }
  deriving stock (Show, Generic)

instance ToJSON ClientDataset where
  toJSON ClientDataset{paymentKey, initialUTxO, txSequence} =
    object
      [ "paymentKey" .= withSecret paymentKey (serialiseToTextEnvelope (Just "paymentKey"))
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
      fmap mkSecret . either (fail . show) pure . deserialiseFromTextEnvelope

-- | Generate a 'Dataset' which does not grow the per-client UTXO set over time.
-- This version provided faucet key owns funds on the initial funds of the
-- devnet (See 'availableInitialFunds' and 'genesis-shelley.json'). Then for a
-- given number of clients a number of transactions are generated.
generateConstantUTxODataset ::
  -- | Faucet signing key
  Secret (SigningKey PaymentKey) ->
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
        mkGenesisTx networkId faucetSk (Coin availableInitialFunds) clientFunds
  clientDatasets <- forM allPaymentKeys (generateClientDataset networkId fundingTransaction nTxs)
  pure
    Dataset
      { fundingTransaction
      , hydraNodeKeys = mkSecret <$> hydraNodeKeys
      , clientDatasets
      , title = Nothing
      , description = Nothing
      }

generateGrowingUTxODataset ::
  -- | Faucet signing key
  Secret (SigningKey PaymentKey) ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  Gen Dataset
generateGrowingUTxODataset faucetSk nClients nTxs = do
  -- TODO: DRY
  hydraNodeKeys <- replicateM nClients genSigningKey
  allPaymentKeys <- replicateM nClients genSigningKey
  -- Prepare funding transaction which will give every client's
  -- 'externalSigningKey' "some" lovelace. The internal 'signingKey' will get
  -- funded in the beginning of the benchmark run.
  clientFunds <- genClientFunds allPaymentKeys availableInitialFunds
  let fundingTransaction =
        mkGenesisTx networkId faucetSk (Coin availableInitialFunds) clientFunds
  clientDatasets <- forM allPaymentKeys (genClientDataset fundingTransaction)
  pure
    Dataset
      { fundingTransaction
      , hydraNodeKeys = mkSecret <$> hydraNodeKeys
      , clientDatasets
      , title = Nothing
      , description = Nothing
      }
 where
  genClientDataset :: Tx -> SigningKey PaymentKey -> Gen ClientDataset
  genClientDataset fundingTransaction paymentKey = do
    let initialUTxO = withInitialUTxO paymentKey fundingTransaction
    let (_, txs) = foldl' (genTx paymentKey) (initialUTxO, []) [1 .. nTxs]
    pure ClientDataset{paymentKey = mkSecret paymentKey, initialUTxO, txSequence = reverse txs}

  genTx :: SigningKey PaymentKey -> (UTxO.UTxO Era, [Tx]) -> Int -> (UTxO.UTxO Era, [Tx])
  genTx sk (utxo, txs) _tx = do
    let vk = getVerificationKey sk
    case UTxO.find (isVkTxOut vk) utxo of
      Nothing -> error "no utxo left to spend"
      Just (txIn, txOut) -> do
        let aBitLess = txOutValue txOut <> negateValue (lovelaceToValue 2_000_000)
        case mkSimpleTx (txIn, txOut) (mkVkAddress networkId vk, aBitLess) (mkSecret sk) of
          Left err ->
            error $ "mkSimpleTx failed: " <> show err
          Right tx -> (utxoFromTx tx, tx : txs)

-- | Generate a 'Dataset' that grows the head's UTxO set for the first half of
-- the tx sequence and contracts it again for the second half. Phase 1 reuses
-- the same per-tx pattern as 'generateGrowingUTxODataset' (1-in -> 2-out via
-- 'mkSimpleTx' with reduced output value). Phase 2 issues 2-in -> 1-out
-- merges that consume two previously created outputs and produce a single
-- combined output. End-state has a single UTxO again.
generateMixedUTxODataset ::
  -- | Faucet signing key
  SigningKey PaymentKey ->
  -- | Number of clients
  Int ->
  -- | Number of transactions
  Int ->
  Gen Dataset
generateMixedUTxODataset faucetSk nClients nTxs = do
  hydraNodeKeys <- replicateM nClients genSigningKey
  allPaymentKeys <- replicateM nClients genSigningKey
  clientFunds <- genClientFunds allPaymentKeys availableInitialFunds
  let fundingTransaction =
        mkGenesisTx networkId faucetSk (Coin availableInitialFunds) clientFunds
  clientDatasets <- forM allPaymentKeys (genClientDataset fundingTransaction)
  pure
    Dataset
      { fundingTransaction
      , hydraNodeKeys
      , clientDatasets
      , title = Just "Mixed UTxO Scenario"
      , description =
          Just
            "Each client first grows its UTxO set (1-in to 2-out) for half of \
            \its tx budget, then contracts it back (2-in to 1-out) for the \
            \remainder."
      }
 where
  growSteps = nTxs `div` 2
  contractSteps = nTxs - growSteps

  genClientDataset :: Tx -> SigningKey PaymentKey -> Gen ClientDataset
  genClientDataset fundingTransaction paymentKey = do
    let initialUTxO = withInitialUTxO paymentKey fundingTransaction
    let vk = getVerificationKey paymentKey
    let (afterGrow, growTxs) =
          foldl' (genGrowTx paymentKey vk) (initialUTxO, []) [1 .. growSteps]
    let (_, contractTxs) =
          foldl' (genContractTx paymentKey vk) (afterGrow, []) [1 .. contractSteps]
    pure
      ClientDataset
        { paymentKey
        , initialUTxO
        , txSequence = reverse growTxs ++ reverse contractTxs
        }

  -- Splits 4 ADA off the largest available UTxO so the change output is large
  -- enough that subsequent iterations can spend it safely.
  growChunk :: Coin
  growChunk = Coin 4_000_000

  -- The grow phase consumes the largest VK-owned UTxO each iteration. Picking
  -- 'UTxO.find' instead would eventually land on one of the small change
  -- outputs from earlier iterations, producing a negative-value txout when
  -- 'growChunk' is subtracted.
  genGrowTx ::
    SigningKey PaymentKey ->
    VerificationKey PaymentKey ->
    (UTxO.UTxO Era, [Tx]) ->
    Int ->
    (UTxO.UTxO Era, [Tx])
  genGrowTx sk vk (utxo, txs) _ =
    case largestVkUTxO vk utxo of
      Nothing -> error "mixed/grow: no utxo left to spend"
      Just (txIn, txOut)
        | selectLovelace (txOutValue txOut) <= growChunk ->
            error $ "mixed/grow: largest VK utxo (" <> show (txOutValue txOut) <> ") is too small to split off " <> show growChunk
        | otherwise ->
            let chunkValue = lovelaceToValue growChunk
             in case mkSimpleTx (txIn, txOut) (mkVkAddress networkId vk, chunkValue) sk of
                  Left err -> error $ "mixed/grow mkSimpleTx failed: " <> show err
                  Right tx ->
                    let remaining = UTxO.difference utxo (UTxO.singleton txIn txOut)
                     in (remaining <> utxoFromTx tx, tx : txs)

  genContractTx ::
    SigningKey PaymentKey ->
    VerificationKey PaymentKey ->
    (UTxO.UTxO Era, [Tx]) ->
    Int ->
    (UTxO.UTxO Era, [Tx])
  genContractTx sk vk (utxo, txs) _ =
    case take 2 (UTxO.toList (UTxO.filter (isVkTxOut vk) utxo)) of
      [(in1, out1), (in2, out2)] ->
        case mkMergeTx networkId sk (in1, out1) (in2, out2) of
          Left err -> error $ "mixed/contract mkMergeTx failed: " <> show err
          Right tx ->
            let spent = UTxO.fromList [(in1, out1), (in2, out2)]
                remaining = UTxO.difference utxo spent
             in (remaining <> utxoFromTx tx, tx : txs)
      _ -> error "mixed/contract: need at least 2 utxos to merge"

  largestVkUTxO :: VerificationKey PaymentKey -> UTxO.UTxO Era -> Maybe (TxIn, TxOut CtxUTxO)
  largestVkUTxO vk =
    let byLovelace :: (TxIn, TxOut CtxUTxO) -> Coin
        byLovelace (_, o) = selectLovelace (txOutValue o)
     in fmap (List.maximumBy (comparing byLovelace)) . nonEmpty . UTxO.toList . UTxO.filter (isVkTxOut vk)

-- | Build a zero-fee 2-in 1-out transaction that merges two of a sender's own
-- outputs into one. Used by the Mixed generator to contract the UTxO set.
mkMergeTx ::
  NetworkId ->
  SigningKey PaymentKey ->
  (TxIn, TxOut CtxUTxO) ->
  (TxIn, TxOut CtxUTxO) ->
  Either TxBodyError Tx
mkMergeTx network sk (in1, out1) (in2, out2) = do
  body <- createAndValidateTransactionBody bodyContent
  let witnesses = [makeShelleyKeyWitness body (WitnessPaymentKey sk)]
  pure $ makeSignedTransaction witnesses body
 where
  vk = getVerificationKey sk
  combined = txOutValue out1 <> txOutValue out2
  bodyContent =
    defaultTxBodyContent
      { txIns =
          [ (in1, BuildTxWith $ KeyWitness KeyWitnessForSpending)
          , (in2, BuildTxWith $ KeyWitness KeyWitnessForSpending)
          ]
      , txOuts =
          [ TxOut @CtxTx
              (mkVkAddress network vk)
              combined
              TxOutDatumNone
              ReferenceScriptNone
          ]
      , txFee = TxFeeExplicit (Coin 0)
      }

-- | Generate a 'Dataset' from an already running network by querying available
-- funds of the well-known 'faucet.sk' and assuming the hydra-nodes we connect
-- to have fuel available. Then for a given number of clients a number of
-- transactions are generated.
generateDemoUTxODataset ::
  NetworkId ->
  SocketPath ->
  -- | Faucet signing key
  Secret (SigningKey PaymentKey) ->
  -- | Number of clients.
  Int ->
  -- | Number of transactions
  Int ->
  IO Dataset
generateDemoUTxODataset network nodeSocket faucetSk nClients nTxs = do
  -- Query available funds
  faucetUTxO <-
    queryUTxOFor (localNodeConnectInfo network nodeSocket) QueryTip faucetVk
  let (Coin fundsAvailable) = UTxO.totalLovelace faucetUTxO
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

    runDirectBackend Options.DirectOptions{Options.networkId = network, Options.nodeSocket} (buildTransaction faucetAddress faucetUTxO [] recipientOutputs) >>= \case
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
  pure ClientDataset{paymentKey = mkSecret paymentKey, initialUTxO, txSequence = reverse txs}
 where
  go sk (utxo, txs) _ = do
    case mkTransferTx network utxo (mkSecret sk) (getVerificationKey sk) of
      Left err -> error $ "mkTransferTx failed: " <> err
      Right tx -> pure (utxoFromTx tx, tx : txs)
