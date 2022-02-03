{-# LANGUAGE TypeApplications #-}

module Hydra.Generator where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (size)

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (mkGenesisTx)
import CardanoCluster (availableInitialFunds)
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Default (def)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (CardanoTx, genKeyPair, mkSimpleCardanoTx)
import Test.QuickCheck (choose, generate, sized)

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

-- | A 'Dataset' that can be run for testing purpose.
-- The 'transactionSequence' is guaranteed to be applicable, in sequence, to the 'initialUTxO'
-- set.
data Dataset = Dataset
  { fundingTransaction :: CardanoTx
  , transactionsSequence :: [CardanoTx]
  , signingKey :: SigningKey PaymentKey
  }
  deriving (Show, Generic)

defaultProtocolParameters :: ProtocolParameters
defaultProtocolParameters = fromLedgerPParams ShelleyBasedEraShelley def

instance Arbitrary Dataset where
  arbitrary = sized (genConstantUTxODataset defaultProtocolParameters)

instance ToJSON Dataset where
  toJSON Dataset{fundingTransaction, transactionsSequence, signingKey} =
    object
      [ "fundingTransaction" .= fundingTransaction
      , "transactionsSequence" .= transactionsSequence
      , "signingKey" .= serialiseToBech32 signingKey
      ]

instance FromJSON Dataset where
  parseJSON =
    withObject "Dataset" $ \o ->
      Dataset <$> o .: "fundingTransaction"
        <*> o .: "transactionsSequence"
        <*> (decodeSigningKey =<< o .: "signingKey")
   where
    decodeSigningKey =
      either (fail . show) pure . deserialiseFromBech32 (AsSigningKey AsPaymentKey)

-- | Generate a 'Dataset' which does not grow the UTXO set over time.
-- The sequence of transactions generated consist only of simple payments from and to
-- arbitrary keys controlled by the "client".
generateConstantUTxODataset :: ProtocolParameters -> Int -> IO Dataset
generateConstantUTxODataset pparams = generate . genConstantUTxODataset pparams

genConstantUTxODataset :: ProtocolParameters -> Int -> Gen Dataset
genConstantUTxODataset pparams len = do
  keyPair@(verificationKey, signingKey) <- genKeyPair
  amount <- choose (1, availableInitialFunds `div` 2)
  let fundingTransaction =
        mkGenesisTx
          networkId
          pparams
          (Lovelace availableInitialFunds)
          signingKey
          verificationKey
          (Lovelace amount)
  -- NOTE: The initialUTxO must contain only the UTXO we will later commit. We
  -- know that by construction, the 'mkGenesisTx' will have exactly two outputs,
  -- the last one being the change output. So, it suffices to lookup for the
  -- minimum key in the utxo map to isolate the commit UTXO.
  let initialUTxO = UTxO.min $ utxoFromTx fundingTransaction
  transactionsSequence <-
    reverse . thrd
      <$> foldM generateOneTransfer (initialUTxO, keyPair, []) [1 .. len]
  pure Dataset{fundingTransaction, transactionsSequence, signingKey}
 where
  thrd (_, _, c) = c

  generateOneTransfer ::
    (UTxO, (VerificationKey PaymentKey, SigningKey PaymentKey), [CardanoTx]) ->
    Int ->
    Gen (UTxO, (VerificationKey PaymentKey, SigningKey PaymentKey), [CardanoTx])
  generateOneTransfer (utxo, (_, sender), txs) _ = do
    recipient <- genKeyPair
    -- NOTE(AB): elements is partial, it crashes if given an empty list, We don't expect
    -- this function to be ever used in production, and crash will be caught in tests
    case UTxO.pairs utxo of
      [txin] ->
        case mkSimpleCardanoTx txin (mkVkAddress networkId (fst recipient), balance @CardanoTx utxo) sender of
          Left e -> error $ "Tx construction failed: " <> show e <> ", utxo: " <> show utxo
          Right tx ->
            pure (utxoFromTx tx, recipient, tx : txs)
      _ ->
        error "Couldn't generate transaction sequence: need exactly one UTXO."

mkCredentials :: Int -> (VerificationKey PaymentKey, SigningKey PaymentKey)
mkCredentials = generateWith genKeyPair
