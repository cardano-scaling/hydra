{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Generator where

import Hydra.Prelude hiding (size)

import Cardano.Api
import Control.Monad (foldM)
import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (
  CardanoTx,
  Utxo,
  genKeyPair,
  mkGenesisTx,
  mkSimpleCardanoTx,
  mkVkAddress,
  utxoFromTx,
  utxoPairs,
 )
import Test.QuickCheck (choose, elements, generate, sized)

networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

-- | A 'Dataset' that can be run for testing purpose.
-- The 'transactionSequence' is guaranteed to be applicable, in sequence, to the 'initialUtxo'
-- set.
data Dataset = Dataset
  { fundingTransaction :: CardanoTx
  , transactionsSequence :: [CardanoTx]
  , signingKey :: SigningKey PaymentKey
  }
  deriving (Show, Generic)

instance Arbitrary Dataset where
  arbitrary = sized genConstantUtxoDataset

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
generateConstantUtxoDataset :: Int -> IO Dataset
generateConstantUtxoDataset = generate . genConstantUtxoDataset

genConstantUtxoDataset :: Int -> Gen Dataset
genConstantUtxoDataset len = do
  keyPair@(verificationKey, signingKey) <- genKeyPair
  amount <- choose (1, availableInitialFunds `div` 2)
  let fundingTransaction =
        mkGenesisTx
          networkId
          (Lovelace availableInitialFunds)
          signingKey
          verificationKey
          (Lovelace amount)
  let initialUtxo = utxoFromTx fundingTransaction
  transactionsSequence <-
    reverse . thrd
      <$> foldM generateOneTransfer (initialUtxo, keyPair, []) [1 .. len]
  pure Dataset{fundingTransaction, transactionsSequence, signingKey}
 where
  thrd (_, _, c) = c

  -- FIXME: This is hard-coded and should correspond to the initial funds set in
  -- the genesis file.
  availableInitialFunds = 9_000_000_000

  generateOneTransfer ::
    (Utxo, (VerificationKey PaymentKey, SigningKey PaymentKey), [CardanoTx]) ->
    Int ->
    Gen (Utxo, (VerificationKey PaymentKey, SigningKey PaymentKey), [CardanoTx])
  generateOneTransfer (utxo, (_, sender), txs) _ = do
    recipient <- genKeyPair
    -- NOTE(AB): elements is partial, it crashes if given an empty list, We don't expect
    -- this function to be ever used in production, and crash will be caught in tests
    txin <- elements $ utxoPairs utxo
    case mkSimpleCardanoTx txin (mkVkAddress networkId (fst recipient), balance @CardanoTx utxo) sender of
      Left e -> error $ "Tx construction failed: " <> show e
      Right tx ->
        pure (utxoFromTx tx, recipient, tx : txs)

mkCredentials :: Int -> (VerificationKey PaymentKey, SigningKey PaymentKey)
mkCredentials = generateWith genKeyPair
