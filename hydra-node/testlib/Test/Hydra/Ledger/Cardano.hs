{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger.Cardano where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Secret (Secret, mkSecret, withSecret)
import Hydra.Cardano.Api hiding (initialLedgerState, utxoFromTx)
import Hydra.Ledger.Cardano (mkTransferTx)
import Hydra.Tx (IsTx (..))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor)
import Test.QuickCheck (choose, getSize, vectorOf)

-- * Generators

-- | Generates a sequence of simple "transfer" transactions for a single key.
genSequenceOfSimplePaymentTransactions :: Gen (UTxO, [Tx])
genSequenceOfSimplePaymentTransactions = do
  n <- getSize
  numTxs <- choose (1, n)
  genFixedSizeSequenceOfSimplePaymentTransactions numTxs

genFixedSizeSequenceOfSimplePaymentTransactions :: Int -> Gen (UTxO, [Tx])
genFixedSizeSequenceOfSimplePaymentTransactions numTxs = do
  (vk, sk) <- genKeyPair
  utxo <- genOneUTxOFor vk
  (_, txs) <- foldM (go sk) (utxo, []) [1 .. numTxs]
  pure (utxo, reverse txs)

go :: SigningKey PaymentKey -> (UTxO, [Tx]) -> Int -> Gen (UTxO, [Tx])
go sk (utxo, txs) _ = do
  case mkTransferTx testNetworkId utxo (mkSecret sk) (getVerificationKey sk) of
    Left err -> error $ "mkTransferTx failed: " <> err
    Right tx -> pure (utxoFromTx tx, tx : txs)

-- | Create a zero-fee, payment cardano transaction with validity range.
mkRangedTx ::
  (TxIn, TxOut CtxUTxO) ->
  -- | Recipient address and amount.
  (AddressInEra, Value) ->
  -- | Sender's signing key.
  Secret (SigningKey PaymentKey) ->
  (Maybe TxValidityLowerBound, Maybe TxValidityUpperBound) ->
  Either TxBodyError Tx
mkRangedTx (txin, TxOut owner valueIn datum refScript) (recipient, valueOut) sk (validityLowerBound, validityUpperBound) = do
  body <- createAndValidateTransactionBody bodyContent
  let witnesses = withSecret sk $ \rawSk -> [makeShelleyKeyWitness body (WitnessPaymentKey rawSk)]
  pure $ makeSignedTransaction witnesses body
 where
  bodyContent =
    defaultTxBodyContent
      { txIns = [(txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
      , txOuts =
          TxOut @CtxTx recipient valueOut TxOutDatumNone ReferenceScriptNone
            : [ fromCtxUTxOTxOut $
                TxOut
                  owner
                  (valueIn <> negateValue valueOut)
                  datum
                  refScript
              | valueOut /= valueIn
              ]
      , txFee = TxFeeExplicit $ Coin 0
      , txValidityLowerBound = fromMaybe TxValidityNoLowerBound validityLowerBound
      , txValidityUpperBound = fromMaybe TxValidityNoUpperBound validityUpperBound
      }

-- * Orphans

instance Arbitrary (Hash PaymentKey) where
  arbitrary = unsafePaymentKeyHashFromBytes . BS.pack <$> vectorOf 28 arbitrary
