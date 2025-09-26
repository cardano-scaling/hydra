{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano (
  module Hydra.Ledger.Cardano,
  module Hydra.Ledger.Cardano.Builder,
  Ledger.ShelleyGenesis (..),
  Ledger.Globals,
  Ledger.LedgerEnv,
  Tx,
) where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (initialLedgerState, utxoFromTx)
import Hydra.Ledger.Cardano.Builder

import Cardano.Api.Shelley (SigningKey, VerificationKey, getVerificationKey)
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Rules (
  FailureDescription (..),
  TagMismatchDescription (FailedUnexpectedly),
 )
import Cardano.Ledger.Api (bodyTxL, raCredential, unWithdrawals, withdrawalsTxBodyL)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.CertState (dsUnifiedL)
import Cardano.Ledger.Conway.Rules (
  ConwayLedgerPredFailure (ConwayUtxowFailure),
  ConwayUtxoPredFailure (UtxosFailure),
  ConwayUtxosPredFailure (ValidationTagMismatch),
  ConwayUtxowPredFailure (UtxoFailure),
 )
import Cardano.Ledger.Plutus (PlutusDebugOverrides (..), debugPlutus)
import Cardano.Ledger.Shelley.API.Mempool qualified as Ledger
import Cardano.Ledger.Shelley.Genesis qualified as Ledger
import Cardano.Ledger.Shelley.LedgerState qualified as Ledger
import Cardano.Ledger.Shelley.Rules qualified as Ledger
import Cardano.Ledger.UMap qualified as UM
import Control.Lens ((%~), (.~), (^.))
import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Tx (IsTx (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor)
import Test.QuickCheck (
  choose,
  getSize,
  vectorOf,
 )

-- * Ledger

-- | Use the cardano-ledger as an in-hydra 'Ledger'.
cardanoLedger :: Ledger.Globals -> Ledger.LedgerEnv LedgerEra -> Ledger Tx
cardanoLedger globals ledgerEnv =
  Ledger{applyTransactions}
 where
  -- NOTE(SN): See full note on 'applyTx' why we only have a single transaction
  -- application here.
  applyTransactions slot utxo = \case
    [] -> Right utxo
    (tx : txs) -> do
      utxo' <- applyTx slot utxo tx
      applyTransactions slot utxo' txs

  -- TODO(SN): Pre-validate transactions to get less confusing errors on
  -- transactions which are not expected to work on a layer-2
  -- NOTE(SN): This is will fail on any transaction requiring the 'DPState' to be
  -- in a certain state as we do throw away the resulting 'DPState' and only take
  -- the ledger's 'UTxO' forward.
  --
  -- We came to this signature of only applying a single transaction because we
  -- got confused why a sequence of transactions worked but sequentially applying
  -- single transactions didn't. This was because of this not-keeping the'DPState'
  -- as described above.
  applyTx (ChainSlot slot) utxo tx =
    case Ledger.applyTx globals env' memPoolState (toLedgerTx tx) of
      Left err ->
        Left (tx, toValidationError err)
      Right (Ledger.LedgerState{Ledger.lsUTxOState = us}, _validatedTx) ->
        Right . fromLedgerUTxO $ Ledger.utxosUtxo us
   where
    -- As we use applyTx we only expect one ledger rule to run and one tx to
    -- fail validation, hence using the heads of non empty lists is fine.
    toValidationError :: Ledger.ApplyTxError LedgerEra -> ValidationError
    toValidationError (Ledger.ApplyTxError (e :| _)) = case e of
      (ConwayUtxowFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch _ (FailedUnexpectedly (PlutusFailure msg ctx :| _)))))) ->
        ValidationError $
          "Plutus validation failed: "
            <> msg
            <> "Debug info: "
            -- NOTE: There is not a clear reason why 'debugPlutus' is an IO
            -- action. It only re-evaluates the script and does not have any
            -- side-effects.
            <> show (unsafeDupablePerformIO $ debugPlutus (decodeUtf8 ctx) $ PlutusDebugOverrides Nothing Nothing Nothing Nothing Nothing Nothing)
      _ -> ValidationError $ show e

    env' = ledgerEnv{Ledger.ledgerSlotNo = fromIntegral slot}

    memPoolState =
      def
        & Ledger.lsUTxOStateL . Ledger.utxoL .~ toLedgerUTxO utxo
        & Ledger.lsCertStateL . Ledger.certDStateL %~ mockCertState

    -- NOTE: Mocked certificate state that simulates any reward accounts for any
    -- withdraw-zero scripts included in the transaction.
    mockCertState = dsUnifiedL %~ (\umap -> foldl' register umap withdrawZeroCredentials)

    register umap hk = UM.RewDepUView umap UM.∪ (hk, UM.RDPair (UM.CompactCoin 0) (UM.CompactCoin 0))

    withdrawZeroCredentials =
      toLedgerTx tx ^. bodyTxL . withdrawalsTxBodyL
        & unWithdrawals
        & Map.filter (== Coin 0)
        & Map.keysSet
        & Set.map raCredential

-- * LedgerEnv

-- | Create a new ledger env from given protocol parameters.
newLedgerEnv :: PParams LedgerEra -> Ledger.LedgerEnv LedgerEra
newLedgerEnv protocolParams =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 0
    , -- NOTE: This can probably stay at 0 forever. This is used internally by the
      -- node's mempool to keep track of transaction seen from peers. Transactions
      -- in Hydra do not go through the node's mempool and follow a different
      -- consensus path so this will remain unused.
      Ledger.ledgerIx = minBound
    , -- NOTE: This keeps track of the ledger's treasury and reserve which are
      -- both unused in Hydra. There might be room for interesting features in the
      -- future with these two but for now, we'll consider them empty.
      Ledger.ledgerAccount = Ledger.AccountState mempty mempty
    , Ledger.ledgerPp = protocolParams
    , Ledger.ledgerEpochNo = Nothing
    }

-- * Conversions and utilities

-- | Simple conversion from a generic slot to a specific local one.
fromChainSlot :: ChainSlot -> SlotNo
fromChainSlot (ChainSlot s) = fromIntegral s

-- | Build a zero-fee transaction which spends the first output owned by given
-- signing key and transfers it in full to given verification key.
mkTransferTx ::
  MonadFail m =>
  NetworkId ->
  UTxO ->
  SigningKey PaymentKey ->
  VerificationKey PaymentKey ->
  m Tx
mkTransferTx networkId utxo sender recipient =
  case UTxO.find (isVkTxOut $ getVerificationKey sender) utxo of
    Nothing -> fail "no utxo left to spend"
    Just (txIn, txOut) ->
      case mkSimpleTx (txIn, txOut) (mkVkAddress networkId recipient, txOutValue txOut) sender of
        Left err ->
          fail $ "mkSimpleTx failed: " <> show err
        Right tx ->
          pure tx

-- | Build a zero-fee payment transaction.
mkSimpleTx ::
  (TxIn, TxOut CtxUTxO) ->
  -- | Recipient address and amount.
  (AddressInEra, Value) ->
  -- | Sender's signing key.
  SigningKey PaymentKey ->
  Either TxBodyError Tx
mkSimpleTx (txin, TxOut owner valueIn datum refScript) (recipient, valueOut) sk = do
  body <- createAndValidateTransactionBody bodyContent
  let witnesses = [makeShelleyKeyWitness body (WitnessPaymentKey sk)]
  pure $ makeSignedTransaction witnesses body
 where
  bodyContent =
    defaultTxBodyContent
      { txIns = [(txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)]
      , txOuts = outs
      , txFee = TxFeeExplicit fee
      }

  outs =
    TxOut @CtxTx recipient valueOut TxOutDatumNone ReferenceScriptNone
      : [ fromCtxUTxOTxOut $
          TxOut
            owner
            (valueIn <> negateValue valueOut)
            datum
            refScript
        | valueOut /= valueIn
        ]

  fee = Coin 0

-- | Create a zero-fee, payment cardano transaction with validity range.
mkRangedTx ::
  (TxIn, TxOut CtxUTxO) ->
  -- | Recipient address and amount.
  (AddressInEra, Value) ->
  -- | Sender's signing key.
  SigningKey PaymentKey ->
  (Maybe TxValidityLowerBound, Maybe TxValidityUpperBound) ->
  Either TxBodyError Tx
mkRangedTx (txin, TxOut owner valueIn datum refScript) (recipient, valueOut) sk (validityLowerBound, validityUpperBound) = do
  body <- createAndValidateTransactionBody bodyContent
  let witnesses = [makeShelleyKeyWitness body (WitnessPaymentKey sk)]
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

-- | Utility function to "adjust" a `UTxO` set given a `Tx`
--
--  The inputs from the `Tx` are removed from the internal map of the `UTxO` and
--  the outputs added, correctly indexed by the `TxIn`. This function is useful
--  to manually maintain a `UTxO` set without caring too much about the `Ledger`
--  rules.
adjustUTxO :: Tx -> UTxO -> UTxO
adjustUTxO tx utxo =
  let txid = txId tx
      consumed = txIns' tx
      produced = UTxO.fromList ((\(txout, ix) -> (TxIn txid (TxIx ix), toCtxUTxOTxOut txout)) <$> zip (txOuts' tx) [0 ..])
      utxo' = UTxO.fromList $ filter (\(txin, _) -> txin `notElem` consumed) $ UTxO.toList utxo
   in utxo' <> produced

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
 where
  -- Magic number is irrelevant.
  testNetworkId = Testnet $ NetworkMagic 42

  go :: SigningKey PaymentKey -> (UTxO, [Tx]) -> Int -> Gen (UTxO, [Tx])
  go sk (utxo, txs) _ = do
    case mkTransferTx testNetworkId utxo sk (getVerificationKey sk) of
      Left err -> error $ "mkTransferTx failed: " <> err
      Right tx -> pure (utxoFromTx tx, tx : txs)

-- * Orphans

instance Arbitrary (Hash PaymentKey) where
  arbitrary = unsafePaymentKeyHashFromBytes . BS.pack <$> vectorOf 28 arbitrary
