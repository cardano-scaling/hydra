{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano (
  module Hydra.Ledger.Cardano,
  module Hydra.Ledger.Cardano.Builder,
  Ledger.ShelleyGenesis (..),
  Tx,
) where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (initialLedgerState)
import Hydra.Ledger.Cardano.Builder

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (decodeAnnotator, serialize')
import qualified Cardano.Crypto.DSIGN as CC
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import qualified Hydra.Contract.Head as Head
import Hydra.Ledger (ChainSlot (..), IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Json ()
import PlutusLedgerApi.V2 (fromBuiltin)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.QuickCheck (
  choose,
  getSize,
  listOf,
  scale,
  shrinkList,
  shrinkMapBy,
  suchThat,
  vectorOf,
 )

-- * Ledger

-- | Use the cardano-ledger as an in-hydra 'Ledger'.
cardanoLedger :: Ledger.Globals -> Ledger.LedgerEnv LedgerEra -> Ledger Tx
cardanoLedger globals ledgerEnv =
  Ledger
    { applyTransactions
    , initUTxO = mempty
    }
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
        Right . fromLedgerUTxO $ Ledger._utxo us
   where
    toValidationError = ValidationError . show

    env' = ledgerEnv{Ledger.ledgerSlotNo = fromIntegral slot}

    memPoolState =
      Ledger.LedgerState
        { Ledger.lsUTxOState = def{Ledger._utxo = toLedgerUTxO utxo}
        , Ledger.lsDPState = def
        }

-- * Cardano Tx

instance IsTx Tx where
  type TxIdType Tx = TxId
  type UTxOType Tx = UTxO
  type ValueType Tx = Value

  txId = getTxId . getTxBody
  balance = foldMap txOutValue
  hashUTxO = fromBuiltin . Head.hashTxOuts . mapMaybe toPlutusTxOut . toList

instance ToCBOR Tx where
  toCBOR = CBOR.encodeBytes . serialize' . toLedgerTx

instance FromCBOR Tx where
  fromCBOR = do
    bs <- CBOR.decodeBytes
    decodeAnnotator "Tx" fromCBOR (fromStrict bs)
      & either
        (fail . toString . toLazyText . build)
        (pure . fromLedgerTx)

instance ToJSON Tx where
  toJSON = toJSON . toLedgerTx

instance FromJSON Tx where
  parseJSON = fmap fromLedgerTx . parseJSON

instance Arbitrary Tx where
  -- TODO: shrinker!
  arbitrary = fromLedgerTx . withoutProtocolUpdates <$> arbitrary
   where
    withoutProtocolUpdates tx@(Ledger.AlonzoTx body _ _ _) =
      let body' = body{Ledger.txUpdates = SNothing}
       in tx{Ledger.body = body'}

-- | Create a zero-fee, payment cardano transaction.
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
    emptyTxBody
      { txIns = map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
      , txOuts = outs
      , txFee = TxFeeExplicit fee
      }

  outs =
    TxOut @CtxTx recipient valueOut TxOutDatumNone ReferenceScriptNone
      : [ TxOut @CtxTx
          owner
          (valueIn <> negateValue valueOut)
          (toTxContext datum)
          refScript
        | valueOut /= valueIn
        ]

  fee = Lovelace 0

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
    emptyTxBody
      { txIns = map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
      , txOuts =
          TxOut @CtxTx recipient valueOut TxOutDatumNone ReferenceScriptNone
            : [ TxOut @CtxTx
                owner
                (valueIn <> negateValue valueOut)
                (toTxContext datum)
                refScript
              | valueOut /= valueIn
              ]
      , txFee = TxFeeExplicit $ Lovelace 0
      , txValidityRange =
          ( fromMaybe TxValidityNoLowerBound validityLowerBound
          , fromMaybe TxValidityNoUpperBound validityUpperBound
          )
      }

-- * Generators

genSigningKey :: Gen (SigningKey PaymentKey)
genSigningKey = do
  -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
  -- play well with pure generation from seed.
  sk <- fromJust . CC.rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 64 arbitrary
  pure (PaymentSigningKey sk)

genVerificationKey :: Gen (VerificationKey PaymentKey)
genVerificationKey = getVerificationKey <$> genSigningKey

genKeyPair :: Gen (VerificationKey PaymentKey, SigningKey PaymentKey)
genKeyPair = do
  sk <- genSigningKey
  pure (getVerificationKey sk, sk)

-- | Generates a sequence of simple "transfer" transactions for a single key.
-- The kind of transactions produced by this generator is very limited, see `generateOneTransfer`.
genSequenceOfSimplePaymentTransactions :: Gen (UTxO, [Tx])
genSequenceOfSimplePaymentTransactions = do
  n <- getSize
  numTxs <- choose (1, n)
  genFixedSizeSequenceOfSimplePaymentTransactions numTxs

genFixedSizeSequenceOfSimplePaymentTransactions :: Int -> Gen (UTxO, [Tx])
genFixedSizeSequenceOfSimplePaymentTransactions numTxs = do
  keyPair@(vk, _) <- genKeyPair
  utxo <- genOneUTxOFor vk
  txs <-
    reverse . thrd
      <$> foldM (generateOneTransfer testNetworkId) (utxo, keyPair, []) [1 .. numTxs]
  pure (utxo, txs)
 where
  thrd (_, _, c) = c
  testNetworkId = Testnet $ NetworkMagic 42

generateOneTransfer ::
  NetworkId ->
  (UTxO, (VerificationKey PaymentKey, SigningKey PaymentKey), [Tx]) ->
  Int ->
  Gen (UTxO, (VerificationKey PaymentKey, SigningKey PaymentKey), [Tx])
generateOneTransfer networkId (utxo, (_, sender), txs) _ = do
  recipient <- genKeyPair
  -- NOTE(AB): elements is partial, it crashes if given an empty list, We don't expect
  -- this function to be ever used in production, and crash will be caught in tests
  case UTxO.pairs utxo of
    [txin] ->
      case mkSimpleTx txin (mkVkAddress networkId (fst recipient), balance @Tx utxo) sender of
        Left e -> error $ "Tx construction failed: " <> show e <> ", utxo: " <> show utxo
        Right tx ->
          pure (utxoFromTx tx, recipient, tx : txs)
    _ ->
      error "Couldn't generate transaction sequence: need exactly one UTXO."

-- TODO: Enable arbitrary datum in generators
-- TODO: This should better be called 'genOutputFor'
genOutput ::
  forall ctx.
  VerificationKey PaymentKey ->
  Gen (TxOut ctx)
genOutput vk = do
  value <- genValue
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone ReferenceScriptNone

-- | Generate an ada-only 'TxOut' payed to an arbitrary public key.
genTxOutAdaOnly :: VerificationKey PaymentKey -> Gen (TxOut ctx)
genTxOutAdaOnly vk = do
  value <- lovelaceToValue . Lovelace <$> scale (* 8) arbitrary `suchThat` (> 0)
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone ReferenceScriptNone

-- | Generate a fixed size UTxO with ada-only outputs.
genUTxOAdaOnlyOfSize :: Int -> Gen UTxO
genUTxOAdaOnlyOfSize numUTxO =
  fold <$> vectorOf numUTxO (UTxO.singleton <$> gen)
 where
  gen = (,) <$> arbitrary <*> (genTxOutAdaOnly =<< arbitrary)

-- | Generate 'Babbage' era 'UTxO', which may contain arbitrary assets in
-- 'TxOut's addressed to public keys *and* scripts. NOTE: This is not reducing
-- size when generating assets in 'TxOut's, so will end up regularly with 300+
-- assets with generator size 30. NOTE: The Arbitrary TxIn instance from the
-- ledger is producing colliding values, so we replace them.
genUTxOAlonzo :: Gen UTxO
genUTxOAlonzo = do
  utxoMap <- Map.toList . Ledger.unUTxO <$> arbitrary
  fmap UTxO.fromPairs . forM utxoMap $ \(_, o) -> do
    i <- arbitrary
    pure (i, fromLedgerTxOut o)

-- | Generate a 'Babbage' era 'UTxO' with given number of outputs. See also
-- 'genTxOut'.
genUTxOSized :: Int -> Gen UTxO
genUTxOSized numUTxO =
  fold <$> vectorOf numUTxO (UTxO.singleton <$> gen)
 where
  gen = (,) <$> arbitrary <*> genTxOut

-- | Genereate a 'UTxO' with a single entry using given 'TxOut' generator.
genUTxO1 :: Gen (TxOut CtxUTxO) -> Gen UTxO
genUTxO1 gen = do
  txIn <- arbitrary
  txOut <- gen
  pure $ UTxO.singleton (txIn, txOut)

-- | Generate a 'Babbage' era 'TxOut', which may contain arbitrary assets
-- addressed to public keys and scripts, as well as datums.
--
-- NOTE: This generator does
--  * not produce byron addresses as most of the cardano ecosystem dropped support for that (including plutus),
--  * not produce reference scripts as they are not fully "visible" from plutus,
--  * replace stake pointers with null references as nobody uses that.
genTxOut :: Gen (TxOut ctx)
genTxOut =
  (noRefScripts . noStakeRefPtr . fromLedgerTxOut <$> arbitrary)
    `suchThat` notByronAddress
 where
  notByronAddress (TxOut addr _ _ _) = case addr of
    ByronAddressInEra{} -> False
    _ -> True

  noStakeRefPtr out@(TxOut addr val dat refScript) = case addr of
    ShelleyAddressInEra (ShelleyAddress _ cre sr) ->
      case sr of
        Ledger.StakeRefPtr _ ->
          TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat refScript
        _ ->
          TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre sr)) val dat refScript
    _ -> out

  noRefScripts out =
    out{txOutReferenceScript = ReferenceScriptNone}

-- | Generate a 'TxOut' with a byron address. This is usually not supported by
-- Hydra or Plutus.
genTxOutByron :: Gen (TxOut ctx)
genTxOutByron = do
  addr <- ByronAddressInEra <$> arbitrary
  value <- genValue
  pure $ TxOut addr value TxOutDatumNone ReferenceScriptNone

-- | Generate a 'TxOut' with a reference script. The standard 'genTxOut' is not
-- including reference scripts, use this generator if you are interested in
-- these cases.
genTxOutWithReferenceScript :: Gen (TxOut ctx)
genTxOutWithReferenceScript = do
  -- Have the ledger generate a TxOut with a reference script as instances are
  -- not so easily accessible.
  refScript <- (txOutReferenceScript . fromLedgerTxOut <$> arbitrary) `suchThat` (/= ReferenceScriptNone)
  genTxOut <&> \out -> out{txOutReferenceScript = refScript}

-- | Generate utxos owned by the given cardano key.
genUTxOFor :: VerificationKey PaymentKey -> Gen UTxO
genUTxOFor vk = do
  n <- arbitrary `suchThat` (> 0)
  inps <- vectorOf n arbitrary
  outs <- vectorOf n (genOutput vk)
  pure $ UTxO $ Map.fromList $ zip inps outs

-- | Generate a single UTXO owned by 'vk'.
genOneUTxOFor :: VerificationKey PaymentKey -> Gen UTxO
genOneUTxOFor vk = do
  input <- arbitrary
  -- NOTE(AB): calling this generator while running a property will yield larger and larger
  -- values (quikcheck increases the 'size' parameter upon success) up to the point they are
  -- too large to fit in a transaction and validation fails in the ledger
  output <- scale (const 1) $ genOutput vk
  pure $ UTxO $ Map.singleton input output

-- | NOTE: See note on 'mkVkAddress' about 'NetworkId'.
genAddressInEra :: NetworkId -> Gen AddressInEra
genAddressInEra networkId =
  mkVkAddress networkId <$> genVerificationKey

genValue :: Gen Value
genValue = fromLedgerValue <$> arbitrary

-- | Generate UTXO entries that do not contain any assets. Useful to test /
-- measure cases where
genAdaOnlyUTxO :: Gen UTxO
genAdaOnlyUTxO = do
  fmap adaOnly <$> arbitrary

adaOnly :: TxOut CtxUTxO -> TxOut CtxUTxO
adaOnly = \case
  TxOut addr value datum refScript ->
    TxOut addr (lovelaceToValue $ selectLovelace value) datum refScript

-- | Generate "simplified" UTXO, ie. without some of the complexities required
-- for backward-compatibility and obscure features.
genUTxOWithSimplifiedAddresses :: Gen UTxO
genUTxOWithSimplifiedAddresses =
  UTxO.fromPairs <$> listOf genEntry
 where
  genEntry = (,) <$> genTxIn <*> genTxOut

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO = shrinkMapBy (UTxO . fromList) UTxO.pairs (shrinkList shrinkOne)
 where
  shrinkOne :: (TxIn, TxOut CtxUTxO) -> [(TxIn, TxOut CtxUTxO)]
  shrinkOne (i, o) = case o of
    TxOut addr value datum refScript ->
      [ (i, TxOut addr value' datum refScript)
      | value' <- shrinkValue value
      ]

shrinkValue :: Value -> [Value]
shrinkValue =
  shrinkMapBy valueFromList valueToList shrinkListAggressively

-- * Orphans

instance Arbitrary AssetName where
  arbitrary = AssetName . BS.take 32 <$> arbitrary

instance Arbitrary TxId where
  arbitrary = onlyTxId <$> arbitrary
   where
    onlyTxId (TxIn txi _) = txi

instance Arbitrary (TxOut CtxUTxO) where
  arbitrary = genTxOut

instance Arbitrary (VerificationKey PaymentKey) where
  arbitrary = fst <$> genKeyPair

instance Arbitrary (Hash PaymentKey) where
  arbitrary = do
    unsafePaymentKeyHashFromBytes . BS.pack <$> vectorOf 28 arbitrary

deriving newtype instance ToJSON UTxO

deriving newtype instance FromJSON UTxO

instance ToCBOR UTxO where
  toCBOR = toCBOR . toLedgerUTxO
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR UTxO where
  fromCBOR = fromLedgerUTxO <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

instance Arbitrary UTxO where
  shrink = shrinkUTxO
  arbitrary = genUTxOAlonzo
