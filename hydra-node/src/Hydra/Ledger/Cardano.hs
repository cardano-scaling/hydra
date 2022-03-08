{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano (
  module Hydra.Ledger.Cardano,
  module Hydra.Ledger.Cardano.Builder,
  Ledger.ShelleyGenesis (..),
  Tx,
) where

import Hydra.Prelude

import Hydra.Cardano.Api
import Hydra.Ledger.Cardano.Builder

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (decodeAnnotator, serialize', unsafeDeserialize')
import qualified Cardano.Crypto.DSIGN as CC
import Cardano.Crypto.Hash (SHA256, digest)
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Crypto as Ledger (StandardCrypto)
import qualified Cardano.Ledger.Mary as Ledger.Mary
import qualified Cardano.Ledger.Shelley.API as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Arrow (left)
import Control.Monad (foldM)
import qualified Control.State.Transition as Ledger
import qualified Data.ByteString as BS
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Json ()
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import qualified Test.Cardano.Ledger.Shelley.Generator.Constants as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Core as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.EraGen as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Utxo as Ledger.Generator
import Test.QuickCheck (
  choose,
  getSize,
  scale,
  shrinkList,
  shrinkMapBy,
  sized,
  suchThat,
  vectorOf,
 )

-- * Ledger

-- TODO(SN): Pre-validate transactions to get less confusing errors on
-- transactions which are not expected to working on a layer-2
cardanoLedger :: Ledger.Globals -> Ledger.LedgerEnv LedgerEra -> Ledger Tx
cardanoLedger globals ledgerEnv =
  Ledger
    { applyTransactions = applyAll
    , initUTxO = mempty
    }
 where
  -- NOTE(SN): See full note on 'applyTx' why we only have a single transaction
  -- application here.
  applyAll utxo = \case
    [] -> Right utxo
    (tx : txs) -> do
      utxo' <- left (first fromLedgerTx) $ fromLedgerUTxO <$> applyTx ledgerEnv (toLedgerUTxO utxo) (toLedgerTx tx)
      applyAll utxo' txs

  -- NOTE(SN): This is will fail on any transaction requiring the 'DPState' to be
  -- in a certain state as we do throw away the resulting 'DPState' and only take
  -- the ledger's 'UTxO' forward.
  --
  -- We came to this signature of only applying a single transaction because we
  -- got confused why a sequence of transactions worked but sequentially applying
  -- single transactions didn't. This was because of this not-keeping the'DPState'
  -- as described above.
  applyTx ::
    ( Ledger.ApplyTx era
    , Default (Ledger.State (Ledger.EraRule "PPUP" era))
    ) =>
    Ledger.LedgerEnv era ->
    Ledger.UTxO era ->
    Ledger.Tx era ->
    Either (Ledger.Tx era, ValidationError) (Ledger.UTxO era)
  applyTx env utxo tx =
    case Ledger.applyTxsTransition globals env (pure tx) memPoolState of
      Left err -> Left (tx, toValidationError err)
      Right (ls, _ds) -> Right $ Ledger._utxo ls
   where
    toValidationError = ValidationError . show
    memPoolState = (def{Ledger._utxo = utxo}, def)

-- * Cardano Tx

instance IsTx Tx where
  type TxIdType Tx = TxId
  type UTxOType Tx = UTxO
  type ValueType Tx = Value

  txId = getTxId . getTxBody
  balance = foldMap txOutValue

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
    withoutProtocolUpdates tx@(Ledger.Alonzo.ValidatedTx body _ _ _) =
      let body' = body{Ledger.Alonzo.txUpdates = SNothing}
       in tx{Ledger.Alonzo.body = body'}

-- | Create a zero-fee, payment cardano transaction.
mkSimpleTx ::
  (TxIn, TxOut CtxUTxO) ->
  -- | Recipient address and amount.
  (AddressInEra, Value) ->
  -- | Sender's signing key.
  SigningKey PaymentKey ->
  Either TxBodyError Tx
mkSimpleTx (txin, TxOut owner valueIn datum) (recipient, valueOut) sk = do
  body <- makeTransactionBody bodyContent
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
    TxOut @CtxTx recipient valueOut TxOutDatumNone :
      [ TxOut @CtxTx
        owner
        (valueIn <> negateValue valueOut)
        (toTxContext datum)
      | valueOut /= valueIn
      ]

  fee = Lovelace 0

hashTxOuts :: [TxOut CtxUTxO] -> ByteString
hashTxOuts =
  digest @SHA256 Proxy . serialize' . fmap toLedgerTxOut

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
  arbitrary = genUTxO

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

-- TODO: Generate non-genesis transactions for better coverage.
-- TODO: Enable Alonzo-specific features. We started off in the Mary era, and
-- some of our tests / interfaces aren't fully ready for Alonzo-specific
-- details. We later changed the ledger's internals to work with Alonzo-era
-- specific types, and, to make this in incremental steps, this function still
-- generates Mary transactions, but cast them to Alonzo's.
genTx :: Ledger.LedgerEnv LedgerEra -> UTxO -> Gen Tx
genTx ledgerEnv utxos = do
  fromLedgerTx <$> Ledger.Generator.genTx genEnv ledgerEnv (utxoState, dpState)
 where
  utxoState = def{Ledger._utxo = toLedgerUTxO utxos}
  dpState = Ledger.DPState def def

  -- NOTE(AB): This sets some parameters for the tx generator that will
  -- affect the structure of generated trasactions. In our case, we want
  -- to remove "special" capabilities which are irrelevant in the context
  -- of a Hydra head
  -- see https://github.com/input-output-hk/cardano-ledger-specs/blob/nil/shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Generator/Constants.hs#L10
  genEnv =
    (Ledger.Generator.genEnv Proxy)
      { Ledger.Generator.geConstants = noPPUpdatesNoScripts
      }
   where
    noPPUpdatesNoScripts =
      Ledger.Generator.defaultConstants
        { Ledger.Generator.frequencyTxUpdates = 0
        , Ledger.Generator.frequencyTxWithMetadata = 0
        , Ledger.Generator.maxCertsPerTx = 0
        }

genSequenceOfValidTransactions :: Ledger.Globals -> Ledger.LedgerEnv LedgerEra -> Gen (UTxO, [Tx])
genSequenceOfValidTransactions globals ledgerEnv = do
  n <- getSize
  numTxs <- choose (1, n)
  genFixedSizeSequenceOfValidTransactions globals ledgerEnv numTxs

genFixedSizeSequenceOfValidTransactions :: Ledger.Globals -> Ledger.LedgerEnv LedgerEra -> Int -> Gen (UTxO, [Tx])
genFixedSizeSequenceOfValidTransactions globals ledgerEnv numTxs = do
  initialUTxO <- genUTxO
  if initialUTxO == mempty
    then pure (initialUTxO, [])
    else (initialUTxO,) . reverse . snd <$> foldM newTx (initialUTxO, []) [1 .. numTxs]
 where
  newTx (utxos, acc) _ = do
    tx <- genTx ledgerEnv utxos
    case applyTransactions (cardanoLedger globals ledgerEnv) utxos [tx] of
      Left err -> error $ show err
      Right newUTxOs -> pure (newUTxOs, tx : acc)

-- TODO: Enable arbitrary datum in generators
-- TODO: This should better be called 'genOutputFor'
genOutput ::
  forall ctx.
  VerificationKey PaymentKey ->
  Gen (TxOut ctx)
genOutput vk = do
  value <- fromLedgerValue <$> scale (* 8) arbitrary
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone

-- | A more random generator than the 'Arbitrary TxIn' from cardano-ledger.
genTxIn :: Gen TxIn
genTxIn =
  fmap fromLedgerTxIn . Ledger.TxIn
    -- NOTE: [88, 32] is a CBOR prefix for a bytestring of 32 bytes.
    <$> fmap (unsafeDeserialize' . BS.pack . ([88, 32] <>)) (vectorOf 32 arbitrary)
    <*> fmap fromIntegral (choose @Int (0, 99))

-- | Generate some number of 'UTxO'.
genUTxO :: Gen UTxO
genUTxO =
  genUTxOAlonzo

-- | Generate 'Alonzo' era 'UTxO', which has Ada-only 'TxOut' addressed to
-- public keys and scripts.
genUTxOAlonzo :: Gen UTxO
genUTxOAlonzo =
  fromLedgerUTxO <$> arbitrary

-- | Generate 'Mary' era 'UTxO', which may contain arbitrary assets in 'TxOut's.
-- NOTE: This is not reducing size when generating assets in 'TxOut's, so will
-- end up regularly with 300+ assets with generator size 30.
genUTxOMary :: Gen UTxO
genUTxOMary =
  convertFromMaryUTxO <$> arbitrary
 where
  convertFromMaryUTxO = fromLedgerUTxO . Ledger.UTxO . Map.map fromMaryTxOut . Ledger.unUTxO

  fromMaryTxOut ::
    Ledger.Mary.TxOut (Ledger.Mary.MaryEra Ledger.StandardCrypto) ->
    Ledger.TxOut LedgerEra
  fromMaryTxOut = \case
    Ledger.Shelley.TxOutCompact addr value ->
      Ledger.Alonzo.TxOutCompact addr value

-- | Generate utxos owned by the given cardano key.
genUTxOFor :: VerificationKey PaymentKey -> Gen UTxO
genUTxOFor vk = do
  n <- arbitrary `suchThat` (> 0)
  inps <- vectorOf n arbitrary
  outs <- vectorOf n (genOutput vk)
  pure $ UTxO $ Map.fromList $ zip (fromLedgerTxIn <$> inps) outs

-- | Generate a single UTXO owned by 'vk'.
genOneUTxOFor :: VerificationKey PaymentKey -> Gen UTxO
genOneUTxOFor vk = do
  input <- arbitrary
  -- NOTE(AB): calling this generator while running a property will yield larger and larger
  -- values (quikcheck increases the 'size' parameter upon success) up to the point they are
  -- too large to fit in a transaction and validation fails in the ledger
  output <- scale (const 1) $ genOutput vk
  pure $ UTxO $ Map.singleton (fromLedgerTxIn input) output

-- | NOTE: See note on 'mkVkAddress' about 'NetworkId'.
genAddressInEra :: NetworkId -> Gen AddressInEra
genAddressInEra networkId =
  mkVkAddress networkId <$> genVerificationKey

genValue :: Gen Value
genValue = txOutValue <$> (genKeyPair >>= (genOutput . fst))

-- | Generate UTXO entries that do not contain any assets. Useful to test /
-- measure cases where
genAdaOnlyUTxO :: Gen UTxO
genAdaOnlyUTxO = do
  fmap adaOnly <$> arbitrary

adaOnly :: TxOut CtxUTxO -> TxOut CtxUTxO
adaOnly = \case
  TxOut addr value datum ->
    TxOut addr (lovelaceToValue $ selectLovelace value) datum

-- | Generate "simplified" UTXO, ie. without some of the complexities required for
-- backward-compatibility and obscure features.
genUTxOWithSimplifiedAddresses :: Gen UTxO
genUTxOWithSimplifiedAddresses = simplifyUTxO <$> arbitrary

-- | Rewrite given UTXO to remove some corner cases.
--
-- * Remove Byron addresses. Those are unnecessarily complicated and deprecated so they should
--   not be used either on- or off-chain
-- * Replace stake pointers with `StakeRefNull`. Stake pointers is an obscure feature of Cardano
--   addresses that's very rarely used in practice.
simplifyUTxO :: UTxO -> UTxO
simplifyUTxO = UTxO . Map.fromList . map tweakAddress . filter notByronAddress . UTxO.pairs
 where
  notByronAddress (_, TxOut addr _ _) = case addr of
    AddressInEra _ ByronAddress{} -> False
    _ -> True
  -- NOTE:
  -- - we discard pointers because there encoding sucks and they are unused.
  --   Ledger team plans to remove them in future versions anyway.
  --
  -- - We fix all network id to testnet.
  tweakAddress out@(txin, TxOut addr val dat) = case addr of
    AddressInEra typ (ShelleyAddress _ cre sr) ->
      case sr of
        Ledger.StakeRefPtr _ ->
          (txin, TxOut (AddressInEra typ (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat)
        _ ->
          (txin, TxOut (AddressInEra typ (ShelleyAddress Ledger.Testnet cre sr)) val dat)
    _ -> out

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO = shrinkMapBy (UTxO . fromList) UTxO.pairs (shrinkList shrinkOne)
 where
  shrinkOne :: (TxIn, TxOut CtxUTxO) -> [(TxIn, TxOut CtxUTxO)]
  shrinkOne (i, o) = case o of
    TxOut addr value datum ->
      [ (i, TxOut addr value' datum)
      | value' <- shrinkValue value
      ]

shrinkValue :: Value -> [Value]
shrinkValue =
  shrinkMapBy valueFromList valueToList shrinkListAggressively

-- * Orphans

instance Arbitrary AssetName where
  arbitrary = AssetName . BS.take 32 <$> arbitrary

instance Arbitrary TxIn where
  arbitrary = genTxIn

instance Arbitrary TxId where
  arbitrary = onlyTxId <$> arbitrary
   where
    onlyTxId (TxIn txi _) = txi

instance Arbitrary (TxOut CtxUTxO) where
  arbitrary = fromLedgerTxOut <$> arbitrary

instance Arbitrary (VerificationKey PaymentKey) where
  arbitrary = fst <$> genKeyPair

instance Arbitrary (Hash PaymentKey) where
  arbitrary = do
    unsafePaymentKeyHashFromBytes . BS.pack <$> vectorOf 28 arbitrary
