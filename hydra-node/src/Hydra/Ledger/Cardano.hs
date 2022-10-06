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
import Cardano.Binary (decodeAnnotator, serialize, serialize', unsafeDeserialize')
import qualified Cardano.Crypto.DSIGN as CC
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Formatting.Buildable (build)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Json ()
import Plutus.V2.Ledger.Api (fromBuiltin)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.QuickCheck (
  choose,
  getSize,
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
  applyTx env utxo tx =
    case Ledger.applyTx globals env memPoolState tx of
      Left err ->
        Left (tx, toValidationError err)
      Right (Ledger.LedgerState{Ledger.lsUTxOState = us}, _validatedTx) ->
        Right $ Ledger._utxo us
   where
    toValidationError = ValidationError . show

    memPoolState =
      Ledger.LedgerState
        { Ledger.lsUTxOState = def{Ledger._utxo = utxo}
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
    withoutProtocolUpdates tx@(Ledger.ValidatedTx body _ _ _) =
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
    TxOut @CtxTx recipient valueOut TxOutDatumNone ReferenceScriptNone :
      [ TxOut @CtxTx
        owner
        (valueIn <> negateValue valueOut)
        (toTxContext datum)
        refScript
      | valueOut /= valueIn
      ]

  fee = Lovelace 0

-- | Obtain a human-readable pretty text representation of a transaction.
renderTx :: IsString str => Tx -> str
renderTx = renderTxWithUTxO mempty

renderTxs :: IsString str => [Tx] -> str
renderTxs xs = fromString $ toString $ intercalate "\n\n" (renderTx <$> xs)

-- | Like 'renderTx', but uses the given UTxO to resolve inputs.
renderTxWithUTxO :: IsString str => UTxO -> Tx -> str
renderTxWithUTxO utxo (Tx body _wits) =
  fromString $
    toString $
      unlines $
        [show (getTxId body)]
          <> [""]
          <> inputLines
          <> [""]
          <> referenceInputLines
          <> [""]
          <> outputLines
          <> [""]
          <> validityLines
          <> [""]
          <> mintLines
          <> [""]
          <> scriptLines
          <> [""]
          <> datumLines
          <> [""]
          <> redeemerLines
          <> [""]
          <> requiredSignersLines
 where
  ShelleyTxBody lbody scripts scriptsData _auxData _validity = body
  outs = Ledger.outputs' lbody
  TxBody content = body

  inputLines =
    "== INPUTS (" <> show (length (txIns content)) <> ")" :
    (("- " <>) . prettyTxIn . fst <$> sortBy (compare `on` fst) (txIns content))

  referenceInputLines =
    "== REFERENCE INPUTS (" <> show (length referenceInputs) <> ")" :
    (("- " <>) . prettyTxIn <$> sort referenceInputs)

  referenceInputs =
    case txInsReference content of
      TxInsReferenceNone -> []
      TxInsReference refInputs -> refInputs

  prettyTxIn i =
    case UTxO.resolve i utxo of
      Nothing -> renderTxIn i
      Just o ->
        renderTxIn i
          <> ("\n      " <> prettyAddr (txOutAddress o))
          <> ("\n      " <> prettyValue 1 (txOutValue o))
          <> ("\n      " <> prettyDatumUtxo (txOutDatum o))

  outputLines =
    [ "== OUTPUTS (" <> show (length (txOuts content)) <> ")"
    , "Total number of assets: " <> show totalNumberOfAssets
    ]
      <> (("- " <>) . prettyOut <$> txOuts content)

  prettyOut o =
    mconcat
      [ prettyAddr (txOutAddress o)
      , "\n      " <> prettyValue 1 (txOutValue o)
      , "\n      " <> prettyDatumCtx (txOutDatum o)
      ]

  prettyAddr = \case
    ShelleyAddressInEra addr -> show addr
    ByronAddressInEra addr -> show addr

  totalNumberOfAssets =
    sum $
      [ foldl' (\n inner -> n + Map.size inner) 0 outer
      | Ledger.TxOut _ (Ledger.Value _ outer) _ _ <- toList outs
      ]

  validityLines =
    [ "== VALIDITY"
    , show (txValidityRange content)
    ]

  mintLines =
    [ "== MINT/BURN\n" <> case txMintValue content of
        TxMintValueNone -> "[]"
        TxMintValue val _ -> prettyValue 0 val
    ]

  prettyValue n =
    T.replace " + " indent . renderValue
   where
    indent = "\n  " <> T.replicate n "    "

  prettyDatumUtxo :: TxOutDatum CtxUTxO -> Text
  prettyDatumUtxo = \case
    TxOutDatumNone ->
      "TxOutDatumNone"
    TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData
    _ -> error "absurd"

  prettyDatumCtx = \case
    TxOutDatumNone ->
      "TxOutDatumNone"
    TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    TxOutDatumInTx scriptData ->
      "TxOutDatumInTx " <> prettyScriptData scriptData
    TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData

  scriptLines =
    [ "== SCRIPTS (" <> show (length scripts) <> ")"
    , "Total size (bytes):  " <> show totalScriptSize
    ]
      <> (("- " <>) . prettyScript <$> scripts)

  totalScriptSize = sum $ BL.length . serialize <$> scripts

  prettyScript (fromLedgerScript -> script)
    | script == fromPlutusScript @PlutusScriptV2 Initial.validatorScript =
        "InitialScript Script (" <> scriptHash <> ")"
    | script == fromPlutusScript @PlutusScriptV2 Commit.validatorScript =
        "CommitScript Script (" <> scriptHash <> ")"
    | script == fromPlutusScript @PlutusScriptV2 Head.validatorScript =
        "Head Script (" <> scriptHash <> ")"
    | otherwise =
        "Unknown Script (" <> scriptHash <> ")"
   where
    scriptHash =
      show (Ledger.hashScript @(ShelleyLedgerEra Era) (toLedgerScript script))

  datumLines = case scriptsData of
    TxBodyNoScriptData -> []
    (TxBodyScriptData (Ledger.TxDats dats) _) ->
      "== DATUMS (" <> show (length dats) <> ")" :
      (("- " <>) . showDatumAndHash <$> Map.toList dats)

  showDatumAndHash (k, v) =
    mconcat
      [ show (Ledger.extractHash k)
      , "\n  "
      , prettyScriptData (fromLedgerData v)
      ]

  prettyScriptData =
    decodeUtf8 . Aeson.encode . scriptDataToJson ScriptDataJsonNoSchema

  redeemerLines = case scriptsData of
    TxBodyNoScriptData -> []
    (TxBodyScriptData _ re) ->
      let rdmrs = Map.toList $ Ledger.unRedeemers re
       in "== REDEEMERS (" <> show (length rdmrs) <> ")" :
          (("- " <>) . prettyRedeemer <$> rdmrs)

  prettyRedeemer (Ledger.RdmrPtr tag ix, (redeemerData, redeemerBudget)) =
    unwords
      [ show tag <> "#" <> show ix
      , mconcat
          [ "( cpu = " <> show (Ledger.exUnitsSteps redeemerBudget)
          , ", mem = " <> show (Ledger.exUnitsMem redeemerBudget) <> " )"
          ]
      , "\n  " <> prettyScriptData (fromLedgerData redeemerData)
      ]

  requiredSignersLines =
    "== REQUIRED SIGNERS" : case txExtraKeyWits content of
      TxExtraKeyWitnessesNone -> ["[]"]
      TxExtraKeyWitnesses xs -> ("- " <>) . show <$> xs

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
genTxOutAdaOnly :: Gen (TxOut ctx)
genTxOutAdaOnly = do
  vk <- arbitrary
  value <- lovelaceToValue . Lovelace <$> scale (* 8) arbitrary `suchThat` (> 0)
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone ReferenceScriptNone

-- | A more random generator than the 'Arbitrary TxIn' from cardano-ledger.
genTxIn :: Gen TxIn
genTxIn =
  fmap fromLedgerTxIn . Ledger.TxIn
    -- NOTE: [88, 32] is a CBOR prefix for a bytestring of 32 bytes.
    <$> fmap (unsafeDeserialize' . BS.pack . ([88, 32] <>)) (vectorOf 32 arbitrary)
    <*> fmap Ledger.TxIx (choose (0, 99))

-- | Generate a fixed size UTxO with ada-only outputs.
genUTxOAdaOnlyOfSize :: Int -> Gen UTxO
genUTxOAdaOnlyOfSize numUTxO =
  fold <$> vectorOf numUTxO (UTxO.singleton <$> gen)
 where
  gen = (,) <$> arbitrary <*> genTxOutAdaOnly

-- | Generate 'Alonzo' era 'UTxO', which may contain arbitrary assets in
-- 'TxOut's addressed to public keys *and* scripts.
-- NOTE: This is not reducing size when generating assets in 'TxOut's, so will
-- end up regularly with 300+ assets with generator size 30.
-- NOTE: The Arbitrary TxIn instance from the ledger is producing colliding
-- values, so we replace them.
genUTxOAlonzo :: Gen UTxO
genUTxOAlonzo = do
  utxoMap <- Map.toList . Ledger.unUTxO <$> arbitrary
  fmap UTxO.fromPairs . forM utxoMap $ \(_, o) -> do
    i <- arbitrary
    pure (i, fromLedgerTxOut o)

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

genAdaValue :: Gen Value
genAdaValue = lovelaceToValue . selectLovelace <$> genValue

-- | Generate UTXO entries that do not contain any assets. Useful to test /
-- measure cases where
genAdaOnlyUTxO :: Gen UTxO
genAdaOnlyUTxO = do
  fmap adaOnly <$> arbitrary

adaOnly :: TxOut CtxUTxO -> TxOut CtxUTxO
adaOnly = \case
  TxOut addr value datum refScript ->
    TxOut addr (lovelaceToValue $ selectLovelace value) datum refScript

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
  notByronAddress (_, TxOut addr _ _ _) = case addr of
    ByronAddressInEra{} -> False
    _ -> True
  -- NOTE:
  -- - we discard pointers because there encoding sucks and they are unused.
  --   Ledger team plans to remove them in future versions anyway.
  --
  -- - We fix all network id to testnet.
  tweakAddress out@(txin, TxOut addr val dat refScript) = case addr of
    ShelleyAddressInEra (ShelleyAddress _ cre sr) ->
      case sr of
        Ledger.StakeRefPtr _ ->
          (txin, TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat refScript)
        _ ->
          (txin, TxOut (ShelleyAddressInEra (ShelleyAddress Ledger.Testnet cre sr)) val dat refScript)
    _ -> out

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
