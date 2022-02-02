{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano (
  module Hydra.Ledger.Cardano,
  module Hydra.Ledger.Cardano.Builder,
  Ledger.ShelleyGenesis (..),
) where

import Hydra.Prelude

import Hydra.Cardano.Api
import Hydra.Ledger.Cardano.Builder

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (decodeAnnotator, serialize')
import qualified Cardano.Crypto.DSIGN as CC
import Cardano.Crypto.Hash (SHA256, digest)
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Crypto as Ledger (StandardCrypto)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary as Ledger.Mary hiding (Value)
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.Slot as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Slotting.EpochInfo as Slotting
import qualified Cardano.Slotting.Time as Slotting
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Arrow (left)
import Control.Monad (foldM)
import qualified Control.State.Transition as Ledger
import qualified Data.ByteString as BS
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Formatting.Buildable (build)
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Json ()
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import qualified Test.Cardano.Ledger.Alonzo.AlonzoEraGen as Ledger.Alonzo
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
  suchThat,
  vectorOf,
 )

-- * Ledger

-- TODO(SN): Pre-validate transactions to get less confusing errors on
-- transactions which are not expected to working on a layer-2
cardanoLedger :: Ledger CardanoTx
cardanoLedger =
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

type CardanoTx = Tx Era

instance IsTx CardanoTx where
  type TxIdType CardanoTx = TxId
  type UTxOType CardanoTx = UTxO
  type ValueType CardanoTx = Value

  txId = getTxId . getTxBody
  balance = foldMap (\(TxOut _ value _) -> txOutValueToValue value)

instance ToCBOR CardanoTx where
  toCBOR = CBOR.encodeBytes . serialize' . toLedgerTx

instance FromCBOR CardanoTx where
  fromCBOR = do
    bs <- CBOR.decodeBytes
    decodeAnnotator "CardanoTx" fromCBOR (fromStrict bs)
      & either
        (fail . toString . toLazyText . build)
        (pure . fromLedgerTx)

instance ToJSON CardanoTx where
  toJSON = toJSON . toLedgerTx

instance FromJSON CardanoTx where
  parseJSON = fmap fromLedgerTx . parseJSON

instance Arbitrary CardanoTx where
  -- TODO: shrinker!
  arbitrary = genUTxO >>= genTx

-- | Create a zero-fee, payment cardano transaction.
mkSimpleCardanoTx ::
  (TxIn, TxOut CtxUTxO Era) ->
  -- | Recipient address and amount.
  (AddressInEra Era, Value) ->
  -- | Sender's signing key.
  SigningKey PaymentKey ->
  Either TxBodyError CardanoTx
mkSimpleCardanoTx (txin, TxOut owner txOutValueIn datum) (recipient, valueOut) sk = do
  body <- makeTransactionBody txBodyContent
  let witnesses = [makeShelleyKeyWitness body (WitnessPaymentKey sk)]
  pure $ makeSignedTransaction witnesses body
 where
  valueIn = txOutValueToValue txOutValueIn

  txBodyContent =
    emptyTxBody
      { txIns = map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
      , txOuts
      , txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra fee
      }

  txOuts =
    TxOut @CtxTx recipient (TxOutValue MultiAssetInAlonzoEra valueOut) TxOutDatumNone :
      [ TxOut @CtxTx
        owner
        (TxOutValue MultiAssetInAlonzoEra $ valueIn <> negateValue valueOut)
        (toTxContext datum)
      | valueOut /= valueIn
      ]

  fee = Lovelace 0

hashTxOuts :: [TxOut CtxUTxO Era] -> ByteString
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

-- TODO: Use Alonzo generators!
-- probably: import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
instance Arbitrary UTxO where
  shrink = shrinkUTxO
  arbitrary =
    fmap
      (fromLedgerUTxO . Ledger.UTxO . Map.map fromMaryTxOut . Ledger.unUTxO)
      arbitrary
   where
    fromMaryTxOut ::
      Ledger.Mary.TxOut (Ledger.Mary.MaryEra Ledger.StandardCrypto) ->
      Ledger.TxOut LedgerEra
    fromMaryTxOut = \case
      Ledger.Shelley.TxOutCompact addr value ->
        Ledger.Alonzo.TxOutCompact addr value

-- * Generators

genVerificationKey :: Gen (VerificationKey PaymentKey)
genVerificationKey = fst <$> genKeyPair

genKeyPair :: Gen (VerificationKey PaymentKey, SigningKey PaymentKey)
genKeyPair = do
  -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
  -- play well with pure generation from seed.
  sk <- fromJust . CC.rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 64 arbitrary
  let vk = CC.deriveVerKeyDSIGN sk
  pure (PaymentVerificationKey (Ledger.VKey vk), PaymentSigningKey sk)

-- TODO: Generate non-genesis transactions for better coverage.
-- TODO: Enable Alonzo-specific features. We started off in the Mary era, and
-- some of our tests / interfaces aren't fully ready for Alonzo-specific
-- details. We later changed the ledger's internals to work with Alonzo-era
-- specific types, and, to make this in incremental steps, this function still
-- generates Mary transactions, but cast them to Alonzo's.
genTx :: UTxO -> Gen CardanoTx
genTx utxos = do
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

genSequenceOfValidTransactions :: UTxO -> Gen [CardanoTx]
genSequenceOfValidTransactions initialUTxO
  | initialUTxO == mempty = pure []
  | otherwise = do
    n <- getSize
    numTxs <- choose (1, n)
    genFixedSizeSequenceOfValidTransactions numTxs initialUTxO

genFixedSizeSequenceOfValidTransactions :: Int -> UTxO -> Gen [CardanoTx]
genFixedSizeSequenceOfValidTransactions numTxs initialUTxO
  | initialUTxO == mempty = pure []
  | otherwise =
    reverse . snd <$> foldM newTx (initialUTxO, []) [1 .. numTxs]
 where
  newTx (utxos, acc) _ = do
    tx <- genTx utxos
    case applyTransactions cardanoLedger utxos [tx] of
      Left err -> error $ show err
      Right newUTxOs -> pure (newUTxOs, tx : acc)

-- TODO: Enable arbitrary datum in generators
-- TODO: This should better be called 'genOutputFor'
genOutput ::
  forall era ctx.
  (IsShelleyBasedEra era) =>
  VerificationKey PaymentKey ->
  Gen (TxOut ctx era)
genOutput vk = do
  assets <- fromLedgerValue <$> scale (* 8) arbitrary
  let value =
        either
          (`TxOutAdaOnly` selectLovelace assets)
          (`TxOutValue` assets)
          (multiAssetSupportedInEra (cardanoEra @era))
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone

genUTxO :: Gen UTxO
genUTxO = do
  genesisTxId <- arbitrary
  utxo <- Ledger.Generator.genUtxo0 (Ledger.Generator.genEnv Proxy)
  pure $ fromLedgerUTxO . Ledger.UTxO . Map.mapKeys (setTxId genesisTxId) $ Ledger.unUTxO utxo
 where
  setTxId ::
    Ledger.TxId Ledger.StandardCrypto ->
    Ledger.TxIn Ledger.StandardCrypto ->
    Ledger.TxIn Ledger.StandardCrypto
  setTxId baseId (Ledger.TxIn _ti wo) = Ledger.TxIn baseId wo

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
genAddressInEra :: NetworkId -> Gen (AddressInEra Era)
genAddressInEra networkId =
  mkVkAddress networkId <$> genVerificationKey

genValue :: Gen Value
genValue = txOutValue <$> (genKeyPair >>= (genOutput . fst))

-- | Generate UTXO entries that do not contain any assets. Useful to test /
-- measure cases where
genAdaOnlyUTxO :: Gen UTxO
genAdaOnlyUTxO = do
  fmap adaOnly <$> arbitrary

adaOnly :: TxOut CtxUTxO AlonzoEra -> TxOut CtxUTxO AlonzoEra
adaOnly = \case
  TxOut addr value datum ->
    TxOut addr (lovelaceToTxOutValue $ txOutValueToLovelace value) datum

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
    AddressInEra ByronAddressInAnyEra _ -> False
    _ -> True
  -- NOTE:
  -- - we discard pointers because there encoding sucks and they are unused.
  --   Ledger team plans to remove them in future versions anyway.
  --
  -- - We fix all network id to testnet.
  tweakAddress out@(txin, TxOut addr val dat) = case addr of
    AddressInEra er@(ShelleyAddressInEra _) (ShelleyAddress _ cre sr) ->
      case sr of
        Ledger.StakeRefPtr _ ->
          (txin, TxOut (AddressInEra er (ShelleyAddress Ledger.Testnet cre Ledger.StakeRefNull)) val dat)
        _ ->
          (txin, TxOut (AddressInEra er (ShelleyAddress Ledger.Testnet cre sr)) val dat)
    _ -> out

shrinkUTxO :: UTxO -> [UTxO]
shrinkUTxO = shrinkMapBy (UTxO . fromList) UTxO.pairs (shrinkList shrinkOne)
 where
  shrinkOne :: (TxIn, TxOut CtxUTxO AlonzoEra) -> [(TxIn, TxOut CtxUTxO AlonzoEra)]
  shrinkOne (i, o) = case o of
    TxOut _ TxOutAdaOnly{} _ ->
      []
    TxOut addr (TxOutValue MultiAssetInAlonzoEra value) datum ->
      [ (i, TxOut addr (TxOutValue MultiAssetInAlonzoEra value') datum)
      | value' <- shrinkValue value
      ]

shrinkValue :: Value -> [Value]
shrinkValue =
  shrinkMapBy valueFromList valueToList shrinkListAggressively

-- * Orphans

instance Arbitrary AssetName where
  arbitrary = AssetName . BS.take 32 <$> arbitrary

instance Arbitrary TxIn where
  arbitrary = fromShelleyTxIn <$> arbitrary

instance Arbitrary TxId where
  arbitrary = onlyTxId . fromShelleyTxIn <$> arbitrary
   where
    onlyTxId (TxIn txi _) = txi

instance Arbitrary (TxOut CtxUTxO AlonzoEra) where
  arbitrary = fromShelleyTxOut ShelleyBasedEraAlonzo <$> arbitrary

instance Arbitrary (VerificationKey PaymentKey) where
  arbitrary = fst <$> genKeyPair

instance Arbitrary (Hash PaymentKey) where
  arbitrary = do
    unsafePaymentKeyHashFromBytes . BS.pack <$> vectorOf 28 arbitrary

-- * Temporary / Quick-n-dirty

-- FIXME: Do not hard-code this, make it configurable / inferred from the
-- genesis configuration.
ledgerEnv :: Ledger.LedgerEnv LedgerEra
ledgerEnv =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 1
    , Ledger.ledgerIx = 0
    , Ledger.ledgerPp =
        def
          { Ledger.Alonzo._maxTxSize = 1024 * 1024
          , Ledger.Alonzo._maxValSize = 5000
          , Ledger.Alonzo._maxCollateralInputs = 10
          , Ledger.Alonzo._maxTxExUnits =
              Ledger.Alonzo.ExUnits
                { Ledger.Alonzo.exUnitsMem = 10_000_000
                , Ledger.Alonzo.exUnitsSteps = 10_000_000_000
                }
          , Ledger.Alonzo._maxBlockExUnits =
              Ledger.Alonzo.ExUnits
                { Ledger.Alonzo.exUnitsMem = 50_000_000
                , Ledger.Alonzo.exUnitsSteps = 40_000_000_000
                }
          , Ledger.Alonzo._costmdls =
              -- XXX(SN): This is a sledgehammer approach: The genTx would hit
              -- execution budgets with the defaultCostModel. There is a TODO in
              -- cardano-ledger's AlonzoEraGen.hs about not using freeCostModel
              Map.fromList $
                [ (lang, Ledger.Alonzo.freeCostModel)
                | lang <- [minBound .. maxBound]
                ]
          }
    , Ledger.ledgerAccount = error "ledgerEnv: ledgersAccount undefined"
    }

-- FIXME: Do not hard-code this, make it configurable / inferred from the
-- genesis configuration.
--
-- From: shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Utils.hs
globals :: Ledger.Globals
globals =
  Ledger.Globals
    { Ledger.epochInfoWithErr = Slotting.fixedEpochInfo (Ledger.EpochSize 100) (Slotting.mkSlotLength 1)
    , Ledger.slotsPerKESPeriod = 20
    , Ledger.stabilityWindow = 33
    , Ledger.randomnessStabilisationWindow = 33
    , Ledger.securityParameter = 10
    , Ledger.maxKESEvo = 10
    , Ledger.quorum = 5
    , Ledger.maxMajorPV = 1000
    , Ledger.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , Ledger.activeSlotCoeff = Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Ledger.networkId = Ledger.Testnet
    , Ledger.systemStart = Slotting.SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
