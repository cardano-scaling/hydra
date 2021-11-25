{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano (
  module Hydra.Ledger.Cardano,
  module Cardano.Api,
  module Cardano.Api.Shelley,
) where

import Hydra.Prelude hiding (id)

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Binary (decodeAnnotator, serialize')
import qualified Cardano.Crypto.DSIGN as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Ledger (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary as Ledger.Mary
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
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
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Formatting.Buildable (build)
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Orphans ()
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import qualified Test.Cardano.Ledger.Alonzo.AlonzoEraGen as Ledger.Alonzo
import qualified Test.Cardano.Ledger.Shelley.Generator.Constants as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Core as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.EraGen as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Utxo as Ledger.Generator
import Test.QuickCheck (choose, getSize, scale, suchThat, vectorOf)

type Era = AlonzoEra

type LedgerCrypto = Ledger.StandardCrypto

type LedgerEra = Ledger.Alonzo.AlonzoEra LedgerCrypto

type CardanoTx = Tx Era

-- TODO(SN): Pre-validate transactions to get less confusing errors on
-- transactions which are not expected to working on a layer-2
cardanoLedger :: Ledger CardanoTx
cardanoLedger =
  Ledger
    { applyTransactions = applyAll
    , initUtxo = mempty
    }
 where
  -- NOTE(SN): See full note on 'applyTx' why we only have a single transaction
  -- application here.
  applyAll utxo = \case
    [] -> Right utxo
    (tx : txs) -> do
      utxo' <- left (first fromLedgerTx) $ fromLedgerUtxo <$> applyTx ledgerEnv (toLedgerUtxo utxo) (toLedgerTx tx)
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

signWith ::
  forall era.
  (IsShelleyBasedEra era) =>
  TxId ->
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  KeyWitness era
signWith (TxId h) (PaymentVerificationKey vk, PaymentSigningKey sk) =
  ShelleyKeyWitness (shelleyBasedEra @era) $
    Ledger.Shelley.WitVKey
      (Ledger.asWitness vk)
      (Ledger.signedDSIGN @Ledger.StandardCrypto sk h)

--
-- Type conversions & plumbing
--

--
-- Utxo
--

type Utxo = Utxo' (TxOut CtxUTxO Era)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO'and provide 'Monoid' and 'Foldable' instances to make utxo
-- manipulation bareable.
newtype Utxo' out = Utxo
  { utxoMap :: Map TxIn out
  }
  deriving newtype (Eq, Show)

instance ToCBOR Utxo where
  toCBOR = toCBOR . toLedgerUtxo
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR Utxo where
  fromCBOR = fromLedgerUtxo <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

instance Functor Utxo' where
  fmap fn (Utxo u) = Utxo (fmap fn u)

instance Foldable Utxo' where
  foldMap fn = foldMap fn . utxoMap
  foldr fn zero = foldr fn zero . utxoMap

instance Semigroup Utxo where
  Utxo uL <> Utxo uR = Utxo (uL <> uR)

instance Monoid Utxo where
  mempty = Utxo mempty

instance ToJSON Utxo where
  toJSON = toJSON . utxoMap

instance FromJSON Utxo where
  parseJSON = fmap Utxo . parseJSON

instance Arbitrary Utxo where
  -- TODO: shrinker!
  -- TODO: Use Alonzo generators!
  -- probably: import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
  arbitrary =
    fmap
      (fromLedgerUtxo . Ledger.UTxO . Map.map fromMaryTxOut . Ledger.unUTxO)
      arbitrary

utxoPairs :: Utxo' out -> [(TxIn, out)]
utxoPairs = Map.toList . utxoMap

prettyUtxo :: (TxIn, TxOut ctx era) -> Text
prettyUtxo (k, TxOut _ (txOutValueToValue -> v) _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> prettyValue v

-- FIXME: This function is wrong! It's mapping a transaction's own inputs to its
-- own outputs. Whoops. It's currently used in Hydra.Chain.Direct.Tx where
-- the calling code only look at the outputs of the transactions and also in the
-- generators of the local-cluster (Whoops bis).
utxoFromTx :: CardanoTx -> Utxo
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ Ledger.Alonzo.outputs' ledgerBody
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [0 .. fromIntegral (length txOuts)]
        ]
   in fromLedgerUtxo $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

--
-- Tx
--

instance IsTx CardanoTx where
  type TxIdType CardanoTx = TxId
  type UtxoType CardanoTx = Utxo
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
  arbitrary = genUtxo >>= genTx

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

  -- TODO: We could define an 'empty' TxBodyContent and use record field
  -- modifiers to simply set the fields of interest.
  txBodyContent =
    TxBodyContent
      (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin])
      TxInsCollateralNone
      txOuts
      (TxFeeExplicit TxFeesExplicitInAlonzoEra fee)
      (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
      TxMetadataNone
      TxAuxScriptsNone
      TxExtraKeyWitnessesNone
      (BuildTxWith Nothing)
      TxWithdrawalsNone
      TxCertificatesNone
      TxUpdateProposalNone
      TxMintNone
      TxScriptValidityNone

  txOuts =
    TxOut @CtxTx recipient (TxOutValue MultiAssetInAlonzoEra valueOut) TxOutDatumNone :
      [ TxOut @CtxTx owner (TxOutValue MultiAssetInAlonzoEra $ valueIn <> negateValue valueOut) (toTxDatum datum)
      | valueOut /= valueIn
      ]

  fee = Lovelace 0

toTxDatum :: TxOutDatum CtxUTxO Era -> TxOutDatum CtxTx Era
toTxDatum = \case
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash sdsie ha -> TxOutDatumHash sdsie ha

-- | Convert an existing @cardano-api@'s 'Tx' to a @cardano-ledger-specs@ 'Tx'
toLedgerTx :: CardanoTx -> Ledger.Tx LedgerEra
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Alonzo.Redeemers mempty)
     in Ledger.Alonzo.ValidatedTx
          { Ledger.Alonzo.body =
              body
          , Ledger.Alonzo.isValid =
              toLedgerScriptValidity validity
          , Ledger.Alonzo.auxiliaryData =
              maybeToStrictMaybe auxData
          , Ledger.Alonzo.wits =
              Ledger.Alonzo.TxWitness
                { Ledger.Alonzo.txwitsVKey =
                    toLedgerKeyWitness vkWits
                , Ledger.Alonzo.txwitsBoot =
                    toLedgerBootstrapWitness vkWits
                , Ledger.Alonzo.txscripts =
                    fromList [(Ledger.hashScript @LedgerEra s, s) | s <- scripts]
                , Ledger.Alonzo.txdats =
                    datums
                , Ledger.Alonzo.txrdmrs =
                    redeemers
                }
          }
 where
  toLedgerScriptValidity :: TxScriptValidity Era -> Ledger.Alonzo.IsValid
  toLedgerScriptValidity =
    Ledger.Alonzo.IsValid . \case
      TxScriptValidityNone -> True
      TxScriptValidity _ ScriptValid -> True
      TxScriptValidity _ ScriptInvalid -> False

-- | Convert an existing @cardano-ledger-specs@'s 'Tx' into a @cardano-api@'s 'Tx'
fromLedgerTx :: Ledger.Tx LedgerEra -> CardanoTx
fromLedgerTx (Ledger.Alonzo.ValidatedTx body wits isValid auxData) =
  Tx
    (ShelleyTxBody era body scripts scriptsData (strictMaybeToMaybe auxData) validity)
    (fromLedgerTxWitness wits)
 where
  era =
    ShelleyBasedEraAlonzo
  scripts =
    Map.elems $ Ledger.Alonzo.txscripts' wits
  scriptsData =
    TxBodyScriptData
      ScriptDataInAlonzoEra
      (Ledger.Alonzo.txdats' wits)
      (Ledger.Alonzo.txrdmrs' wits)
  validity = case isValid of
    Ledger.Alonzo.IsValid True ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptValid
    Ledger.Alonzo.IsValid False ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptInvalid

--
-- TxId
--

toLedgerTxId :: TxId -> Ledger.TxId Ledger.StandardCrypto
toLedgerTxId (TxId h) =
  Ledger.TxId (Ledger.unsafeMakeSafeHash (CC.castHash h))

fromLedgerTxId :: Ledger.TxId Ledger.StandardCrypto -> TxId
fromLedgerTxId (Ledger.TxId h) =
  TxId (CC.castHash (Ledger.extractHash h))

--
-- Address
--

-- | Create an address from a verificaton key.
mkVkAddress ::
  IsShelleyBasedEra era =>
  NetworkId ->
  VerificationKey PaymentKey ->
  AddressInEra era
mkVkAddress networkId vk =
  shelleyAddressInEra $
    makeShelleyAddress
      networkId
      (PaymentCredentialByKey $ verificationKeyHash vk)
      NoStakeAddress

toLedgerAddr :: AddressInEra Era -> Ledger.Addr Ledger.StandardCrypto
toLedgerAddr = \case
  AddressInEra ByronAddressInAnyEra (ByronAddress addr) ->
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr)
  AddressInEra (ShelleyAddressInEra _) (ShelleyAddress ntwrk creds stake) ->
    Ledger.Addr ntwrk creds stake

--
-- TxOut
--

toMaryTxOut :: Ledger.TxOut LedgerEra -> Ledger.Mary.TxOut (Ledger.Mary.MaryEra Ledger.StandardCrypto)
toMaryTxOut = \case
  Ledger.Alonzo.TxOutCompact addr value ->
    Ledger.Shelley.TxOutCompact addr value
  Ledger.Alonzo.TxOutCompactDH addr value _datum ->
    Ledger.Shelley.TxOutCompact addr value

fromMaryTxOut :: Ledger.Mary.TxOut (Ledger.Mary.MaryEra Ledger.StandardCrypto) -> Ledger.TxOut LedgerEra
fromMaryTxOut = \case
  Ledger.Shelley.TxOutCompact addr value ->
    Ledger.Alonzo.TxOutCompact addr value

--
-- Utxo
--

toLedgerUtxo :: Utxo -> Ledger.UTxO LedgerEra
toLedgerUtxo =
  Ledger.UTxO . Map.foldMapWithKey fn . utxoMap
 where
  fn ::
    TxIn ->
    TxOut CtxUTxO Era ->
    Map (Ledger.TxIn Ledger.StandardCrypto) (Ledger.TxOut LedgerEra)
  fn i o =
    Map.singleton (toShelleyTxIn i) (toShelleyTxOut shelleyBasedEra o)

fromLedgerUtxo :: Ledger.UTxO LedgerEra -> Utxo
fromLedgerUtxo =
  Utxo . Map.foldMapWithKey fn . Ledger.unUTxO
 where
  fn ::
    Ledger.TxIn Ledger.StandardCrypto ->
    Ledger.TxOut LedgerEra ->
    Map TxIn (TxOut CtxUTxO Era)
  fn i o =
    Map.singleton (fromShelleyTxIn i) (fromShelleyTxOut shelleyBasedEra o)

--
-- KeyWitness
--

toLedgerKeyWitness ::
  [KeyWitness era] ->
  Set (Ledger.Shelley.WitVKey 'Ledger.Witness Ledger.StandardCrypto)
toLedgerKeyWitness vkWits =
  fromList [w | ShelleyKeyWitness _ w <- vkWits]

toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness Ledger.StandardCrypto)
toLedgerBootstrapWitness vkWits =
  fromList [w | ShelleyBootstrapWitness _ w <- vkWits]

fromLedgerTxWitness :: Ledger.Alonzo.TxWitness LedgerEra -> [KeyWitness Era]
fromLedgerTxWitness wits =
  Set.foldr ((:) . ShelleyKeyWitness era) [] (Ledger.Alonzo.txwitsVKey' wits)
    ++ Set.foldr ((:) . ShelleyBootstrapWitness era) [] (Ledger.Alonzo.txwitsBoot' wits)
 where
  era =
    ShelleyBasedEraAlonzo

--
-- Formatting
--

-- TODO: Maybe consider using 'renderValue' from cardano-api instead?
prettyValue :: Value -> Text
prettyValue value =
  let Lovelace lovelace = fromMaybe 0 (valueToLovelace value)
      (ada, decimal) = lovelace `quotRem` 1000000
      n = length (valueToList value)
   in unwords $
        [ show ada <> "." <> padLeft '0' 6 (show decimal)
        , "₳"
        ]
          ++ if n == 0
            then mempty
            else ["and", show n, "asset(s)"]

-- | Pad a text-string to left with the given character until it reaches the given
-- length.
--
-- NOTE: Truncate the string if longer than the given length.
-- TODO: Move into a separate module.
padLeft :: Char -> Int -> Text -> Text
padLeft c n str = T.takeEnd n (T.replicate n (T.singleton c) <> str)

--
-- Generators
--

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
genTx :: Utxo -> Gen CardanoTx
genTx utxos = do
  fromLedgerTx <$> Ledger.Generator.genTx genEnv ledgerEnv (utxoState, dpState)
 where
  utxoState = def{Ledger._utxo = toLedgerUtxo utxos}
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

genSequenceOfValidTransactions :: Utxo -> Gen [CardanoTx]
genSequenceOfValidTransactions initialUtxo
  | initialUtxo == mempty = pure []
  | otherwise = do
    n <- getSize
    numTxs <- choose (1, n)
    genFixedSizeSequenceOfValidTransactions numTxs initialUtxo

genFixedSizeSequenceOfValidTransactions :: Int -> Utxo -> Gen [CardanoTx]
genFixedSizeSequenceOfValidTransactions numTxs initialUtxo
  | initialUtxo == mempty = pure []
  | otherwise =
    reverse . snd <$> foldM newTx (initialUtxo, []) [1 .. numTxs]
 where
  newTx (utxos, acc) _ = do
    tx <- genTx utxos
    case applyTransactions cardanoLedger utxos [tx] of
      Left err -> error $ show err
      Right newUtxos -> pure (newUtxos, tx : acc)

-- TODO: Enable arbitrary datum in generators
genOutput ::
  forall era ctx.
  (IsShelleyBasedEra era) =>
  VerificationKey PaymentKey ->
  Gen (TxOut ctx era)
genOutput vk = do
  assets <- fromMaryValue <$> scale (* 8) arbitrary
  let value =
        either
          (`TxOutAdaOnly` selectLovelace assets)
          (`TxOutValue` assets)
          (multiAssetSupportedInEra (cardanoEra @era))
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone

genUtxo :: Gen Utxo
genUtxo = do
  genesisTxId <- arbitrary
  utxo <- Ledger.Generator.genUtxo0 (Ledger.Generator.genEnv Proxy)
  pure $ fromLedgerUtxo . Ledger.UTxO . Map.mapKeys (setTxId genesisTxId) $ Ledger.unUTxO utxo
 where
  setTxId ::
    Ledger.TxId Ledger.StandardCrypto ->
    Ledger.TxIn Ledger.StandardCrypto ->
    Ledger.TxIn Ledger.StandardCrypto
  setTxId baseId (Ledger.TxIn _ti wo) = Ledger.TxIn baseId wo

-- | Generate utxos owned by the given cardano key.
genUtxoFor :: VerificationKey PaymentKey -> Gen Utxo
genUtxoFor vk = do
  n <- arbitrary `suchThat` (> 0)
  inputs <- vectorOf n arbitrary
  outputs <- vectorOf n (genOutput vk)
  pure $ Utxo $ Map.fromList $ zip (fromShelleyTxIn <$> inputs) outputs

-- | Generate a single UTXO owned by 'vk'.
genOneUtxoFor :: VerificationKey PaymentKey -> Gen Utxo
genOneUtxoFor vk = do
  input <- arbitrary
  -- NOTE(AB): calling this generator while running a property will yield larger and larger
  -- values (quikcheck increases the 'size' parameter upon success) up to the point they are
  -- too large to fit in a transaction and validation fails in the ledger
  output <- scale (const 1) $ genOutput vk
  pure $ Utxo $ Map.singleton (fromShelleyTxIn input) output

--
-- Temporary / Quick-n-dirty
--

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
