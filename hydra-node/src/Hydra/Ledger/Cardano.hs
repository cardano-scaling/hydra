{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano where

import Hydra.Prelude hiding (id)

import Cardano.Binary (
  Annotator,
  decodeAnnotator,
  decodeBytes,
  decodeListLenOf,
  decodeWord,
  encodeBytes,
  encodeListLen,
  encodeWord,
  serialize',
  serializeEncoding',
 )
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Cardano
import Cardano.Ledger.BaseTypes (StrictMaybe (SNothing), boundRational, mkActiveSlotCoeff)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary (AuxiliaryData, MaryEra)
import qualified Cardano.Ledger.Mary.Value as Cardano
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Cardano
import qualified Cardano.Ledger.ShelleyMA.TxBody as Cardano
import Cardano.Ledger.Slot (EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import Control.Monad (foldM)
import Data.Aeson (
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSONKey,
  Value (String),
  decode,
  object,
  toJSONKey,
  withArray,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Default (def)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Ledger (Ledger (..), Tx (..), ValidationError (ValidationError))
import Shelley.Spec.Ledger.API (_maxTxSize)
import qualified Shelley.Spec.Ledger.API as Cardano hiding (TxBody)
import Shelley.Spec.Ledger.Tx (WitnessSetHKD (WitnessSet))
import Test.Cardano.Ledger.MaryEraGen ()
import Test.QuickCheck (Gen, choose, getSize)
import qualified Test.Shelley.Spec.Ledger.Generator.Constants as Constants
import Test.Shelley.Spec.Ledger.Generator.Core (geConstants)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)

cardanoLedger :: Ledger CardanoTx
cardanoLedger =
  Ledger
    { applyTransactions = \utxo ->
        applyTx ledgerEnv utxo . map convertTx
    , initUtxo = mempty
    }
 where
  convertTx CardanoTx{body, witnesses, auxiliaryData} =
    Cardano.Tx body witnesses auxiliaryData

-- * The 'CardanoTx' and associated types

data CardanoTx = CardanoTx
  { id :: Cardano.TxId StandardCrypto -- XXX(SN): make invalid values impossible to represent
  , body :: CardanoTxBody
  , witnesses :: CardanoTxWitnesses
  , auxiliaryData :: CardanoAuxiliaryData
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Tx CardanoTx where
  type Utxo CardanoTx = Cardano.UTxO CardanoEra
  type TxId CardanoTx = Cardano.TxId StandardCrypto

  txId CardanoTx{body} = Cardano.TxId $ SafeHash.hashAnnotated body

-- NOTE(SN): We do serialize CBOR-in-CBOR to utilize the 'FromCBOR (Annotator
-- (Tx era))' instance from the Cardano.Ledger.Tx

instance ToCBOR CardanoTx where
  toCBOR CardanoTx{body, witnesses, auxiliaryData} =
    encodeBytes $ serialize' $ Cardano.Tx @CardanoEra body witnesses auxiliaryData

instance FromCBOR CardanoTx where
  fromCBOR = do
    bs <- decodeBytes
    case decodeAnnotator "Cardano.Tx" fromCBOR (fromStrict bs) of
      Left err -> fail $ show err
      Right (Cardano.Tx body witnesses auxiliaryData :: Cardano.Tx CardanoEra) ->
        pure $
          CardanoTx
            { id = Cardano.TxId $ SafeHash.hashAnnotated body
            , body
            , witnesses
            , auxiliaryData
            }

instance Arbitrary CardanoTx where
  arbitrary = genUtxo >>= genCardanoTx

genUtxo :: Gen (Utxo CardanoTx)
genUtxo = genUtxo0 (genEnv Proxy)

genCardanoTx :: Utxo CardanoTx -> Gen CardanoTx
genCardanoTx utxos = do
  let utxoState = def{Cardano._utxo = utxos}
      dpState = Cardano.DPState def def
      -- NOTE(AB): This sets some parameters for the tx generator that will
      -- affect the structure of generated trasactions. In our case, we want
      -- to remove "special" capabilities which are irrelevant in the context
      -- of a Hydra head
      -- see https://github.com/input-output-hk/cardano-ledger-specs/blob/nil/shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Generator/Constants.hs#L10
      generatorEnv = (genEnv Proxy){geConstants = noPPUpdatesTransactions}
  tx <- genTx generatorEnv ledgerEnv (utxoState, dpState)
  case tx of
    (Cardano.Tx body wits aux) ->
      pure $
        CardanoTx
          (Cardano.TxId $ SafeHash.hashAnnotated body)
          body
          wits
          aux

genSequenceOfValidTransactions :: Utxo CardanoTx -> Gen [CardanoTx]
genSequenceOfValidTransactions initialUtxos = do
  n <- getSize
  numTxs <- choose (1, n)
  reverse . snd <$> foldM newTx (initialUtxos, []) [1 .. numTxs]
 where
  newTx (utxos, acc) _ = do
    tx <- genCardanoTx utxos
    case applyTransactions cardanoLedger utxos [tx] of
      Left err -> error $ show err
      Right newUtxos -> pure (newUtxos, tx : acc)

noPPUpdatesTransactions :: Constants.Constants
noPPUpdatesTransactions =
  Constants.defaultConstants{Constants.frequencyTxUpdates = 0}

type CardanoEra = MaryEra StandardCrypto

type CardanoTxBody = Cardano.TxBody CardanoEra

type CardanoTxWitnesses = Cardano.WitnessSet CardanoEra

type CardanoAuxiliaryData = StrictMaybe (AuxiliaryData CardanoEra)

--
--  Transaction Id
--

instance Crypto crypto => ToJSON (Cardano.TxId crypto) where
  toJSON = String . txIdToText @crypto

instance Crypto crypto => FromJSON (Cardano.TxId crypto) where
  parseJSON = withText "base16 encoded TxId" txIdFromText

txIdToText :: Cardano.TxId crypto -> Text
txIdToText (Cardano.TxId h) =
  encodeBase16 $ Crypto.hashToBytes $ SafeHash.extractHash h

txIdFromText :: (Crypto crypto, MonadFail m) => Text -> m (Cardano.TxId crypto)
txIdFromText t =
  case decodeBase16 (encodeUtf8 t) of
    Left e -> fail $ "decoding base16: " <> show e
    Right bytes ->
      case Crypto.hashFromBytes bytes of
        Nothing -> fail "hashFromBytes yielded Nothing"
        Just h -> pure . Cardano.TxId $ SafeHash.unsafeMakeSafeHash h

--
-- Transaction Body
--

-- TODO(AB): serialise more fields from the transaction body
instance FromJSON CardanoTxBody where
  parseJSON = withObject "CardanoTxBody" $ \o -> do
    inputs <- o .: "inputs" >>= traverse parseJSON
    outputs <- o .: "outputs" >>= traverse parseJSON
    pure $
      Cardano.TxBody
        (Set.fromList inputs)
        (StrictSeq.fromList outputs)
        mempty
        (Cardano.Wdrl mempty)
        mempty
        (Cardano.ValidityInterval SNothing SNothing)
        Cardano.SNothing
        Cardano.SNothing
        mempty

instance ToJSON CardanoTxBody where
  toJSON (Cardano.TxBody inputs outputs _certs _wdrls _txfee _vldt _update _adHash _mint) =
    object
      [ "inputs" .= inputs
      , "outputs" .= outputs
      ]

--
-- Input
--

instance Crypto crypto => ToJSON (Cardano.TxIn crypto) where
  toJSON = toJSON . txInToText

instance Crypto crypto => FromJSON (Cardano.TxIn crypto) where
  parseJSON = withText "TxIn" txInFromText

txInFromText :: (Crypto crypto, MonadFail m) => Text -> m (Cardano.TxIn crypto)
txInFromText t = do
  let (txIdText, txIxText) = Text.breakOn "#" t
  Cardano.TxIn
    <$> txIdFromText txIdText
    <*> parseIndex txIxText
 where
  parseIndex txIxText =
    maybe
      (fail $ "cannot parse " <> show txIxText <> " as a natural index")
      pure
      (decode (encodeUtf8 $ Text.drop 1 txIxText))

txInToText :: Crypto crypto => Cardano.TxIn crypto -> Text
txInToText (Cardano.TxIn id ix) = txIdToText id <> "#" <> show ix

instance Crypto crypto => FromJSONKey (Cardano.TxIn crypto) where
  fromJSONKey = FromJSONKeyTextParser txInFromText

instance Crypto crypto => ToJSONKey (Cardano.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

--
-- Output
--

instance Crypto crypto => ToJSON (Cardano.TxOut (MaryEra crypto)) where
  toJSON (Cardano.TxOut addr value) =
    object
      [ "address" .= serialiseAddressBech32
      , "value" .= valueToJson value
      ]
   where
    -- Serialise addresses in bech32 including the prefix as standardized:
    -- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/CIP-0005.md
    serialiseAddressBech32 =
      Bech32.encodeLenient prefix . Bech32.dataPartFromBytes $ Cardano.serialiseAddr addr

    -- REVIEW(SN): The ledger's 'Addr' type is bigger than we need here and we
    -- are forced to come up with a prefix for Byron "bootstrap" addresses,
    -- although they should actually be serialised differently..and would not be
    -- relevant for Hydra in the first place.
    prefix = case addr of
      (Cardano.Addr Cardano.Mainnet _ _) -> [Bech32.humanReadablePart|addr|]
      (Cardano.Addr Cardano.Testnet _ _) -> [Bech32.humanReadablePart|addr_test|]
      (Cardano.AddrBootstrap _) -> [Bech32.humanReadablePart|addr_boot|]

instance Crypto crypto => FromJSON (Cardano.TxOut (MaryEra crypto)) where
  parseJSON = withObject "TxOut" $ \o -> do
    address <- o .: "address" >>= deserialiseAddressBech32
    value <- o .: "value" >>= valueParseJson
    pure $ Cardano.TxOut address value
   where
    valueParseJson = withObject "Value" $ \o ->
      Cardano.Value <$> o .: "lovelace" <*> pure mempty

    deserialiseAddressBech32 t =
      case Bech32.decodeLenient t of
        Left err -> fail $ "failed to decode bech32: " <> show err
        Right (prefix, dataPart) ->
          let mAddr = Bech32.dataPartToBytes dataPart >>= Cardano.deserialiseAddr
           in case (Bech32.humanReadablePartToText prefix, mAddr) of
                ("addr", Just addr@(Cardano.Addr Cardano.Mainnet _ _)) -> pure addr
                ("addr_test", Just addr@(Cardano.Addr Cardano.Testnet _ _)) -> pure addr
                ("addr_boot", Just addr@(Cardano.AddrBootstrap _)) -> pure addr
                (p, Just _) -> fail $ "invalid bech32 prefix: " <> show p
                (_, Nothing) -> fail "failed to decode data part"

valueToJson :: Cardano.Value crypto -> Value
valueToJson (Cardano.Value lovelace _assets) =
  object ["lovelace" .= lovelace]

--
-- Utxo
--

instance Semigroup (Cardano.UTxO (MaryEra crypto)) where
  Cardano.UTxO u1 <> Cardano.UTxO u2 = Cardano.UTxO (u1 <> u2)

instance Monoid (Cardano.UTxO (MaryEra crypto)) where
  mempty = Cardano.UTxO mempty

instance Crypto crypto => ToJSON (Cardano.UTxO (MaryEra crypto)) where
  toJSON = toJSON . Cardano.unUTxO

instance Crypto crypto => FromJSON (Cardano.UTxO (MaryEra crypto)) where
  parseJSON v = Cardano.UTxO <$> parseJSON v

--
-- Witnesses
--

instance ToJSON CardanoTxWitnesses where
  toJSON (WitnessSet addrWitnesses scriptWitnesses _) =
    object
      [ "addresses" .= addrWitnessesAsJSON
      , "scripts" .= scriptWitnesses
      ]
   where
    addrWitnessesAsJSON = toJSON $ map (encodeBase16 . serializeEncoding' . prefixWithTag) $ toList addrWitnesses
    prefixWithTag wit = encodeListLen 2 <> encodeWord 0 <> toCBOR wit

instance
  (FromCBOR (Annotator (Cardano.WitVKey 'Cardano.Witness StandardCrypto))) =>
  FromJSON CardanoTxWitnesses
  where
  parseJSON = withObject "CardanoTxWitnesses" $ \obj -> do
    addrWits <- obj .: "addresses" >>= parseAddressWitnesses
    scriptWits <- obj .: "scripts"
    pure $ WitnessSet addrWits scriptWits mempty
   where
    parseAddressWitnesses = withArray "CardanoTxWitnesses" $ \a -> do
      Set.fromList . toList <$> traverse parseAddressWitness a

    parseAddressWitness = withText "AddrWitness" $ \t ->
      -- TODO(AB): this is ugly
      case decodeBase16 $ encodeUtf8 t of
        Left err -> fail $ show err
        Right bs' -> case decodeAnnotator "ShelleyKeyWitness" decoder (fromStrict bs') of
          Left err -> fail $ show err
          Right v -> pure v

    decoder = do
      decodeListLenOf 2
      t <- decodeWord
      case t of
        0 -> fromCBOR
        _ -> fail $ "Invalid tag decoding witness, only support 1: " <> show t

instance ToJSON (Cardano.Timelock StandardCrypto) where
  toJSON = String . encodeBase16 . serialize'

instance FromJSON (Cardano.Timelock StandardCrypto) where
  parseJSON = withText "Timelock" $ \t ->
    case decodeBase16 $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeAnnotator "ShelleyKeyWitness" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

instance ToJSONKey (Cardano.ScriptHash StandardCrypto) where
  toJSONKey = toJSONKeyText (\(Cardano.ScriptHash h) -> encodeBase16 (Crypto.hashToBytes h))

instance FromJSONKey (Cardano.ScriptHash StandardCrypto) where
  fromJSONKey = FromJSONKeyTextParser hashFromText
   where
    hashFromText t =
      case decodeBase16 (encodeUtf8 t) of
        Left e -> fail $ "decoding base16: " <> show e
        Right bytes ->
          case Crypto.hashFromBytes bytes of
            Nothing -> fail "hashFromBytes yielded Nothing"
            Just h -> pure $ Cardano.ScriptHash h

--
-- AuxiliaryData
--

instance ToJSON (AuxiliaryData CardanoEra) where
  toJSON = String . encodeBase16 . serialize'

instance FromJSON (AuxiliaryData CardanoEra) where
  parseJSON = withText "Hex-encoded auxiliary data" $ \t ->
    case decodeBase16 $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeAnnotator "AuxiliaryData" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

-- * Calling the Cardano ledger

applyTx ::
  Cardano.LedgerEnv CardanoEra ->
  Cardano.UTxO CardanoEra ->
  [Cardano.Tx CardanoEra] ->
  Either ValidationError (Cardano.UTxO CardanoEra)
applyTx env utxo txs =
  case Cardano.applyTxsTransition globals env (Seq.fromList txs) memPoolState of
    Left err -> Left $ toValidationError err
    Right (ls, _ds) -> Right $ Cardano._utxo ls
 where
  toValidationError = ValidationError . show

  memPoolState = (def{Cardano._utxo = utxo}, def)

-- TODO(SN): not hard-code these obviously
-- From: shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Utils.hs
globals :: Cardano.Globals
globals =
  Cardano.Globals
    { Cardano.epochInfoWithErr = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
    , Cardano.slotsPerKESPeriod = 20
    , Cardano.stabilityWindow = 33
    , Cardano.randomnessStabilisationWindow = 33
    , Cardano.securityParameter = 10
    , Cardano.maxKESEvo = 10
    , Cardano.quorum = 5
    , Cardano.maxMajorPV = 1000
    , Cardano.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , Cardano.activeSlotCoeff = mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Cardano.networkId = Cardano.Testnet
    , Cardano.systemStart = SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ boundRational r

-- TODO(SN): not hard-code this
ledgerEnv :: Cardano.LedgerEnv CardanoEra
ledgerEnv =
  Cardano.LedgerEnv
    { Cardano.ledgerSlotNo = SlotNo 1
    , Cardano.ledgerIx = 0
    , -- TODO(AB): should be parameterized
      Cardano.ledgerPp = def{_maxTxSize = 1024 * 1024}
    , Cardano.ledgerAccount = error "mkLedgerenv ledgersAccount undefined"
    }
