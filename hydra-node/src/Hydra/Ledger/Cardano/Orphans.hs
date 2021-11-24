{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans instances (mostly ToJSON/FromJSON) required by Hydra.Ledger.Cardano
-- to satisfies our various internal interfaces.
--
-- Nothing to see here.
module Hydra.Ledger.Cardano.Orphans where

import Hydra.Prelude

import Cardano.Api (AlonzoEra, AssetName (AssetName), CtxUTxO, ShelleyBasedEra (ShelleyBasedEraAlonzo), TxIn, TxOut)
import Cardano.Api.Orphans ()
import Cardano.Api.Shelley (fromShelleyTxIn, fromShelleyTxOut)
import Cardano.Binary (
  Annotator,
  decodeAnnotator,
  decodeFull',
  decodeListLenOf,
  decodeWord,
  encodeListLen,
  encodeWord,
  serialize',
  serializeEncoding',
 )
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger.Mary
import qualified Codec.Binary.Bech32 as Bech32
import Data.Aeson (
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSONKey,
  Value (String),
  decode,
  object,
  toJSONKey,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Types (
  Parser,
  mapFromJSONKeyFunction,
  toJSONKeyText,
 )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as B8
import Data.Maybe.Strict (StrictMaybe (..), maybeToStrictMaybe)
import qualified Data.Text as Text
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

--
-- Addr
--
-- NOTE: ToJSON instance defined in cardano-ledger-specs
-- NOTE: Not defining 'FromJSON' because of conflicts with cardano-ledger-specs

decodeAddress ::
  Crypto crypto =>
  Text ->
  Parser (Ledger.Addr crypto)
decodeAddress t =
  decodeBech32 <|> decodeBase16
 where
  decodeBech32 =
    case Bech32.decodeLenient t of
      Left err ->
        fail $ "failed to decode from bech32: " <> show err
      Right (_prefix, dataPart) ->
        case Bech32.dataPartToBytes dataPart >>= Ledger.deserialiseAddr of
          Nothing -> fail "failed to deserialise addresse."
          Just addr -> pure addr

  decodeBase16 =
    parseJSON (String t)

--
-- AssetName
--

-- TODO(SN): these Ledger.Mary.AssetName instances are only used when
-- deserializing a Ledger.TxBody

-- NOTE: cardano-api uses 'decodeLatin1' to convert encoded base16 asset
-- names; there's no 'encodeLatin1' in Data.Text.Encoding but the doc says:
--
--    decodeLatin1 is semantically equivalent to Data.Text.pack . Data.ByteString.Char8.unpack
--
encodeLatin1 :: Text -> ByteString
encodeLatin1 = B8.pack . Text.unpack

instance FromJSON Ledger.Mary.AssetName where
  parseJSON = withText "AssetName" $ \t ->
    case Base16.decode $ encodeLatin1 t of
      Left err -> fail $ show err
      Right bs -> pure $ Ledger.Mary.AssetName bs

instance FromJSONKey Ledger.Mary.AssetName where
  fromJSONKey = FromJSONKeyTextParser nameFromText
   where
    nameFromText t =
      case Base16.decode (encodeLatin1 t) of
        Left e -> fail $ "failed to decode from base16: " <> show e
        Right bytes -> pure $ Ledger.Mary.AssetName bytes

instance Arbitrary AssetName where
  arbitrary = AssetName . BS.take 32 <$> arbitrary

--
-- AuxiliaryData
--

instance ToCBOR (Ledger.Alonzo.AuxiliaryData era) => ToJSON (Ledger.Alonzo.AuxiliaryData era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromCBOR (Annotator (Ledger.Alonzo.AuxiliaryData era)) => FromJSON (Ledger.Alonzo.AuxiliaryData era) where
  parseJSON = withText "AuxiliaryData" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "AuxiliaryData" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

instance ToJSON (Ledger.AuxiliaryDataHash crypto) where
  toJSON =
    String
      . decodeUtf8
      . Base16.encode
      . Crypto.hashToBytes
      . Ledger.extractHash
      . Ledger.unsafeAuxiliaryDataHash

instance Crypto crypto => FromJSON (Ledger.AuxiliaryDataHash crypto) where
  parseJSON = fmap Ledger.AuxiliaryDataHash . parseJSON

--
-- Bootstrap Witness
--

instance Crypto crypto => ToJSON (Ledger.BootstrapWitness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromCBOR (Annotator (Ledger.BootstrapWitness crypto)) => FromJSON (Ledger.BootstrapWitness crypto) where
  parseJSON = withText "BootstrapWitness" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "BootstrapWitness" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- DCert
--
-- TODO: Delegation certificates can actually be represented as plain JSON
-- objects (it's a sum type), so we may want to revisit this interface later?

instance Crypto crypto => ToJSON (Ledger.DCert crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance Crypto crypto => FromJSON (Ledger.DCert crypto) where
  parseJSON = withText "DCert" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

--
-- IsValid
--

instance ToJSON Ledger.Alonzo.IsValid where
  toJSON (Ledger.Alonzo.IsValid b) = toJSON b

instance FromJSON Ledger.Alonzo.IsValid where
  parseJSON = fmap Ledger.Alonzo.IsValid . parseJSON

--
-- PolicyID
--
-- NOTE: ToJSON instance defined in Cardano.Api.Orphans

instance Crypto crypto => FromJSON (Ledger.Mary.PolicyID crypto) where
  parseJSON = fmap Ledger.Mary.PolicyID . parseJSON

instance Crypto crypto => FromJSONKey (Ledger.Mary.PolicyID crypto) where
  fromJSONKey = mapFromJSONKeyFunction Ledger.Mary.PolicyID fromJSONKey

--
-- Redeemers
--
-- TODO: Provide maybe better instances for redeemers from which we can actually
-- view them as a map from pointers to data?

instance
  ( FromCBOR (Annotator (Ledger.Alonzo.Redeemers era))
  ) =>
  FromJSON (Ledger.Alonzo.Redeemers era)
  where
  parseJSON = withText "Redeemers" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeAnnotator "Redeemers" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

instance ToCBOR (Ledger.Alonzo.Redeemers era) => ToJSON (Ledger.Alonzo.Redeemers era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

--
-- SafeHash
--

instance Crypto crypto => ToJSONKey (Ledger.SafeHash crypto any) where
  toJSONKey = toJSONKeyText safeHashToText

safeHashToText ::
  Ledger.SafeHash crypto any ->
  Text
safeHashToText =
  decodeUtf8 . Base16.encode . Crypto.hashToBytes . Ledger.extractHash

instance Crypto crypto => FromJSON (Ledger.SafeHash crypto any) where
  parseJSON = withText "SafeHash" safeHashFromText

instance Crypto crypto => FromJSONKey (Ledger.SafeHash crypto any) where
  fromJSONKey = FromJSONKeyTextParser safeHashFromText

safeHashFromText ::
  (Crypto crypto, MonadFail m) =>
  Text ->
  m (Ledger.SafeHash crypto any)
safeHashFromText t =
  case Crypto.hashFromTextAsHex t of
    Nothing -> fail "failed to decode from base16."
    Just h -> pure $ Ledger.unsafeMakeSafeHash h

--
-- Script
--

instance
  ( ToCBOR (Ledger.Alonzo.Script era)
  ) =>
  ToJSON (Ledger.Alonzo.Script era)
  where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance
  ( Crypto (Ledger.Crypto era)
  , Typeable era
  ) =>
  FromJSON (Ledger.Alonzo.Script era)
  where
  parseJSON = withText "Script" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeAnnotator "Script" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- ScriptHash
--

instance Crypto crypto => ToJSONKey (Ledger.ScriptHash crypto) where
  toJSONKey = toJSONKeyText $ \(Ledger.ScriptHash h) ->
    decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)

instance Crypto crypto => FromJSONKey (Ledger.ScriptHash crypto) where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case Crypto.hashFromTextAsHex t of
      Nothing -> fail "failed to decode from base16."
      Just h -> pure $ Ledger.ScriptHash h

--
-- Timelock
--

instance ToJSON (Ledger.Mary.Timelock StandardCrypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (Ledger.Mary.Timelock StandardCrypto) where
  parseJSON = withText "Timelock" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "Timelock" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- TxBody
--

instance
  ( Ledger.Alonzo.AlonzoBody era
  , Show (Core.Value era)
  , ToJSON (Core.Value era)
  , ToJSON (Core.AuxiliaryData era)
  , Era era
  ) =>
  ToJSON (Ledger.Alonzo.TxBody era)
  where
  toJSON b =
    object
      [ "inputs" .= Ledger.Alonzo.inputs' b
      , "collateral" .= Ledger.Alonzo.collateral' b
      , "outputs" .= Ledger.Alonzo.outputs' b
      , "certificates" .= Ledger.Alonzo.certs' b
      , "withdrawals" .= Ledger.Alonzo.wdrls' b
      , "fees" .= Ledger.Alonzo.txfee' b
      , "validity" .= Ledger.Alonzo.vldt' b
      , "requiredSignatures" .= Ledger.Alonzo.reqSignerHashes' b
      , "mint" .= Ledger.Alonzo.mint' b
      , "scriptIntegrityHash" .= Ledger.Alonzo.scriptIntegrityHash' b
      , "auxiliaryDataHash" .= Ledger.Alonzo.adHash' b
      , "networkId" .= Ledger.Alonzo.txnetworkid' b
      ]

instance
  ( Ledger.Alonzo.AlonzoBody era
  , Show (Core.Value era)
  , FromJSON (Core.Value era)
  , FromJSON (Core.AuxiliaryData era)
  ) =>
  FromJSON (Ledger.Alonzo.TxBody era)
  where
  parseJSON = withObject "TxBody" $ \o -> do
    Ledger.Alonzo.TxBody
      <$> (o .: "inputs")
      <*> (o .:? "collateral" .!= mempty)
      <*> (o .: "outputs")
      <*> (o .:? "certificates" .!= mempty)
      <*> (o .:? "withdrawals" .!= Ledger.Wdrl mempty)
      <*> (o .:? "fees" .!= mempty)
      <*> (o .:? "validity" .!= Ledger.Mary.ValidityInterval SNothing SNothing)
      <*> pure SNothing -- TODO: Protocol Updates? Likely irrelevant to the L2.
      <*> (o .:? "requiredSignatures" .!= mempty)
      <*> (o .:? "mint" .!= mempty)
      <*> (o .:? "scriptIntegrityHash" .!= SNothing)
      <*> (o .:? "auxiliaryDataHash" .!= SNothing)
      <*> (o .:? "networkId" .!= SNothing)

--
-- TxDats
--

instance
  ( Typeable era
  , Crypto (Ledger.Crypto era)
  ) =>
  ToJSON (Ledger.Alonzo.TxDats era)
  where
  toJSON (Ledger.Alonzo.TxDats datums) = toJSON datums

instance
  ( Typeable era
  , Crypto (Ledger.Crypto era)
  ) =>
  FromJSON (Ledger.Alonzo.TxDats era)
  where
  parseJSON = fmap Ledger.Alonzo.TxDats . parseJSON

instance
  ( Typeable era
  ) =>
  ToJSON (Ledger.Alonzo.Data era)
  where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance
  ( Typeable era
  ) =>
  FromJSON (Ledger.Alonzo.Data era)
  where
  parseJSON = withText "Data" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "Data" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- TxId
--

instance Crypto crypto => ToJSON (Ledger.TxId crypto) where
  toJSON = String . txIdToText @crypto

txIdToText :: Ledger.TxId crypto -> Text
txIdToText (Ledger.TxId h) = safeHashToText h

instance Crypto crypto => FromJSON (Ledger.TxId crypto) where
  parseJSON = withText "TxId" txIdFromText

txIdFromText :: (Crypto crypto, MonadFail m) => Text -> m (Ledger.TxId crypto)
txIdFromText = fmap Ledger.TxId . safeHashFromText

--
-- TxIn
--

instance Crypto crypto => FromJSON (Ledger.TxIn crypto) where
  parseJSON = withText "TxIn" txInFromText

txInFromText :: (Crypto crypto, MonadFail m) => Text -> m (Ledger.TxIn crypto)
txInFromText t = do
  let (txIdText, txIxText) = Text.breakOn "#" t
  Ledger.TxIn
    <$> txIdFromText txIdText
    <*> parseIndex txIxText
 where
  parseIndex txIxText =
    maybe
      (fail $ "cannot parse " <> show txIxText <> " as a natural index")
      pure
      (decode (encodeUtf8 $ Text.drop 1 txIxText))

instance FromJSONKey TxIn where
  fromJSONKey = FromJSONKeyTextParser (fmap fromShelleyTxIn . txInFromText)

instance Arbitrary TxIn where
  arbitrary = fromShelleyTxIn <$> arbitrary

--
-- TxOut
--
-- NOTE: ToJSON defined in Cardano.Api.Orphans

instance
  ( Era era
  , Show (Core.Value era)
  , FromJSON (Core.Value era)
  , FromJSON (Ledger.Alonzo.DataHash (Ledger.Crypto era))
  ) =>
  FromJSON (Ledger.Alonzo.TxOut era)
  where
  parseJSON = withObject "TxOut" $ \o ->
    Ledger.Alonzo.TxOut
      <$> (o .: "address" >>= decodeAddress)
      <*> o .: "value"
      <*> fmap maybeToStrictMaybe (o .:? "datahash")

instance Arbitrary (TxOut CtxUTxO AlonzoEra) where
  arbitrary = fromShelleyTxOut ShelleyBasedEraAlonzo <$> arbitrary

--
-- TxWitness
--

instance
  ( ToJSON (Core.Script era)
  , Core.Script era ~ Ledger.Alonzo.Script era
  , Era era
  ) =>
  ToJSON (Ledger.Alonzo.TxWitness era)
  where
  toJSON (Ledger.Alonzo.TxWitness vkeys boots scripts datums redeemers) =
    object
      [ "keys" .= vkeys
      , "bootstrap" .= boots
      , "scripts" .= scripts
      , "datums" .= datums
      , "redeemers" .= redeemers
      ]

instance
  ( FromJSON (Core.Script era)
  , Core.Script era ~ Ledger.Alonzo.Script era
  , Era era
  ) =>
  FromJSON (Ledger.Alonzo.TxWitness era)
  where
  parseJSON = withObject "TxWitness" $ \o ->
    Ledger.Alonzo.TxWitness
      <$> (o .:? "keys" .!= mempty)
      <*> (o .:? "bootstrap" .!= mempty)
      <*> (o .:? "scripts" .!= mempty)
      <*> (o .:? "datums" .!= Ledger.Alonzo.TxDats mempty)
      <*> (o .:? "redeemers" .!= Ledger.Alonzo.Redeemers mempty)

--
-- ValidatedTx
--

instance
  ( ToJSON (Ledger.Alonzo.TxWitness era)
  , ToJSON (Core.TxBody era)
  , ToJSON (Core.AuxiliaryData era)
  , ToJSON (Core.Script era)
  , Core.Script era ~ Ledger.Alonzo.Script era
  , Era era
  ) =>
  ToJSON (Ledger.Alonzo.ValidatedTx era)
  where
  toJSON (Ledger.Alonzo.ValidatedTx body witnesses isValid auxiliaryData) =
    object
      [ "body" .= body
      , "witnesses" .= witnesses
      , "isValid" .= isValid
      , "auxiliaryData" .= auxiliaryData
      ]

instance
  ( FromJSON (Core.TxBody era)
  , FromJSON (Core.AuxiliaryData era)
  , FromJSON (Core.Script era)
  , Core.Script era ~ Ledger.Alonzo.Script era
  , Era era
  ) =>
  FromJSON (Ledger.Alonzo.ValidatedTx era)
  where
  parseJSON = withObject "Tx" $ \o ->
    Ledger.Alonzo.ValidatedTx
      <$> o .: "body"
      <*> o .: "witnesses"
      <*> o .:? "isValid" .!= Ledger.Alonzo.IsValid True
      <*> o .:? "auxiliaryData" .!= SNothing

--
-- ValidityInterval
--

instance ToJSON Ledger.Mary.ValidityInterval where
  toJSON (Ledger.Mary.ValidityInterval notBefore notAfter) =
    object
      [ "notBefore" .= notBefore
      , "notAfter" .= notAfter
      ]

instance FromJSON Ledger.Mary.ValidityInterval where
  parseJSON = withObject "ValidityInterval" $ \obj ->
    Ledger.Mary.ValidityInterval
      <$> obj .: "notBefore"
      <*> obj .: "notAfter"

--
-- Value
--
-- NOTE: ToJSON defined in Cardano.Api.Orphans

instance Crypto crypto => FromJSON (Ledger.Mary.Value crypto) where
  parseJSON = withObject "Value" $ \o ->
    Ledger.Mary.Value
      <$> o .: "lovelace"
      <*> o .:? "policies" .!= mempty

--
-- Wdrl
--

instance Crypto crypto => ToJSON (Ledger.Wdrl crypto) where
  toJSON = toJSON . Ledger.unWdrl

instance Crypto crypto => FromJSON (Ledger.Wdrl crypto) where
  parseJSON v = Ledger.Wdrl <$> parseJSON v

--
-- WitVKey
--

instance Crypto crypto => ToJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serializeEncoding' . prefixWithTag
   where
    prefixWithTag wit = encodeListLen 2 <> encodeWord 0 <> toCBOR wit

instance Crypto crypto => FromJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
  parseJSON = withText "VKeyWitness" $ \t ->
    -- TODO(AB): this is ugly
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeAnnotator "ShelleyKeyWitness" decoder (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v
   where
    decoder = do
      decodeListLenOf 2
      t <- decodeWord
      case t of
        0 -> fromCBOR
        _ -> fail $ "Invalid tag decoding key witness, only support 1: " <> show t
