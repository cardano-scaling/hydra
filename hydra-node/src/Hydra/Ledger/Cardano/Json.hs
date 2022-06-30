{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans ToJSON/FromJSON instances required by Hydra.Ledger.Cardano
-- to satisfies our various internal interfaces.
module Hydra.Ledger.Cardano.Json where

import Hydra.Cardano.Api hiding (Era)
import Hydra.Prelude

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
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Babbage.Tx as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.SafeHash as Ledger
import Cardano.Ledger.Serialization (Sized, mkSized)
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger.Mary
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.Binary.Bech32 as Bech32
import Data.Aeson (
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSONKey,
  Value (String),
  object,
  toJSONKey,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (
  Pair,
  Parser,
  toJSONKeyText,
 )
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import qualified Data.Set as Set
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
  decodeBech32 <|> parseJSON (String t)
 where
  decodeBech32 =
    case Bech32.decodeLenient t of
      Left err ->
        fail $ "failed to decode from bech32: " <> show err
      Right (_prefix, dataPart) ->
        case Bech32.dataPartToBytes dataPart >>= Ledger.deserialiseAddr of
          Nothing -> fail "failed to deserialise addresse."
          Just addr -> pure addr

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

instance ToJSON Ledger.Babbage.IsValid where
  toJSON (Ledger.Babbage.IsValid b) = toJSON b

instance FromJSON Ledger.Babbage.IsValid where
  parseJSON = fmap Ledger.Babbage.IsValid . parseJSON

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
-- RewardAcnt
--

-- NOTE: The Ledge derive generic ToJSONKey from 'RewardAcnt', which by default
-- turn them into an array of elements.
rewardAcntToText :: Ledger.RewardAcnt crypto -> Text
rewardAcntToText = decodeUtf8 . Base16.encode . Ledger.serialiseRewardAcnt

rewardAcntFromText :: Crypto crypto => Text -> Maybe (Ledger.RewardAcnt crypto)
rewardAcntFromText t = do
  case Base16.decode (encodeUtf8 t) of
    Left{} -> Nothing
    Right bs -> Ledger.deserialiseRewardAcnt bs

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

instance ToJSON (Ledger.Babbage.TxBody LedgerEra) where
  toJSON b =
    object $
      mconcat
        [ onlyIf (const True) "inputs" (Set.map fromLedgerTxIn (Ledger.Babbage.inputs b))
        , onlyIf (not . null) "collateral" (Set.map fromLedgerTxIn (Ledger.Babbage.collateral b))
        , onlyIf (not . null) "referenceInputs" (Set.map fromLedgerTxIn (Ledger.Babbage.referenceInputs b))
        , onlyIf (const True) "outputs" (fromLedgerTxOut <$> Ledger.Babbage.outputs' b)
        , onlyIf isSJust "collateralReturn" (fromLedgerTxOut <$> Ledger.Babbage.collateralReturn' b)
        , onlyIf isSJust "totalCollateral" (Ledger.Babbage.totalCollateral b)
        , onlyIf (not . null) "certificates" (Ledger.Babbage.txcerts b)
        , onlyIf (not . null . Ledger.unWdrl) "withdrawals" (Ledger.Babbage.txwdrls b)
        , onlyIf (const True) "fees" (Ledger.Babbage.txfee b)
        , onlyIf (not . isOpenInterval) "validity" (Ledger.Babbage.txvldt b)
        , onlyIf (not . null) "requiredSignatures" (Ledger.Babbage.reqSignerHashes b)
        , onlyIf (/= mempty) "mint" (fromLedgerValue (Ledger.Babbage.mint b))
        , onlyIf isSJust "scriptIntegrityHash" (Ledger.Babbage.scriptIntegrityHash b)
        , onlyIf isSJust "auxiliaryDataHash" (Ledger.Babbage.adHash b)
        , onlyIf isSJust "networkId" (Ledger.Babbage.txnetworkid b)
        ]

instance (ToCBOR a, FromJSON a) => FromJSON (Sized a) where
  parseJSON =
    fmap mkSized . parseJSON

instance
  ( Ledger.Babbage.BabbageBody era
  , Show (Core.Value era)
  , FromJSON (Core.Value era)
  , FromJSON (Ledger.Mary.Value (Ledger.Crypto era))
  , FromJSON (Core.AuxiliaryData era)
  , FromJSON (Ledger.TxIn (Ledger.Crypto era))
  , FromJSON (Ledger.Babbage.TxOut era)
  ) =>
  FromJSON (Ledger.Babbage.TxBody era)
  where
  parseJSON = withObject "TxBody" $ \o -> do
    Ledger.Babbage.TxBody
      <$> (o .: "inputs")
      <*> (o .:? "collateral" .!= mempty)
      <*> (o .:? "referenceInputs" .!= mempty)
      <*> (o .: "outputs")
      <*> (o .:? "collateralReturn" .!= SNothing)
      <*> (o .:? "totalCollateral" .!= SNothing)
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

instance FromJSON (Ledger.TxIn StandardCrypto) where
  parseJSON = fmap toLedgerTxIn . parseJSON

--
-- TxOut
--

instance FromJSON (Ledger.Babbage.TxOut LedgerEra) where
  parseJSON = fmap toLedgerTxOut . parseJSON

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
    object $
      mconcat
        [ onlyIf (not . null) "keys" vkeys
        , onlyIf (not . null) "bootstrap" boots
        , onlyIf (not . null) "scripts" scripts
        , onlyIf (not . Ledger.Alonzo.nullDats) "datums" datums
        , onlyIf (not . Ledger.Alonzo.nullRedeemers) "redeemers" redeemers
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
  ToJSON (Ledger.Babbage.ValidatedTx era)
  where
  toJSON (Ledger.Babbage.ValidatedTx body witnesses isValid auxiliaryData) =
    object $
      mconcat
        [ ["id" .= Ledger.txid body]
        , ["body" .= body]
        , ["witnesses" .= witnesses]
        , ["isValid" .= isValid]
        , onlyIf isSJust "auxiliaryData" auxiliaryData
        ]

instance
  ( FromJSON (Core.TxBody era)
  , FromJSON (Core.AuxiliaryData era)
  , FromJSON (Core.Script era)
  , FromCBOR (Annotator (Core.TxBody era))
  , FromCBOR (Annotator (Core.AuxiliaryData era))
  , FromCBOR (Annotator (Core.Witnesses era))
  , Core.Script era ~ Ledger.Alonzo.Script era
  , Ledger.ValidateScript era
  , Era era
  ) =>
  FromJSON (Ledger.Babbage.ValidatedTx era)
  where
  parseJSON value =
    -- We accepts transactions in three forms:
    --
    -- (a) As high-level JSON object, which full format is specified via a
    -- JSON-schema.
    --
    -- (b) As a JSON 'text-envelope', which is a format defined and produced by
    -- the cardano-cli, wrapping base16-encoded strings as JSON objects with
    -- tags.
    --
    -- (c) As base16 string representing a CBOR-serialized transaction, since
    -- this is the most common medium of exchange used for transactions.
    parseAsBase16CBOR value
      <|> parseAsEnvelopedBase16CBOR value
      <|> parseAsAdHocJSONObject value
   where
    parseAsBase16CBOR =
      withText "Tx" $ \t ->
        case Base16.decode $ encodeUtf8 t of
          Left base16Error ->
            fail $ show base16Error
          Right bytes ->
            case decodeAnnotator "ValidatedTx" fromCBOR (fromStrict bytes) of
              Left cborError -> fail $ show cborError
              Right tx -> pure tx

    parseAsEnvelopedBase16CBOR =
      withObject "Tx" $ \o -> do
        let TextEnvelopeType envelopeType = textEnvelopeType (proxyToAsType (Proxy @Tx))
        str <- o .: "cborHex"
        guard . (== envelopeType) =<< (o .: "type")
        parseAsBase16CBOR (String str)

    parseAsAdHocJSONObject =
      withObject "Tx" $ \o -> do
        Ledger.Babbage.ValidatedTx
          <$> o .: "body"
          <*> o .: "witnesses"
          <*> o .:? "isValid" .!= Ledger.Babbage.IsValid True
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

instance FromJSON (Ledger.Mary.Value StandardCrypto) where
  parseJSON = fmap toLedgerValue . parseJSON

--
-- Wdrl
--

instance Crypto crypto => ToJSON (Ledger.Wdrl crypto) where
  toJSON = toJSON . Map.mapKeys rewardAcntToText . Ledger.unWdrl

instance Crypto crypto => FromJSON (Ledger.Wdrl crypto) where
  parseJSON json = do
    m <- Map.foldMapWithKey fn <$> parseJSON json
    maybe (fail "failed to parse withdrawal map.") (pure . Ledger.Wdrl) m
   where
    fn k v = Map.singleton <$> rewardAcntFromText k <*> v

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

--
-- Helpers
--

onlyIf :: ToJSON a => (a -> Bool) -> Aeson.Key -> a -> [Pair]
onlyIf predicate k v =
  [(k, toJSON v) | predicate v]

isOpenInterval :: Ledger.Mary.ValidityInterval -> Bool
isOpenInterval = \case
  Ledger.Mary.ValidityInterval SNothing SNothing -> True
  _ -> False
