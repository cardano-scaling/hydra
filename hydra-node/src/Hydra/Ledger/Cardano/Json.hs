{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans ToJSON/FromJSON instances on ledger types used by
-- Hydra.Ledger.Cardano to have JSON representations for various types.
--
-- XXX: The ledger team notified that we should be using lenses going forward.
module Hydra.Ledger.Cardano.Json where

import Hydra.Cardano.Api hiding (Era)
import Hydra.Prelude

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxAuxData as Ledger
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Ledger
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
import Cardano.Ledger.Binary (
  DecCBOR,
  decCBOR,
  decodeAnnotator,
  decodeFull',
  decodeFullAnnotator,
  decodeListLenOf,
  decodeWord,
  encodeListLen,
  encodeWord,
  serialize',
 )
import Cardano.Ledger.Binary.Decoding (Annotator)
import Cardano.Ledger.Block (txid)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import Cardano.Ledger.Serialization (Sized, mkSized)
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.TxCert as Ledger
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

instance
  ( Typeable era
  , ToCBOR (Ledger.AlonzoTxAuxData era)
  ) =>
  ToJSON (Ledger.AlonzoTxAuxData era)
  where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance
  ( Era era
  , FromCBOR (Annotator (Ledger.AlonzoTxAuxData era))
  , Core.Script era ~ Ledger.AlonzoScript era
  ) =>
  FromJSON (Ledger.AlonzoTxAuxData era)
  where
  parseJSON = withText "AuxiliaryData" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "AuxiliaryData" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

instance Crypto crypto => FromJSON (Ledger.AuxiliaryDataHash crypto) where
  parseJSON = fmap Ledger.AuxiliaryDataHash . parseJSON

--
-- Bootstrap Witness
--

instance Crypto crypto => ToJSON (Ledger.BootstrapWitness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (Ledger.BootstrapWitness crypto) where
  parseJSON = parseHexEncodedCbor "BootstrapWitness"

--
-- DCert
--
-- TODO: Delegation certificates can actually be represented as plain JSON
-- objects (it's a sum type), so we may want to revisit this interface later?

instance ToJSON (Ledger.ShelleyTxCert era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (Ledger.ShelleyTxCert era) where
  parseJSON = withText "TxCert" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeFull' bs' of
        Left err -> fail $ show err
        Right v -> pure v

--
-- IsValid
--

instance ToJSON Ledger.IsValid where
  toJSON (Ledger.IsValid b) = toJSON b

instance FromJSON Ledger.IsValid where
  parseJSON = fmap Ledger.IsValid . parseJSON

--
-- Redeemers
--
-- TODO: Provide maybe better instances for redeemers from which we can actually
-- view them as a map from pointers to data?

instance FromJSON (Ledger.Redeemers era) where
  parseJSON = withText "Redeemers" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeAnnotator "Redeemers" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

instance ToCBOR (Ledger.Redeemers era) => ToJSON (Ledger.Redeemers era) where
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
  ( Crypto (Ledger.EraCrypto era)
  , Ledger.Era era
  ) =>
  FromJSON (Ledger.AlonzoScript era)
  where
  parseJSON = withText "Script" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ "failed to decode from base16: " <> show err
      Right bs' -> case decodeAnnotator "Script" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- Timelock
--

instance ToJSON (Ledger.Timelock StandardCrypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (Ledger.Timelock StandardCrypto) where
  parseJSON = withText "Timelock" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "Timelock" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- TxBody
--

instance ToJSON (Ledger.BabbageTxBody LedgerEra) where
  toJSON b =
    object $
      mconcat
        [ onlyIf (const True) "inputs" (Set.map fromLedgerTxIn (Ledger.spendInputs' b))
        , onlyIf (not . null) "collateral" (Set.map fromLedgerTxIn (Ledger.collateralInputs' b))
        , onlyIf (not . null) "referenceInputs" (Set.map fromLedgerTxIn (Ledger.referenceInputs' b))
        , onlyIf (const True) "outputs" (fromLedgerTxOut <$> Ledger.outputs' b)
        , onlyIf isSJust "collateralReturn" (fromLedgerTxOut <$> Ledger.collateralReturn' b)
        , onlyIf isSJust "totalCollateral" (Ledger.totalCollateral' b)
        , onlyIf (not . null) "certificates" (Ledger.certs' b)
        , onlyIf (not . null . Ledger.unWithdrawals) "withdrawals" (Ledger.withdrawals' b)
        , onlyIf (const True) "fees" (Ledger.txfee' b)
        , onlyIf (not . isOpenInterval) "validity" (Ledger.vldt' b)
        , onlyIf (not . null) "requiredSignatures" (Ledger.reqSignerHashes' b)
        , onlyIf (/= mempty) "mint" (fromLedgerValue $ Ledger.MaryValue 0 (Ledger.mint' b))
        , onlyIf isSJust "scriptIntegrityHash" (Ledger.scriptIntegrityHash' b)
        , onlyIf isSJust "auxiliaryDataHash" (Ledger.adHash' b)
        , onlyIf isSJust "networkId" (Ledger.txnetworkid' b)
        ]

instance (ToCBOR a, FromJSON a) => FromJSON (Sized a) where
  parseJSON =
    fmap mkSized . parseJSON

instance
  ( Ledger.BabbageEraTxBody era
  , FromJSON (Core.Value era)
  , FromJSON (Ledger.MaryValue (Ledger.EraCrypto era))
  , FromJSON (Ledger.AuxiliaryData era)
  , FromJSON (Ledger.TxIn (Ledger.EraCrypto era))
  , FromJSON (Ledger.BabbageTxOut era)
  ) =>
  FromJSON (Ledger.BabbageTxBody era)
  where
  parseJSON = withObject "TxBody" $ \o -> do
    Ledger.BabbageTxBody
      <$> (o .: "inputs")
      <*> (o .:? "collateral" .!= mempty)
      <*> (o .:? "referenceInputs" .!= mempty)
      <*> (o .: "outputs")
      <*> (o .:? "collateralReturn" .!= SNothing)
      <*> (o .:? "totalCollateral" .!= SNothing)
      <*> (o .:? "certificates" .!= mempty)
      <*> (o .:? "withdrawals" .!= Ledger.Withdrawals mempty)
      <*> (o .:? "fees" .!= mempty)
      <*> (o .:? "validity" .!= Ledger.ValidityInterval SNothing SNothing)
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
  , Crypto (Ledger.EraCrypto era)
  ) =>
  ToJSON (Ledger.TxDats era)
  where
  toJSON (Ledger.TxDats datums) = toJSON datums

instance
  ( Crypto (Ledger.EraCrypto era)
  , Ledger.Era era
  ) =>
  FromJSON (Ledger.TxDats era)
  where
  parseJSON = fmap Ledger.TxDats . parseJSON

instance Typeable era => ToJSON (Ledger.Data era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance Ledger.Era era => FromJSON (Ledger.Data era) where
  parseJSON = withText "Data" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs' -> case decodeAnnotator "Data" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- TxIn
--

instance FromJSON (Ledger.TxIn StandardCrypto) where
  parseJSON = fmap toLedgerTxIn . parseJSON

--
-- TxOut
--

instance FromJSON (Ledger.BabbageTxOut LedgerEra) where
  parseJSON = fmap toLedgerTxOut . parseJSON

--
-- TxWitness
--

instance
  ( ToJSON (Core.Script era)
  , Core.Script era ~ Ledger.AlonzoScript era
  , Era era
  ) =>
  ToJSON (Ledger.AlonzoTxWits era)
  where
  toJSON (Ledger.AlonzoTxWits vkeys boots scripts datums redeemers) =
    object $
      mconcat
        [ onlyIf (not . null) "keys" vkeys
        , onlyIf (not . null) "bootstrap" boots
        , onlyIf (not . null) "scripts" scripts
        , onlyIf (not . Ledger.nullDats) "datums" datums
        , onlyIf (not . Ledger.nullRedeemers) "redeemers" redeemers
        ]

instance
  ( FromJSON (Core.Script era)
  , Core.Script era ~ Ledger.AlonzoScript era
  , Era era
  ) =>
  FromJSON (Ledger.AlonzoTxWits era)
  where
  parseJSON = withObject "TxWitness" $ \o ->
    Ledger.AlonzoTxWits
      <$> (o .:? "keys" .!= mempty)
      <*> (o .:? "bootstrap" .!= mempty)
      <*> (o .:? "scripts" .!= mempty)
      <*> (o .:? "datums" .!= Ledger.TxDats mempty)
      <*> (o .:? "redeemers" .!= Ledger.Redeemers mempty)

--
-- ValidatedTx
--

instance
  ( ToJSON (Ledger.AlonzoTxWits era)
  , ToJSON (Core.TxBody era)
  , ToJSON (Ledger.AuxiliaryData era)
  , ToJSON (Core.Script era)
  , Core.Script era ~ Ledger.AlonzoScript era
  , Core.EraTxBody era
  , Core.Era era
  ) =>
  ToJSON (Ledger.AlonzoTx era)
  where
  toJSON (Ledger.AlonzoTx body witnesses isValid auxiliaryData) =
    object $
      mconcat
        [ ["id" .= txid body]
        , ["body" .= body]
        , ["witnesses" .= witnesses]
        , ["isValid" .= isValid]
        , onlyIf isSJust "auxiliaryData" auxiliaryData
        ]

instance
  ( FromJSON (Ledger.TxBody era)
  , FromJSON (Ledger.AuxiliaryData era)
  , FromJSON (Core.Script era)
  , FromCBOR (Annotator (Core.TxBody era))
  , FromCBOR (Annotator (Ledger.AuxiliaryData era))
  , Era era
  , Core.Script era ~ Ledger.AlonzoScript era
  , Core.EraScript era
  ) =>
  FromJSON (Ledger.AlonzoTx era)
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
        Ledger.AlonzoTx
          <$> o
          .: "body"
          <*> o
          .: "witnesses"
          <*> o
          .:? "isValid"
          .!= Ledger.IsValid True
          <*> o
          .:? "auxiliaryData"
          .!= SNothing

--
-- ValidityInterval
--

instance ToJSON Ledger.ValidityInterval where
  toJSON (Ledger.ValidityInterval notBefore notAfter) =
    object
      [ "notBefore" .= notBefore
      , "notAfter" .= notAfter
      ]

instance FromJSON Ledger.ValidityInterval where
  parseJSON = withObject "ValidityInterval" $ \obj ->
    Ledger.ValidityInterval
      <$> obj
      .: "notBefore"
      <*> obj
      .: "notAfter"

--
-- Value
--

instance FromJSON (Ledger.MaryValue StandardCrypto) where
  parseJSON = fmap toLedgerValue . parseJSON

--
-- Wdrl
--

instance Crypto crypto => ToJSON (Ledger.Withdrawals crypto) where
  toJSON = toJSON . Map.mapKeys rewardAcntToText . Ledger.unWithdrawals

instance Crypto crypto => FromJSON (Ledger.Withdrawals crypto) where
  parseJSON json = do
    m <- Map.foldMapWithKey fn <$> parseJSON json
    maybe (fail "failed to parse withdrawal map.") (pure . Ledger.Withdrawals) m
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

isOpenInterval :: Ledger.ValidityInterval -> Bool
isOpenInterval = \case
  Ledger.ValidityInterval SNothing SNothing -> True
  _ -> False

-- | Parse a hex-encoded CBOR value in given 'era'.
parseHexEncodedCbor ::
  forall era a.
  (Ledger.Era era, DecCBOR a) =>
  Text ->
  Aeson.Value ->
  Parser a
parseHexEncodedCbor lbl =
  withText (toString lbl) $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs ->
        case decodeFullDecoder version lbl decCBOR (fromStrict bs) of
          Left err -> fail $ show err
          Right v -> pure v
 where
  version = Ledger.eraProtVerLow @era

-- | Parse a hex-encoded, annotated CBOR value in given 'era'.
parseHexEncodedCborAnnotated ::
  forall era a.
  (Ledger.Era era, DecCBOR (Annotator a)) =>
  Text ->
  Aeson.Value ->
  Parser a
parseHexEncodedCborAnnotated lbl =
  withText (toString lbl) $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left e -> fail $ "failed to decode from base16: " <> show e
      Right bs ->
        case decodeFullAnnotator version lbl decCBOR (fromStrict bs) of
          Left err -> fail $ show err
          Right v -> pure v
 where
  version = Ledger.eraProtVerLow @era
