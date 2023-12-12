{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans ToJSON/FromJSON instances on ledger types used by
-- Hydra.Ledger.Cardano to have JSON representations for various types.
--
-- XXX: The ledger team notified that we should be using lenses going forward.
module Hydra.Ledger.Cardano.Json where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Allegra.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxAuxData qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (outputsTxBodyL)
import Cardano.Ledger.Babbage.Tx qualified as Ledger
import Cardano.Ledger.Babbage.TxBody qualified as Ledger
import Cardano.Ledger.BaseTypes (StrictMaybe (..), isSJust)
import Cardano.Ledger.Binary (
  DecCBOR,
  EncCBOR,
  Sized,
  decCBOR,
  decodeFullAnnotator,
  decodeFullDecoder,
  mkSized,
  serialize',
 )
import Cardano.Ledger.Binary.Decoding (Annotator)
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Core (eraProtVerLow)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Ledger
import Cardano.Ledger.SafeHash qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Shelley.TxCert qualified as Ledger
import Codec.Binary.Bech32 qualified as Bech32
import Control.Lens ((^.))
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
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (
  Pair,
  Parser,
  toJSONKeyText,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.Map qualified as Map
import Data.Set qualified as Set

--
-- Addr
--
-- NOTE: ToJSON instance defined in cardano-ledger-specs
-- NOTE: Not defining 'FromJSON' because of conflicts with cardano-ledger-specs

decodeAddress ::
  Ledger.Crypto crypto =>
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

instance Ledger.Era era => ToJSON (Ledger.AlonzoTxAuxData era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @era)

instance Ledger.Era era => FromJSON (Ledger.AlonzoTxAuxData era) where
  parseJSON = parseHexEncodedCborAnnotated @era "AlonzoTxAuxData"

instance Ledger.Crypto crypto => FromJSON (Ledger.AuxiliaryDataHash crypto) where
  parseJSON = fmap Ledger.AuxiliaryDataHash . parseJSON

--
-- Bootstrap Witness
--

instance Ledger.Crypto crypto => ToJSON (Ledger.BootstrapWitness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @LedgerEra)

instance Ledger.Crypto crypto => FromJSON (Ledger.BootstrapWitness crypto) where
  parseJSON = parseHexEncodedCborAnnotated @LedgerEra "BootstrapWitness"

--
-- DCert
--
-- TODO: Delegation certificates can actually be represented as plain JSON
-- objects (it's a sum type), so we may want to revisit this interface later?

instance Ledger.Era era => ToJSON (Ledger.ShelleyTxCert era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @era)

instance
  ( Ledger.ShelleyEraTxCert era
  , Ledger.TxCert era ~ Ledger.ShelleyTxCert era
  ) =>
  FromJSON (Ledger.ShelleyTxCert era)
  where
  parseJSON = parseHexEncodedCbor @era "TxCert"

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

instance Ledger.Era era => FromJSON (Ledger.Redeemers era) where
  parseJSON = parseHexEncodedCborAnnotated @era "Redeemers"

instance Ledger.Era era => ToJSON (Ledger.Redeemers era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @era)

--
-- RewardAcnt
--

-- NOTE: The Ledge derive generic ToJSONKey from 'RewardAcnt', which by default
-- turn them into an array of elements.
rewardAcntToText :: Ledger.RewardAcnt crypto -> Text
rewardAcntToText = decodeUtf8 . Base16.encode . Ledger.serialiseRewardAcnt

rewardAcntFromText :: Ledger.Crypto crypto => Text -> Maybe (Ledger.RewardAcnt crypto)
rewardAcntFromText t = do
  case Base16.decode (encodeUtf8 t) of
    Left{} -> Nothing
    Right bs -> Ledger.deserialiseRewardAcnt bs

--
-- SafeHash
--

instance Ledger.Crypto crypto => ToJSONKey (Ledger.SafeHash crypto any) where
  toJSONKey = toJSONKeyText safeHashToText

safeHashToText ::
  Ledger.SafeHash crypto any ->
  Text
safeHashToText =
  decodeUtf8 . Base16.encode . Crypto.hashToBytes . Ledger.extractHash

instance Ledger.Crypto crypto => FromJSONKey (Ledger.SafeHash crypto any) where
  fromJSONKey = FromJSONKeyTextParser safeHashFromText

safeHashFromText ::
  (Ledger.Crypto crypto, MonadFail m) =>
  Text ->
  m (Ledger.SafeHash crypto any)
safeHashFromText t =
  case Crypto.hashFromTextAsHex t of
    Nothing -> fail "failed to decode from base16."
    Just h -> pure $ Ledger.unsafeMakeSafeHash h

--
-- Script
--

instance Ledger.Era era => FromJSON (Ledger.AlonzoScript era) where
  parseJSON = parseHexEncodedCborAnnotated @era "Script"

--
-- Timelock
--

instance Ledger.Era era => ToJSON (Ledger.Timelock era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @era)

instance Ledger.Era era => FromJSON (Ledger.Timelock era) where
  parseJSON = parseHexEncodedCborAnnotated @era "Timelock"

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
        , onlyIf (const True) "outputs" (fromLedgerTxOut <$> b ^. outputsTxBodyL)
        , onlyIf isSJust "collateralReturn" (fromLedgerTxOut <$> Ledger.collateralReturn' b)
        , onlyIf isSJust "totalCollateral" (Ledger.totalCollateral' b)
        , onlyIf (not . null) "certificates" (Ledger.certs' b)
        , onlyIf (not . null . Ledger.unWithdrawals) "withdrawals" (Ledger.withdrawals' b)
        , onlyIf (const True) "fees" (Ledger.txfee' b)
        , onlyIf (not . isOpenInterval) "validity" (Ledger.vldt' b)
        , onlyIf (not . null) "requiredSignatures" (Ledger.reqSignerHashes' b)
        , onlyIf (/= mempty) "mint" (fromLedgerMultiAsset (Ledger.mint' b))
        , onlyIf isSJust "scriptIntegrityHash" (Ledger.scriptIntegrityHash' b)
        , onlyIf isSJust "auxiliaryDataHash" (Ledger.adHash' b)
        , onlyIf isSJust "networkId" (Ledger.txnetworkid' b)
        ]

-- NOTE: The 'Sized' instance is always using the fixed 'LedgerEra' to determine
-- version and thus encoded size.
instance (EncCBOR a, FromJSON a) => FromJSON (Sized a) where
  parseJSON =
    fmap (mkSized $ eraProtVerLow @LedgerEra) . parseJSON

instance
  ( Ledger.BabbageEraTxBody era
  , FromJSON (Ledger.MaryValue (Ledger.EraCrypto era))
  , FromJSON (Ledger.TxAuxData era)
  , FromJSON (Ledger.TxOut era)
  , FromJSON (Ledger.TxCert era)
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
      <*> (valueToMultiAsset <$> o .:? "mint" .!= mempty)
      <*> (o .:? "scriptIntegrityHash" .!= SNothing)
      <*> (o .:? "auxiliaryDataHash" .!= SNothing)
      <*> (o .:? "networkId" .!= SNothing)
   where
    valueToMultiAsset (Ledger.MaryValue _ multiAsset) = multiAsset

--
-- TxDats
--

instance
  ( Ledger.Era era
  , Ledger.Crypto (Ledger.EraCrypto era)
  ) =>
  ToJSON (Ledger.TxDats era)
  where
  toJSON (Ledger.TxDats datums) = toJSON datums

instance
  ( Ledger.Crypto (Ledger.EraCrypto era)
  , Ledger.Era era
  ) =>
  FromJSON (Ledger.TxDats era)
  where
  parseJSON = fmap Ledger.TxDats . parseJSON

instance Ledger.Era era => ToJSON (Ledger.Data era) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @era)

instance Ledger.Era era => FromJSON (Ledger.Data era) where
  parseJSON = parseHexEncodedCborAnnotated @era "Data"

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
  ( ToJSON (Ledger.Script era)
  , Ledger.Script era ~ Ledger.AlonzoScript era
  , Ledger.Era era
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
  ( FromJSON (Ledger.Script era)
  , Ledger.Script era ~ Ledger.AlonzoScript era
  , Ledger.Era era
  ) =>
  FromJSON (Ledger.AlonzoTxWits era)
  where
  parseJSON = withObject "AlonzoTxWits" $ \o ->
    Ledger.AlonzoTxWits
      <$> (o .:? "keys" .!= mempty)
      <*> (o .:? "bootstrap" .!= mempty)
      <*> (o .:? "scripts" .!= mempty)
      <*> (o .:? "datums" .!= Ledger.TxDats mempty)
      <*> (o .:? "redeemers" .!= Ledger.Redeemers mempty)

--
-- AlonzoTx
--

instance
  ( ToJSON (Ledger.TxBody era)
  , ToJSON (Ledger.TxAuxData era)
  , ToJSON (Ledger.TxWits era)
  , Ledger.EraTxBody era
  , Ledger.Era era
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
  , FromJSON (Ledger.TxWits era)
  , FromJSON (Ledger.TxAuxData era)
  , DecCBOR (Annotator (Ledger.TxBody era))
  , DecCBOR (Annotator (Ledger.TxWits era))
  , DecCBOR (Annotator (Ledger.TxAuxData era))
  , Ledger.Era era
  ) =>
  FromJSON (Ledger.AlonzoTx era)
  where
  parseJSON value =
    -- We accept transactions in three forms:
    --
    -- (1) As a JSON 'text-envelope', which is a format defined and produced by
    -- the cardano-cli, wrapping base16-encoded strings as JSON objects with
    -- tags.
    parseAsEnvelopedBase16CBOR value
      -- (2) As base16 string representing a CBOR-serialized transaction, since
      -- this is the most common medium of exchange used for transactions.
      <|> parseHexEncodedCborAnnotated @era "Tx" value
      -- (3) As high-level JSON object, which full format is specified via a
      -- JSON-schema.
      <|> parseAsAdHocJSONObject value
   where
    parseAsEnvelopedBase16CBOR =
      withObject "Tx" $ \o -> do
        let TextEnvelopeType envelopeType = textEnvelopeType (proxyToAsType (Proxy @Tx))
        str <- o .: "cborHex"
        guard . (== envelopeType) =<< (o .: "type")
        parseHexEncodedCborAnnotated @era "Tx" (String str)

    parseAsAdHocJSONObject =
      withObject "Tx" $ \o -> do
        Ledger.AlonzoTx
          <$> (o .: "body")
          <*> (o .: "witnesses")
          <*> (o .:? "isValid" .!= Ledger.IsValid True)
          <*> (o .:? "auxiliaryData" .!= SNothing)

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
      <$> (obj .: "notBefore")
      <*> (obj .: "notAfter")

--
-- Value
--
-- REVIEW: Check if this is really roundtripping with toJSON
instance FromJSON (Ledger.MaryValue StandardCrypto) where
  parseJSON = fmap toLedgerValue . parseJSON

--
-- Wdrl
--

instance Ledger.Crypto crypto => ToJSON (Ledger.Withdrawals crypto) where
  toJSON = toJSON . Map.mapKeys rewardAcntToText . Ledger.unWithdrawals

instance Ledger.Crypto crypto => FromJSON (Ledger.Withdrawals crypto) where
  parseJSON json = do
    m <- Map.foldMapWithKey fn <$> parseJSON json
    maybe (fail "failed to parse withdrawal map.") (pure . Ledger.Withdrawals) m
   where
    fn k v = Map.singleton <$> rewardAcntFromText k <*> v

--
-- WitVKey
--

instance Ledger.Crypto crypto => ToJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize' (eraProtVerLow @LedgerEra)

instance Ledger.Crypto crypto => FromJSON (Ledger.WitVKey 'Ledger.Witness crypto) where
  parseJSON = parseHexEncodedCborAnnotated @LedgerEra "WitVKey"

-- * Helpers

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
