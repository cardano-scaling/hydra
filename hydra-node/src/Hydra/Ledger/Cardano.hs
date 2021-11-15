{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano where

import Hydra.Prelude hiding (id)

import Cardano.Binary (
  decodeAnnotator,
  decodeBytes,
  decodeFull,
  decodeListLenOf,
  decodeWord,
  encodeBytes,
  encodeListLen,
  encodeWord,
  serialize',
  serializeEncoding',
 )
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (..),
  VerKeyDSIGN (VerKeyMockDSIGN),
  deriveVerKeyDSIGN,
 )
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Cardano
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (AuxiliaryDataHash), unsafeAuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SNothing), boundRational, mkActiveSlotCoeff)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys (KeyPair (vKey), asWitness, signedDSIGN)
import Cardano.Ledger.Mary (AuxiliaryData, MaryEra)
import qualified Cardano.Ledger.Mary.Value as Cardano
import Cardano.Ledger.SafeHash (extractHash)
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (Wdrl (Wdrl), unWdrl, _maxTxSize)
import qualified Cardano.Ledger.Shelley.API as Cardano hiding (TxBody)
import Cardano.Ledger.Shelley.Tx (WitnessSetHKD (WitnessSet))
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Cardano
import qualified Cardano.Ledger.ShelleyMA.TxBody as Cardano
import Cardano.Ledger.Slot (EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Ledger.Val (invert)
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
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
  (.=),
 )
import Data.Aeson.Types (mapFromJSONKeyFunction, toJSONKeyText)
import qualified Data.ByteString.Base16 as Base16
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Records (getField)
import Hydra.Ledger (Balance (..), Ledger (..), Tx (..), ValidationError (ValidationError))
import Hydra.Party (Party (Party), vkey)
import Test.Cardano.Ledger.MaryEraGen ()
import qualified Test.Cardano.Ledger.Shelley.Generator.Constants as Constants
import Test.Cardano.Ledger.Shelley.Generator.Core (geConstants)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genUtxo0)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)
import Test.QuickCheck (Gen, choose, getSize, scale, suchThat, vectorOf)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (mkQCGen)

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
      utxo' <- applyTx ledgerEnv utxo (convertTx tx)
      applyAll utxo' txs

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
  type AssetId CardanoTx = (Cardano.PolicyID StandardCrypto, Cardano.AssetName)

  txId CardanoTx{body} = Cardano.TxId $ SafeHash.hashAnnotated body
  balance (Cardano.UTxO u) =
    let aggregate (Cardano.TxOut _ value) = (<>) value
        valueToBalance (Cardano.Value (fromIntegral -> lovelace) assetsByPolicy) =
          Balance
            { lovelace
            , assets =
                Map.foldrWithKey
                  (\k v -> Map.union (Map.mapKeys (k,) v))
                  mempty
                  assetsByPolicy
            }
     in valueToBalance $ Map.foldr aggregate mempty u

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

genKeyPair :: Gen (Cardano.KeyPair 'Cardano.Payment StandardCrypto)
genKeyPair = do
  -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
  -- play well with pure generation from seed.
  sk <- fromJust . rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 64 arbitrary
  pure $ Cardano.KeyPair (Cardano.VKey (deriveVerKeyDSIGN sk)) sk

-- Construct a simple transaction which spends a UTXO, to the given address,
-- signed by the given key.
mkSimpleCardanoTx ::
  (TxIn, TxOut) ->
  (CardanoAddress, Cardano.Value StandardCrypto) ->
  CardanoKeyPair ->
  CardanoTx
mkSimpleCardanoTx (i, Cardano.TxOut owner valueIn) (recipient, valueOut) credentials =
  CardanoTx{id, body, witnesses, auxiliaryData}
 where
  id = Cardano.TxId $ SafeHash.hashAnnotated body

  body =
    Cardano.TxBody
      (Set.singleton i)
      ( StrictSeq.fromList $
          Cardano.TxOut recipient valueOut :
            [ Cardano.TxOut owner (valueIn <> invert valueOut)
            | valueOut /= valueIn
            ]
      )
      mempty
      (Cardano.Wdrl mempty)
      fees
      (Cardano.ValidityInterval Cardano.SNothing Cardano.SNothing)
      Cardano.SNothing
      Cardano.SNothing
      mempty
   where
    fees = Cardano.Coin 0

  witnesses =
    WitnessSet addrWits scriptWits bootWits
   where
    addrWits = Set.singleton (id `signWith` credentials)
    scriptWits = mempty
    bootWits = Set.empty

  auxiliaryData =
    Cardano.SNothing

genCardanoTx :: Utxo CardanoTx -> Gen CardanoTx
genCardanoTx utxos = do
  tx <- genTx generatorEnv ledgerEnv (utxoState, dpState)
  case tx of
    (Cardano.Tx body wits aux) ->
      pure $
        CardanoTx
          (Cardano.TxId $ SafeHash.hashAnnotated body)
          body
          wits
          aux
 where
  noPPUpdatesTransactions =
    Constants.defaultConstants
      { Constants.frequencyTxUpdates = 0
      , Constants.maxCertsPerTx = 0
      }

  utxoState = def{Cardano._utxo = utxos}

  dpState = Cardano.DPState def def

  -- NOTE(AB): This sets some parameters for the tx generator that will
  -- affect the structure of generated trasactions. In our case, we want
  -- to remove "special" capabilities which are irrelevant in the context
  -- of a Hydra head
  -- see https://github.com/input-output-hk/cardano-ledger-specs/blob/nil/shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Generator/Constants.hs#L10
  generatorEnv = (genEnv Proxy){geConstants = noPPUpdatesTransactions}

genSequenceOfValidTransactions :: Utxo CardanoTx -> Gen [CardanoTx]
genSequenceOfValidTransactions initialUtxo
  | initialUtxo == mempty = pure []
  | otherwise = do
    n <- getSize
    numTxs <- choose (1, n)
    genFixedSizeSequenceOfValidTransactions numTxs initialUtxo

genFixedSizeSequenceOfValidTransactions :: Int -> Utxo CardanoTx -> Gen [CardanoTx]
genFixedSizeSequenceOfValidTransactions numTxs initialUtxo
  | initialUtxo == mempty = pure []
  | otherwise = do
    reverse . snd <$> foldM newTx (initialUtxo, []) [1 .. numTxs]
 where
  newTx (utxos, acc) _ = do
    tx <- genCardanoTx utxos
    case applyTransactions cardanoLedger utxos [tx] of
      Left err -> error $ show err
      Right newUtxos -> pure (newUtxos, tx : acc)

type CardanoEra = MaryEra StandardCrypto

type CardanoTxBody = Cardano.TxBody CardanoEra

type CardanoTxWitnesses = Cardano.WitnessSet CardanoEra

type CardanoAuxiliaryData = StrictMaybe (AuxiliaryData CardanoEra)

type CardanoAddress = Cardano.Addr StandardCrypto

type CardanoKeyPair = Cardano.KeyPair 'Cardano.Payment StandardCrypto

type TxIn = Cardano.TxIn StandardCrypto

type TxOut = Cardano.TxOut CardanoEra

signWith ::
  Cardano.TxId StandardCrypto ->
  Cardano.KeyPair 'Cardano.Payment StandardCrypto ->
  Cardano.WitVKey 'Cardano.Witness StandardCrypto
signWith (Cardano.TxId safeHash) credentials =
  Cardano.WitVKey
    (asWitness $ Cardano.vKey credentials)
    (signedDSIGN @StandardCrypto (Cardano.sKey credentials) (extractHash safeHash))

--
-- Balance
--

prettyBalance :: Balance tx -> Text
prettyBalance Balance{lovelace, assets} =
  let (ada, decimal) = lovelace `quotRem` 1000000
      padLeft c n str = T.takeEnd n (T.replicate n c <> str)
   in unwords $
        [ show ada <> "." <> padLeft "0" 6 (show decimal)
        , "₳"
        ]
          ++ if null assets
            then mempty
            else
              [ "and"
              , show (Map.size assets)
              , "asset(s)"
              ]

--
--  Transaction Id
--

instance Crypto crypto => ToJSON (Cardano.TxId crypto) where
  toJSON = String . txIdToText @crypto

instance Crypto crypto => FromJSON (Cardano.TxId crypto) where
  parseJSON = withText "base16 encoded TxId" txIdFromText

txIdToText :: Cardano.TxId crypto -> Text
txIdToText (Cardano.TxId h) =
  decodeUtf8 . Base16.encode $ Crypto.hashToBytes $ SafeHash.extractHash h

txIdFromText :: (Crypto crypto, MonadFail m) => Text -> m (Cardano.TxId crypto)
txIdFromText t =
  case Base16.decode (encodeUtf8 t) of
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
    certificates <- o .:? "certificates" .!= mempty
    withdrawals <- o .:? "withdrawals" .!= noWithdrawals
    fees <- o .:? "fees" .!= mempty
    validity <- o .:? "validity" .!= Cardano.ValidityInterval SNothing SNothing
    auxiliaryDataHash <- o .:? "auxiliaryDataHash" .!= SNothing
    mint <- o .:? "mint" .!= mempty
    pure $
      Cardano.TxBody
        (Set.fromList inputs)
        (StrictSeq.fromList outputs)
        certificates
        withdrawals
        fees
        validity
        Cardano.SNothing
        auxiliaryDataHash
        mint
   where
    noWithdrawals = Wdrl mempty

instance ToJSON CardanoTxBody where
  toJSON (Cardano.TxBody inputs outputs certificates withdrawals fees validity _update auxiliaryDataHash mint) =
    object
      [ "inputs" .= inputs
      , "outputs" .= outputs
      , "certificates" .= certificates
      , "withdrawals" .= withdrawals
      , "fees" .= fees
      , "validity" .= validity
      , "auxiliaryDataHash" .= auxiliaryDataHash
      , "mint" .= mint
      ]

instance Crypto crypto => ToJSON (Cardano.Wdrl crypto) where
  toJSON = toJSON . unWdrl

instance Crypto crypto => FromJSON (Cardano.Wdrl crypto) where
  parseJSON v = Cardano.Wdrl <$> parseJSON v

instance ToJSON Cardano.ValidityInterval where
  toJSON (Cardano.ValidityInterval notBefore notAfter) =
    object
      [ "notBefore" .= notBefore
      , "notAfter" .= notAfter
      ]

instance FromJSON Cardano.ValidityInterval where
  parseJSON = withObject "ValidityInterval" $ \obj ->
    Cardano.ValidityInterval <$> obj .: "notBefore" <*> obj .: "notAfter"

instance ToJSON (AuxiliaryDataHash crypto) where
  toJSON = String . decodeUtf8 . Base16.encode . Crypto.hashToBytes . SafeHash.extractHash . unsafeAuxiliaryDataHash

instance Crypto crypto => FromJSON (AuxiliaryDataHash crypto) where
  parseJSON = withText "AuxiliaryDataHash" $ \t ->
    case Base16.decode (encodeUtf8 t) of
      Left e -> fail $ "decoding base16: " <> show e
      Right bytes ->
        case Crypto.hashFromBytes bytes of
          Nothing -> fail "hashFromBytes yielded Nothing"
          Just h -> pure . AuxiliaryDataHash $ SafeHash.unsafeMakeSafeHash h

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
-- Address
--

mkVkAddress ::
  Cardano.VKey 'Cardano.Payment StandardCrypto ->
  CardanoAddress
mkVkAddress vk =
  let paymentCredential = Cardano.KeyHashObj $ Cardano.hashKey vk
      stakeReference = Cardano.StakeRefNull
   in Cardano.Addr Cardano.Testnet paymentCredential stakeReference

-- Serialise addresses in bech32 including the prefix as standardized:
-- https://github.com/cardano-foundation/CIPs/blob/master/CIP-0005/CIP-0005.md
encodeAddress ::
  Cardano.Addr crypto ->
  Text
encodeAddress addr =
  Bech32.encodeLenient prefix . Bech32.dataPartFromBytes $ Cardano.serialiseAddr addr
 where
  -- REVIEW(SN): The ledger's 'Addr' type is bigger than we need here and we
  -- are forced to come up with a prefix for Byron "bootstrap" addresses,
  -- although they should actually be serialised differently..and would not be
  -- relevant for Hydra in the first place.
  prefix = case addr of
    (Cardano.Addr Cardano.Mainnet _ _) -> [Bech32.humanReadablePart|addr|]
    (Cardano.Addr Cardano.Testnet _ _) -> [Bech32.humanReadablePart|addr_test|]
    (Cardano.AddrBootstrap _) -> [Bech32.humanReadablePart|addr_boot|]

--
-- Output
--

instance Crypto crypto => ToJSON (Cardano.TxOut (MaryEra crypto)) where
  toJSON (Cardano.TxOut addr value) =
    object
      [ "address" .= encodeAddress addr
      , "value" .= value
      ]

instance Crypto crypto => FromJSON (Cardano.TxOut (MaryEra crypto)) where
  parseJSON = withObject "TxOut" $ \o -> do
    address <- o .: "address" >>= deserialiseAddressBech32
    value <- o .: "value"
    pure $ Cardano.TxOut address value
   where
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

instance Crypto crypto => ToJSON (Cardano.Value crypto) where
  toJSON (Cardano.Value lovelace assets) =
    object $
      [ "lovelace" .= lovelace
      ]
        <> addedAssets
   where
    addedAssets = ["assets" .= assets | assets /= mempty]

instance Crypto crypto => FromJSON (Cardano.Value crypto) where
  parseJSON = withObject "Value" $ \o ->
    Cardano.Value <$> o .: "lovelace" <*> o .:? "assets" .!= mempty

instance Crypto crypto => ToJSON (Cardano.PolicyID crypto) where
  toJSON (Cardano.PolicyID h) = toJSON h

instance Crypto crypto => FromJSON (Cardano.PolicyID crypto) where
  parseJSON v = Cardano.PolicyID <$> parseJSON v

instance Crypto crypto => ToJSONKey (Cardano.PolicyID crypto) where
  toJSONKey = contramap (\(Cardano.PolicyID h) -> h) toJSONKey

instance Crypto crypto => FromJSONKey (Cardano.PolicyID crypto) where
  fromJSONKey = mapFromJSONKeyFunction Cardano.PolicyID fromJSONKey

instance ToJSON Cardano.AssetName where
  toJSON (Cardano.AssetName bytes) = String $ decodeUtf8 $ Base16.encode bytes

instance FromJSON Cardano.AssetName where
  parseJSON = withText "AssetName" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs -> pure $ Cardano.AssetName bs

instance ToJSONKey Cardano.AssetName where
  toJSONKey = toJSONKeyText $ \(Cardano.AssetName bytes) -> decodeUtf8 $ Base16.encode bytes

instance FromJSONKey Cardano.AssetName where
  fromJSONKey = FromJSONKeyTextParser nameFromText
   where
    nameFromText t =
      case Base16.decode (encodeUtf8 t) of
        Left e -> fail $ "decoding base16: " <> show e
        Right bytes -> pure $ Cardano.AssetName bytes

genOutput :: Cardano.VKey 'Cardano.Payment StandardCrypto -> Gen TxOut
genOutput vk =
  -- NOTE: Scaling a bit the generator to get non-trivial outputs with some
  -- funds, and not just a few lovelaces.
  Cardano.TxOut (mkVkAddress vk) <$> scale (* 8) arbitrary

---
--- Certificates
---

instance ToJSON (Cardano.DCert StandardCrypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (Cardano.DCert StandardCrypto) where
  parseJSON = withText "DCert" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeFull (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

--
-- Utxo
--

instance Semigroup (Cardano.UTxO (MaryEra crypto)) where
  Cardano.UTxO u1 <> Cardano.UTxO u2 = Cardano.UTxO (u1 <> u2)

instance Monoid (Cardano.UTxO (MaryEra crypto)) where
  mempty = Cardano.UTxO mempty

instance Crypto crypto =>  ToJSON (Cardano.UTxO (MaryEra crypto)) where
  toJSON = utxoToJSON

-- NOTE: This function exists because we encountered an overlapping instance
-- problem in the benchmark. We tried to use Cardano.Api but we don't want (yet)
-- to use Cardano.Api's JSON instances, because we only have access to ToJSON
-- instances and we would need to ensure consistency with ToJSON/FromJSON everywhere,
-- which is quite annoying.
utxoToJSON :: Crypto crypto => (Cardano.UTxO (MaryEra crypto)) -> Value
utxoToJSON = toJSON . Cardano.unUTxO

instance Crypto crypto => FromJSON (Cardano.UTxO (MaryEra crypto)) where
  parseJSON v = Cardano.UTxO <$> parseJSON v

prettyUtxo :: (TxIn, TxOut) -> Text
prettyUtxo (k, v) =
  let value = prettyBalance $ balance @CardanoTx $ Cardano.UTxO (Map.singleton k v)
   in T.drop 54 (txInToText k) <> " ↦ " <> value

genUtxo :: Gen (Utxo CardanoTx)
genUtxo = do
  genesisTxId <- arbitrary
  utxo <- genUtxo0 (genEnv Proxy)
  pure $ Cardano.UTxO $ Map.mapKeys (setTxId genesisTxId) $ Cardano.unUTxO utxo
 where
  setTxId :: Cardano.TxId StandardCrypto -> Cardano.TxIn StandardCrypto -> Cardano.TxIn StandardCrypto
  setTxId baseId (Cardano.TxIn _ti wo) = Cardano.TxIn baseId wo

-- | Generate utxos owned by the given cardano key.
genUtxoFor :: Cardano.VKey 'Cardano.Payment StandardCrypto -> Gen (Utxo CardanoTx)
genUtxoFor vk = do
  n <- arbitrary `suchThat` (> 0)
  inputs <- vectorOf n arbitrary
  outputs <- vectorOf n (genOutput vk)
  pure $ Cardano.UTxO $ Map.fromList $ zip inputs outputs

-- | Generate a single UTXO owned by 'vk'.
genOneUtxoFor :: Cardano.VKey 'Cardano.Payment StandardCrypto -> Gen (Utxo CardanoTx)
genOneUtxoFor vk = do
  input <- arbitrary
  -- NOTE(AB): calling this generator while running a property will yield larger and larger
  -- values (quikcheck increases the 'size' parameter upon success) up to the point they are
  -- too large to fit in a transaction and validation fails in the ledger
  output <- scale (const 1) $ genOutput vk
  pure $ Cardano.UTxO $ Map.singleton input output

utxoSize :: Utxo CardanoTx -> Int
utxoSize = length . Cardano.unUTxO

utxoToList :: Utxo CardanoTx -> [(TxIn, TxOut)]
utxoToList = Map.toList . Cardano.unUTxO

utxoValue :: Utxo CardanoTx -> Cardano.Value StandardCrypto
utxoValue = mconcat . map (getField @"value") . Map.elems . Cardano.unUTxO

utxoFromTx :: CardanoTx -> Utxo CardanoTx
utxoFromTx CardanoTx{id, body = (Cardano.TxBody _ outputs _ _ _ _ _ _ _)} =
  let txOuts = toList outputs
      txIns = map (Cardano.TxIn id) [0 .. fromIntegral (length txOuts)]
   in Cardano.UTxO $ Map.fromList $ zip txIns txOuts

--
-- Witnesses
--

instance ToJSON CardanoTxWitnesses where
  toJSON (WitnessSet addrWitnesses scriptWitnesses _) =
    object
      [ "keys" .= addrWitnesses
      , "scripts" .= scriptWitnesses
      ]

instance FromJSON CardanoTxWitnesses where
  parseJSON = withObject "CardanoTxWitnesses" $ \obj -> do
    WitnessSet
      <$> obj .: "keys"
      <*> obj .: "scripts"
      <*> pure mempty

instance ToJSON (Cardano.WitVKey 'Cardano.Witness StandardCrypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serializeEncoding' . prefixWithTag
   where
    prefixWithTag wit = encodeListLen 2 <> encodeWord 0 <> toCBOR wit

instance FromJSON (Cardano.WitVKey 'Cardano.Witness StandardCrypto) where
  parseJSON = withText "KeyWitness" $ \t ->
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

instance ToJSON (Cardano.Timelock StandardCrypto) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (Cardano.Timelock StandardCrypto) where
  parseJSON = withText "Timelock" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeAnnotator "Timelock" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

instance Crypto crypto => ToJSONKey (Cardano.ScriptHash crypto) where
  toJSONKey = toJSONKeyText (\(Cardano.ScriptHash h) -> decodeUtf8 $ Base16.encode (Crypto.hashToBytes h))

instance Crypto crypto => FromJSONKey (Cardano.ScriptHash crypto) where
  fromJSONKey = FromJSONKeyTextParser hashFromText
   where
    hashFromText t =
      case Base16.decode (encodeUtf8 t) of
        Left e -> fail $ "decoding base16: " <> show e
        Right bytes ->
          case Crypto.hashFromBytes bytes of
            Nothing -> fail "hashFromBytes yielded Nothing"
            Just h -> pure $ Cardano.ScriptHash h

--
-- AuxiliaryData
--

instance ToJSON (AuxiliaryData CardanoEra) where
  toJSON = String . decodeUtf8 . Base16.encode . serialize'

instance FromJSON (AuxiliaryData CardanoEra) where
  parseJSON = withText "Hex-encoded auxiliary data" $ \t ->
    case Base16.decode $ encodeUtf8 t of
      Left err -> fail $ show err
      Right bs' -> case decodeAnnotator "AuxiliaryData" fromCBOR (fromStrict bs') of
        Left err -> fail $ show err
        Right v -> pure v

-- * Calling the Cardano ledger

-- NOTE(SN): This is will fail on any transaction requiring the 'DPState' to be
-- in a certain state as we do throw away the resulting 'DPState' and only take
-- the ledger's 'UTxO' forward.
--
-- We came to this signature of only applying a single transaction because we
-- got confused why a sequence of transactions worked but sequentially applying
-- single transactions didn't. This was because of this not-keeping the'DPState'
-- as described above.
applyTx ::
  Cardano.LedgerEnv CardanoEra ->
  Cardano.UTxO CardanoEra ->
  Cardano.Tx CardanoEra ->
  Either ValidationError (Cardano.UTxO CardanoEra)
applyTx env utxo tx =
  case Cardano.applyTxsTransition globals env (pure tx) memPoolState of
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

-- * UTXO Faucet & Converting credentials

-- | For now, we _fake it until we make it_ ^TM. Credentials and initial UTXO are
-- generated *deterministically* from Hydra verification keys (the 'Party').
-- Thus, coupling Hydra keys (signing the Head itself) with Cardano keys
-- (signing transactions in a Head). In the end, the client will figure out
-- credentials and UTXO via some other means. Likely, the credentials will be
-- user-provided, whereas the UTXO would come from a local node + chain sync.

-- | Create a cardano key pair from a party. This would not be done in a real
-- application and we'd manage the Cardano keys separate from the Hydra keys.
-- For now though, this makes it easy to create assets for Head participants and
-- send values between "them".
getCredentials :: Party -> CardanoKeyPair
getCredentials Party{vkey} =
  let VerKeyMockDSIGN word = vkey
      seed = fromIntegral word
   in generateWith generateKeyPair seed
 where
  generateKeyPair = do
    -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
    -- play well with pure generation from seed.
    sk <- fromJust . rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 64 arbitrary
    pure $ Cardano.KeyPair (Cardano.VKey (deriveVerKeyDSIGN sk)) sk

-- | Similarly to 'getCredentials', this gives us "the" Cardano address given a
-- Hydra 'Party'. In a real world deployment it would make no sense to send a
-- Head participant something, the ledger would be fully decoupled.
getAddress :: Party -> CardanoAddress
getAddress =
  mkVkAddress . vKey . getCredentials

verificationKey :: Cardano.KeyPair r crypto -> Cardano.VKey r crypto
verificationKey = Cardano.vKey

-- | Generate a Utxo set for a given party "out of thin air".
faucetUtxo :: Party -> Map TxIn TxOut
faucetUtxo party@Party{vkey} =
  let VerKeyMockDSIGN word = vkey
      seed = fromIntegral word
      vk = vKey $ getCredentials party
      Cardano.UTxO u = generateWith (scale (const 5) $ genUtxoFor vk) seed
   in u

generateWith :: Gen a -> Int -> a
generateWith (MkGen runGen) seed =
  runGen (mkQCGen seed) 30
