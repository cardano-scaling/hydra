{-# LANGUAGE OverloadedRecordDot #-}
-- NOTE: Uses the TxBody/TxBodyContent API that cardano-api deprecated in favour of
-- Cardano.Api.Experimental; the migration is tracked separately in
-- https://github.com/cardano-scaling/hydra/issues/2896
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.UserValidationRules where

import Cardano.Api (AssetId (..), HashableScriptData, TxBodyContent (txMintValue), TxOutDatum (..), getOriginalScriptDataBytes, getTxBody, getTxBodyContent, txMintValueToValue)
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Control.Exception (IOException)
import Control.Exception qualified as E
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object, Parser, parseEither)
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import GHC.IsList (toList)
import Hydra.Cardano.Api (AddressInEra, PolicyId, Tx, TxId, TxOut, UTxO, toLedgerAddr, txOuts', pattern TxOut)
import Hydra.Prelude hiding (Key, toList)
import Hydra.Tx (IsTx (TxIdType, UTxOType))

-- | Loading and parsing of user policy
loadUserValidationRules :: FilePath -> IO UserValidationRules
loadUserValidationRules path = do
  result <- E.try (Yaml.decodeFileEither path)
  value <- case result of
    Left (err :: IOException) ->
      die $ "Failed to read policy file " <> path <> ": " <> show err
    Right (Left err) ->
      die $
        "Invalid YAML in policy file "
          <> path
          <> ":\n"
          <> Yaml.prettyPrintParseException err
    Right (Right decoded) -> pure decoded
  case parseEither parsePolicyFile value of
    Left err ->
      die $ "Invalid policy in " <> path <> ":\n" <> err
    Right rules -> pure rules

parsePolicyFile :: Aeson.Value -> Parser UserValidationRules
parsePolicyFile = Aeson.withObject "Policy file" $ \o -> do
  rejectUnknownKeys ["head-policy"] o
  policy <- o Aeson..: "head-policy"
  parseHeadPolicy policy

parseHeadPolicy :: Aeson.Value -> Parser UserValidationRules
parseHeadPolicy = Aeson.withObject "head-policy" $ \o -> do
  rejectUnknownKeys
    [ "deposits"
    , "l2-minting"
    , "allowed-assets"
    , "max-utxo-count"
    , "max-output-value-assets"
    , "max-datum-bytes"
    , "allowed-script-addresses"
    ]
    o

  policies <- optionalField "allowed-assets" Aeson.parseJSON o
  minting <- optionalField "l2-minting" parseMinting o
  utxoLimit <- optionalField "max-utxo-count" Aeson.parseJSON o
  assetLimit <- optionalField "max-output-value-assets" Aeson.parseJSON o
  datumLimit <- optionalField "max-datum-bytes" Aeson.parseJSON o
  scriptAddresses <-
    optionalField "allowed-script-addresses" parseScriptAddresses o
  deposits <- optionalField "deposits" parseDeposits o

  pure
    UserValidationRules
      { policyRule = AllowOnlyPolicies <$> policies
      , l2MintingRule = fromMaybe AllowMinting minting
      , maxUTxOCount = utxoLimit
      , maxOutputValueAssets = assetLimit
      , maxDatumSize = datumLimit
      , allowedScriptAddresses = scriptAddresses
      , allowedDeposits = deposits
      }
 where
  parseMinting :: Aeson.Value -> Parser MintingOnL2Rule
  parseMinting = Aeson.withText "l2-minting" $ \case
    "forbidden" -> pure ForbidMinting
    "allowed" -> pure AllowMinting
    _ -> fail "l2-minting must be 'forbidden' or 'allowed'"

  parseDeposits :: Aeson.Value -> Parser (Set TxId)
  parseDeposits = \case
    Aeson.String "none-after-open" -> pure Set.empty
    value@(Aeson.Array _) -> Aeson.parseJSON value
    _ -> fail "deposits must be 'none-after-open' or a list of transaction IDs"

  parseScriptAddresses :: Aeson.Value -> Parser (Set AddressInEra)
  parseScriptAddresses value = do
    addresses <- Aeson.parseJSON value :: Parser [AddressInEra]
    forM_ addresses $ \address ->
      case toLedgerAddr address of
        Ledger.Addr _ (Ledger.ScriptHashObj _) _ -> pure ()
        _ ->
          fail $
            "allowed-script-addresses requires script payment addresses: "
              <> show address
    pure $ Set.fromList addresses

-- Missing fields are optional; explicit null values are rejected.
optionalField ::
  Key ->
  (Aeson.Value -> Parser a) ->
  Object ->
  Parser (Maybe a)
optionalField key parseValue object =
  case KeyMap.lookup key object of
    Nothing -> pure Nothing
    Just value -> Just <$> parseValue value

rejectUnknownKeys :: [Key] -> Object -> Parser ()
rejectUnknownKeys known object =
  unless (null unknown) $
    fail $
      "Unknown policy fields: " <> show unknown
 where
  unknown = filter (`notElem` known) (KeyMap.keys object)

-- | All potential policy violations
data PolicyViolation
  = PolicyMintingViolation {invalidPolicyIds :: Set PolicyId, allowedPolicyIds :: Set PolicyId}
  | MaxUTxOCountExceeded
      { actualCount :: Natural
      , maximumCount :: Natural
      }
  | MaxOutputValueAssetsExceeded
      { actualAssetCount :: Natural
      , maximumAssetCount :: Natural
      }
  | MaxOutputDatumSizeExceeded
      { actualDatumSize :: Natural
      , maximumDatumSize :: Natural
      }
  | ScriptAddressNotAllowed
      { rejectedScriptAddress :: AddressInEra
      }
  | DepositNotAllowed {rejectedDepositTxId :: TxId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR PolicyViolation where
  toCBOR = genericToCBOR

instance FromCBOR PolicyViolation where
  fromCBOR = genericFromCBOR

data PolicyRule
  = ForbidAnyPolicy
  | AllowOnlyPolicies (Set PolicyId)
  deriving (Eq, Show, Generic)

data MintingOnL2Rule
  = ForbidMinting
  | AllowMinting
  deriving (Eq, Show, Generic)

data UserValidationRules = UserValidationRules
  { policyRule :: Maybe PolicyRule
  , l2MintingRule :: MintingOnL2Rule
  , maxUTxOCount :: Maybe Natural
  , maxOutputValueAssets :: Maybe Natural
  , maxDatumSize :: Maybe Natural
  , allowedScriptAddresses :: Maybe (Set AddressInEra)
  , allowedDeposits :: Maybe (Set TxId)
  }
  deriving (Eq, Show, Generic)

data UserValidationAction
  = ValidateL2Transaction Tx
  | AdmitNewDeposit TxId

-- | Abstract handle around utxo and tx types
data UserValidation tx = UserValidation
  { validateTransaction :: tx -> Either PolicyViolation ()
  , validateResultingUTxO :: UTxOType tx -> Either PolicyViolation ()
  , validateDeposit :: TxIdType tx -> Either PolicyViolation ()
  }

validateUserActionWith ::
  UserValidationRules ->
  UTxO ->
  UserValidationAction ->
  Either PolicyViolation ()
validateUserActionWith rules resultingUTxO = \case
  ValidateL2Transaction tx ->
    validateUserTransactionWith (cardanoUserValidation rules) resultingUTxO tx
  AdmitNewDeposit depositTxId ->
    validateDeposit (cardanoUserValidation rules) depositTxId

validateUserTransactionWith ::
  UserValidation tx ->
  UTxOType tx ->
  tx ->
  Either PolicyViolation ()
validateUserTransactionWith validation resultingUTxO tx = do
  validation.validateTransaction tx
  validation.validateResultingUTxO resultingUTxO

cardanoUserValidation :: UserValidationRules -> UserValidation Tx
cardanoUserValidation rules =
  UserValidation
    { validateTransaction = preValidateUserTransactionWith rules
    , validateResultingUTxO = \utxo ->
        case rules.maxUTxOCount of
          Nothing -> Right ()
          Just limit -> checkMaxUTxOCount limit utxo
    , validateDeposit = \depositTxId ->
        for_ rules.allowedDeposits $ \allowed -> checkDeposit allowed depositTxId
    }

preValidateUserTransactionWith ::
  UserValidationRules -> Tx -> Either PolicyViolation ()
preValidateUserTransactionWith rules tx = do
  case rules.policyRule of
    Nothing -> Right ()
    Just ForbidAnyPolicy -> checkPolicies mempty tx
    Just (AllowOnlyPolicies policies) -> checkPolicies policies tx
  case rules.l2MintingRule of
    ForbidMinting -> checkPolicies mempty tx
    AllowMinting -> Right ()
  case rules.maxOutputValueAssets of
    Nothing -> Right ()
    Just limit -> traverse_ (checkMaxOutputValueAssets limit) outputs
  case rules.maxDatumSize of
    Nothing -> Right ()
    Just limit -> traverse_ (checkMaxDatumSize limit) outputs
  case rules.allowedScriptAddresses of
    Nothing -> Right ()
    Just allowed -> traverse_ (checkAllowedScriptAddresses allowed) outputs
 where
  outputs = txOuts' tx

checkPolicies :: Set PolicyId -> Tx -> Either PolicyViolation ()
checkPolicies policies tx = do
  let mintedValue =
        txMintValueToValue . txMintValue . getTxBodyContent $ getTxBody tx
      usedPolicies =
        Set.fromList
          [ policyId
          | (AssetId policyId _, quantity) <- toList mintedValue
          , quantity /= 0
          ]
      disallowedPolicies = usedPolicies `Set.difference` policies

  unless (Set.null disallowedPolicies) $
    Left
      PolicyMintingViolation
        { invalidPolicyIds = disallowedPolicies
        , allowedPolicyIds = policies
        }

checkMaxUTxOCount :: Natural -> UTxO -> Either PolicyViolation ()
checkMaxUTxOCount limit utxo =
  let actual = fromIntegral (Map.size (UTxO.unUTxO utxo))
   in if actual <= limit
        then Right ()
        else
          Left
            MaxUTxOCountExceeded
              { actualCount = actual
              , maximumCount = limit
              }

checkMaxOutputValueAssets :: Natural -> TxOut ctx -> Either PolicyViolation ()
checkMaxOutputValueAssets limit (TxOut _ value _ _) =
  let assets =
        Set.fromList
          [ assetId
          | (assetId@(AssetId _ _), quantity) <-
              toList value
          , quantity /= 0
          ]
      actual = fromIntegral (Set.size assets)
   in if actual <= limit
        then Right ()
        else
          Left
            MaxOutputValueAssetsExceeded
              { actualAssetCount = actual
              , maximumAssetCount = limit
              }

checkMaxDatumSize :: Natural -> TxOut ctx -> Either PolicyViolation ()
checkMaxDatumSize limit (TxOut _ _ datum _) =
  case datum of
    TxOutDatumNone -> Right ()
    TxOutDatumHash _ _ -> Right ()
    TxOutDatumInline _ scriptData -> checkSize scriptData
    TxOutSupplementalDatum _ scriptData -> checkSize scriptData
 where
  checkSize :: HashableScriptData -> Either PolicyViolation ()
  checkSize scriptData =
    let actual =
          fromIntegral (BS.length (getOriginalScriptDataBytes scriptData))
     in if actual <= limit
          then Right ()
          else
            Left
              MaxOutputDatumSizeExceeded
                { actualDatumSize = actual
                , maximumDatumSize = limit
                }

checkAllowedScriptAddresses :: Set AddressInEra -> TxOut ctx -> Either PolicyViolation ()
checkAllowedScriptAddresses allowed (TxOut address _ _ _) =
  case toLedgerAddr address of
    Ledger.Addr _ (Ledger.ScriptHashObj _) _ ->
      unless (Set.member address allowed) $
        Left
          ScriptAddressNotAllowed
            { rejectedScriptAddress = address
            }
    _ -> Right ()

checkDeposit :: Set TxId -> TxId -> Either PolicyViolation ()
checkDeposit allowedDeposits depositTxId =
  unless (Set.member depositTxId allowedDeposits) reject
 where
  reject =
    Left
      DepositNotAllowed
        { rejectedDepositTxId = depositTxId
        }
