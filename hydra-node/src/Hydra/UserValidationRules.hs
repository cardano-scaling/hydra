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
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Cardano.Api (AddressInEra, PolicyId, Tx, TxId, TxOut, UTxO, toLedgerAddr, txOuts', pattern TxOut)
import Hydra.Prelude hiding (toList)

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
  deriving (Eq, Show)

data PolicyRule
  = ForbidAnyPolicy
  | AllowOnlyPolicies (Set PolicyId)
  deriving (Eq, Show, Generic)

data MintingOnL2Rule
  = ForbidMinting
  | AllowMinting
  deriving (Eq, Show, Generic)

data DepositRule
  = NoDepositsAfterOpen
  | AllowOnlyDeposits (Set TxId)
  deriving (Eq, Show, Generic)

data UserValidationRules = UserValidationRules
  { policyRule :: Maybe PolicyRule
  , l2MintingRule :: MintingOnL2Rule
  , maxUTxOCount :: Maybe Natural
  , maxOutputValueAssets :: Maybe Natural
  , maxDatumSize :: Maybe Natural
  , allowedScriptAddresses :: Maybe (Set AddressInEra)
  , depositRule :: Maybe DepositRule
  }
  deriving (Eq, Show, Generic)

data UserValidationAction
  = ValidateL2Transaction Tx
  | AdmitNewDeposit TxId

validateUserActionWith ::
  UserValidationRules ->
  UTxO ->
  UserValidationAction ->
  Either PolicyViolation ()
validateUserActionWith rules resultingUTxO = \case
  ValidateL2Transaction tx ->
    validateUserTransactionWith rules resultingUTxO tx
  AdmitNewDeposit depositTxId ->
    case rules.depositRule of
      Nothing -> Right ()
      Just rule -> checkDeposit rule depositTxId

validateUserTransactionWith :: UserValidationRules -> UTxO -> Tx -> Either PolicyViolation ()
validateUserTransactionWith rules resultingUTxO tx = do
  case rules.policyRule of
    Just ForbidAnyPolicy -> checkPolicies mempty tx
    Just (AllowOnlyPolicies policies) -> checkPolicies policies tx
    Nothing -> Right ()
  case rules.l2MintingRule of
    ForbidMinting -> checkPolicies mempty tx
    AllowMinting -> Right ()
  case rules.maxUTxOCount of
    Nothing -> Right ()
    Just c -> checkMaxUTxOCount c resultingUTxO
  case rules.maxOutputValueAssets of
    Nothing -> Right ()
    Just limit ->
      traverse_ (checkMaxOutputValueAssets limit) (txOuts' tx)
  case rules.maxDatumSize of
    Nothing -> Right ()
    Just limit ->
      traverse_ (checkMaxDatumSize limit) (txOuts' tx)
  case rules.allowedScriptAddresses of
    Nothing -> Right ()
    Just allowed ->
      traverse_ (checkAllowedScriptAddresses allowed) (txOuts' tx)

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

checkDeposit :: DepositRule -> TxId -> Either PolicyViolation ()
checkDeposit rule depositTxId =
  case rule of
    NoDepositsAfterOpen -> reject
    AllowOnlyDeposits allowed ->
      unless (Set.member depositTxId allowed) reject
 where
  reject =
    Left
      DepositNotAllowed
        { rejectedDepositTxId = depositTxId
        }
