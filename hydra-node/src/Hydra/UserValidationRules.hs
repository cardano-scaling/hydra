{-# LANGUAGE OverloadedRecordDot #-}
-- NOTE: Uses the TxBody/TxBodyContent API that cardano-api deprecated in favour of
-- Cardano.Api.Experimental; the migration is tracked separately in
-- https://github.com/cardano-scaling/hydra/issues/2896
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.UserValidationRules where

import Cardano.Api (AssetId (..), TxBodyContent (txMintValue), getTxBody, getTxBodyContent, txMintValueToValue)
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Cardano.Api (PolicyId, Tx, UTxO)
import Hydra.Prelude hiding (toList)
import qualified Data.Map as Map
import qualified Cardano.Api.UTxO as UTxO

data PolicyViolation
  = PolicyMintingViolation {invalidPolicyIds :: Set PolicyId, allowedPolicyIds :: Set PolicyId}
  | MaxUTxOCountExceeded
      { actualCount :: Natural
      , maximumCount :: Natural
      }
  deriving (Eq, Show)

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
  }
  deriving (Eq, Show, Generic)

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
        else Left MaxUTxOCountExceeded
          { actualCount = actual
          , maximumCount = limit
          }
