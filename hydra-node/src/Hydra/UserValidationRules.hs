{-# LANGUAGE OverloadedRecordDot #-}
-- NOTE: Uses the TxBody/TxBodyContent API that cardano-api deprecated in favour of
-- Cardano.Api.Experimental; the migration is tracked separately in
-- https://github.com/cardano-scaling/hydra/issues/2896
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.UserValidationRules where

import Cardano.Api (AssetId (..), TxBodyContent (txMintValue), getTxBody, getTxBodyContent, txMintValueToValue)
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Cardano.Api (PolicyId, Tx)
import Hydra.Prelude hiding (toList)

data PolicyViolation
  = PolicyMintingViolation {invalidPolicyIds :: Set PolicyId, allowedPolicyIds :: Set PolicyId}
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
  }
  deriving (Eq, Show, Generic)

validateUserTransactionWith :: UserValidationRules -> Tx -> Either PolicyViolation ()
validateUserTransactionWith rules tx = do
  case rules.policyRule of
    Just ForbidAnyPolicy -> checkPolicies mempty tx
    Just (AllowOnlyPolicies policies) -> checkPolicies policies tx
    Nothing -> Right ()
  case rules.l2MintingRule of
    ForbidMinting -> checkPolicies mempty tx
    AllowMinting -> Right ()

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
