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

data MintingRule
  = ForbidAnyPolicy
  | AllowOnlyPolicies (Set PolicyId)
  deriving (Eq, Show, Generic)

newtype UserValidationRules = UserValidationRules
  { mintingRule :: Maybe MintingRule
  }
  deriving (Eq, Show, Generic)

validateUserTransactionWith :: UserValidationRules -> Tx -> Either PolicyViolation ()
validateUserTransactionWith rules tx =
  case rules.mintingRule of
    Just ForbidAnyPolicy -> checkMinting mempty tx -- here if policies is empty it means we don't accept any
    Just (AllowOnlyPolicies policies) -> checkMinting policies tx
    Nothing -> Right ()

checkMinting :: Set PolicyId -> Tx -> Either PolicyViolation ()
checkMinting policies tx = do
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
