-- NOTE: Uses the TxBody/TxBodyContent API that cardano-api deprecated in favour of
-- Cardano.Api.Experimental; the migration is tracked separately in
-- https://github.com/cardano-scaling/hydra/issues/2896
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.UserValidationRulesSpec where

import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Cardano.Api (AssetId (..), getTxBody, getTxBodyContent, txMintValue, txMintValueToValue)
import Hydra.Prelude hiding (toList)
import Hydra.UserValidationRules (
  MintingRule (..),
  UserValidationRules (..),
  checkMinting,
  validateUserTransactionWith,
 )
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (genTxWithMint)
import Test.QuickCheck (Property, checkCoverage, coverTable, forAll, oneof, sublistOf, tabulate, (===))

instance Arbitrary MintingRule where
  arbitrary = genericArbitrary

instance Arbitrary UserValidationRules where
  arbitrary = genericArbitrary

spec :: Spec
spec = do
  describe "user validated transactions" $ do
    prop "checkMinting" prop_checkMintedAssets
    prop "validateUserTransactionWith" prop_validateUserTransactionWith

prop_validateUserTransactionWith :: Property
prop_validateUserTransactionWith =
  forAll genTxWithMint $ \tx ->
    forAll arbitrary $ \rules@(UserValidationRules rule) ->
      let (category, expected) = case rule of
            Nothing ->
              ("validation disabled", Right ())
            Just ForbidAnyPolicy ->
              ("forbid all policies", checkMinting Set.empty tx)
            Just (AllowOnlyPolicies allowed) ->
              ("allow selected policies", checkMinting allowed tx)
       in tabulate "Minting rule" [category] $
            validateUserTransactionWith rules tx === expected

prop_checkMintedAssets :: Property
prop_checkMintedAssets =
  checkCoverage
    $ coverTable
      "Policy cases"
      [ ("no mint or burn", 20)
      , ("empty allowlist rejects", 20)
      , ("allowed policies accept", 20)
      ]
    $ forAll genTxWithMint
    $ \tx ->
      let mintedValue =
            txMintValueToValue . txMintValue . getTxBodyContent $
              getTxBody tx
          usedPolicies =
            Set.fromList
              [ policyId
              | (AssetId policyId _, quantity) <- toList mintedValue
              , quantity /= 0
              ]
       in forAll
            ( oneof
                [ pure Set.empty
                , pure usedPolicies
                , Set.fromList <$> sublistOf (Set.toList usedPolicies)
                ]
            )
            $ \allowed ->
              let expected = usedPolicies `Set.isSubsetOf` allowed
                  category
                    | Set.null usedPolicies = "no mint or burn"
                    | Set.null allowed = "empty allowlist rejects"
                    | expected = "allowed policies accept"
                    | otherwise = "partial allowlist rejects"
               in tabulate "Policy cases" [category] $
                    isRight (checkMinting allowed tx) === expected
