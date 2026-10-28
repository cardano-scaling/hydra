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
  MintingOnL2Rule (..),
  PolicyRule (..),
  UserValidationRules (..),
  checkPolicies,
  validateUserTransactionWith,
  checkMaxUTxOCount,
 )
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (genTxWithMint, genUTxO)
import Test.QuickCheck (Property, checkCoverage, coverTable, forAll, oneof, sublistOf, tabulate, (===))

instance Arbitrary PolicyRule where
  arbitrary = genericArbitrary

instance Arbitrary MintingOnL2Rule where
  arbitrary = genericArbitrary

instance Arbitrary UserValidationRules where
  arbitrary = genericArbitrary

spec :: Spec
spec = do
  describe "user validated transactions" $ do
    prop "checkPolicies" prop_checkMintedAssets
    prop "validateUserTransactionWith" prop_validateUserTransactionWith

prop_validateUserTransactionWith :: Property
prop_validateUserTransactionWith =
  forAll genCase $ \(tx, utxo, rules) ->
    tabulate "Policy rule" [policyCategory (policyRule rules)] $
      tabulate "L2 minting rule" [show (l2MintingRule rules)] $
        validateUserTransactionWith rules utxo tx
          === expectedResult rules utxo tx
 where
  genCase =
    (,,) <$> genTxWithMint <*> genUTxO <*> arbitrary

  policyCategory = \case
    Nothing -> "policy restriction disabled"
    Just ForbidAnyPolicy -> "forbid all policies"
    Just (AllowOnlyPolicies _) -> "allow selected policies"

  expectedResult rules utxo tx = do
    case policyRule rules of
      Nothing -> Right ()
      Just ForbidAnyPolicy -> checkPolicies Set.empty tx
      Just (AllowOnlyPolicies allowed) -> checkPolicies allowed tx
    case l2MintingRule rules of
      ForbidMinting -> checkPolicies Set.empty tx
      AllowMinting -> Right ()
    maybe (Right ()) (`checkMaxUTxOCount` utxo) (maxUTxOCount rules)

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
                    isRight (checkPolicies allowed tx) === expected
