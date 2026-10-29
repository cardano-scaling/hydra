-- NOTE: Uses the TxBody/TxBodyContent API that cardano-api deprecated in favour of
-- Cardano.Api.Experimental; the migration is tracked separately in
-- https://github.com/cardano-scaling/hydra/issues/2896
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.UserValidationRulesSpec where

import Data.ByteString qualified as BS
import Data.Set qualified as Set
import GHC.IsList (toList)
import Hydra.Cardano.Api (
  AssetId (..),
  CtxTx,
  Quantity (..),
  ScriptData (..),
  TxOut,
  getOriginalScriptDataBytes,
  getTxBody,
  getTxBodyContent,
  hashScriptDataBytes,
  txMintValue,
  txMintValueToValue,
  txOuts',
  unsafeHashableScriptData,
  pattern TxOut,
  pattern TxOutDatumHash,
  pattern TxOutDatumInline,
  pattern TxOutDatumNone,
  pattern TxOutSupplementalDatum,
 )
import Hydra.Prelude hiding (toList)
import Hydra.UserValidationRules (
  MintingOnL2Rule (..),
  PolicyRule (..),
  PolicyViolation (..),
  UserValidationRules (..),
  checkMaxDatumSize,
  checkMaxOutputValueAssets,
  checkMaxUTxOCount,
  checkPolicies,
  validateUserTransactionWith,
 )
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (genTxOut, genTxWithMint, genUTxO)
import Test.QuickCheck (
  Property,
  checkCoverage,
  chooseInteger,
  conjoin,
  counterexample,
  coverTable,
  forAll,
  forAllBlind,
  listOf1,
  oneof,
  resize,
  sublistOf,
  tabulate,
  (===),
 )

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
    prop "checkMaxOutputValueAssets" prop_checkMaxOutputValueAssets
    prop "checkMaxDatumSize" prop_checkMaxDatumSize
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
    case maxOutputValueAssets rules of
      Nothing -> Right ()
      Just limit ->
        traverse_ (checkMaxOutputValueAssets limit) (txOuts' tx)
    case maxDatumSize rules of
      Nothing -> Right ()
      Just limit ->
        traverse_ (checkMaxDatumSize limit) (txOuts' tx)

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

prop_checkMaxOutputValueAssets :: Property
prop_checkMaxOutputValueAssets =
  forAll genCase $ \(TxOut address _ datum refScript, policyId, names, quantity) ->
    let value =
          fromList $
            (AdaAssetId, Quantity 2000000)
              : [ (AssetId policyId name, Quantity quantity)
                | name <- Set.toList names
                ]
        output = TxOut address value datum refScript
        count = fromIntegral (Set.size names)
     in tabulate "Output assets" [if count == 0 then "ADA only" else "tokens"] $
          conjoin $
            [ checkMaxOutputValueAssets count output === Right ()
            , checkMaxOutputValueAssets (count + 1) output === Right ()
            ]
              <> [ checkMaxOutputValueAssets (count - 1) output
                  === Left
                    MaxOutputValueAssetsExceeded
                      { actualAssetCount = count
                      , maximumAssetCount = count - 1
                      }
                 | count > 0
                 ]
 where
  genCase = do
    output <- genTxOut :: Gen (TxOut CtxTx)
    policyId <- arbitrary
    names <-
      oneof
        [ pure Set.empty
        , Set.fromList <$> resize 12 (listOf1 arbitrary)
        ]
    quantity <- chooseInteger (1, 1000000)
    pure (output, policyId, names, quantity)

prop_checkMaxDatumSize :: Property
prop_checkMaxDatumSize =
  forAllBlind (genTxOut :: Gen (TxOut CtxTx)) $ \(TxOut address value _ refScript) ->
    forAll (arbitrary :: Gen [Word8]) $ \bytes ->
      let scriptData =
            unsafeHashableScriptData (ScriptDataBytes (BS.pack bytes))
          datumSize =
            fromIntegral (BS.length (getOriginalScriptDataBytes scriptData))
          output datum = TxOut address value datum refScript
          withOutput datum assertion =
            let actualOutput = output datum
             in counterexample
                  ("Output being checked: " <> show actualOutput)
                  $ assertion actualOutput
          checkPayload datum =
            withOutput datum $ \actualOutput ->
              counterexample ("Encoded datum bytes: " <> show datumSize) $
                conjoin $
                  [ checkMaxDatumSize datumSize actualOutput === Right ()
                  , checkMaxDatumSize (datumSize + 1) actualOutput === Right ()
                  ]
                    <> [ checkMaxDatumSize (datumSize - 1) actualOutput
                        === Left
                          MaxOutputDatumSizeExceeded
                            { actualDatumSize = datumSize
                            , maximumDatumSize = datumSize - 1
                            }
                       | datumSize > 0
                       ]
       in conjoin
            [ counterexample "TxOutDatumInline" $
                checkPayload (TxOutDatumInline scriptData)
            , counterexample "TxOutSupplementalDatum" $
                checkPayload (TxOutSupplementalDatum scriptData)
            , counterexample "TxOutDatumNone" $
                withOutput TxOutDatumNone $ \actualOutput ->
                  checkMaxDatumSize 0 actualOutput === Right ()
            , counterexample "TxOutDatumHash" $
                withOutput (TxOutDatumHash (hashScriptDataBytes scriptData)) $ \actualOutput ->
                  checkMaxDatumSize 0 actualOutput === Right ()
            ]
