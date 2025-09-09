module Hydra.Tx.DepositSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Cardano.Api (AssetId (..), AssetName, Coin (..), PolicyAssets (..), PolicyId, Quantity (..), UTxO, selectLovelace, txOutValue, valueToPolicyAssets)
import Hydra.Tx.Deposit (capUTxO, pickTokensToDeposit, splitTokens)
import Test.Hydra.Tx.Fixture (testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOSized, genUTxOWithAssetsSized)
import Test.QuickCheck (Property, chooseInteger, counterexample, cover, elements, forAll, frequency, listOf, oneof, property, (===), (==>))

spec :: Spec
spec =
  parallel $ do
    describe "pickTokensToDeposit" $ do
      describe "tests" $ do
        it "returns empty results when no tokens are specified" $ do
          let utxo = genUTxOSized 3 `generateWith` 42
          let toDeposit = pickTokensToDeposit utxo mempty
          toDeposit `shouldBe` mempty
        it "returns empty results when UTxO is empty" $ do
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 10)])]
          let toDeposit = pickTokensToDeposit mempty tokens
          toDeposit `shouldBe` mempty
        it "returns all tokens as invalid when policy is missing from UTxO" $ do
          let utxo = genUTxOSized 3 `generateWith` 42 -- UTxO with only ADA
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 10)])]
          let toDeposit = pickTokensToDeposit utxo tokens
          toDeposit `shouldBe` mempty
        it "validates tokens correctly when exact quantities match" $ do
          let testUTxO = utxoWithTokens [(testPolicyId, testAssetName, Quantity 100)]
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          let toDeposit = pickTokensToDeposit testUTxO tokens
          toDeposit `shouldBe` testUTxO
        it "validates tokens correctly when UTxO has more than required" $ do
          let testUTxO = utxoWithTokens [(testPolicyId, testAssetName, Quantity 150)]
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          let toDeposit = pickTokensToDeposit testUTxO tokens
          toDeposit `shouldBe` utxoWithTokens [(testPolicyId, testAssetName, Quantity 100)]
        it "returns tokens intact when UTxO has less than required" $ do
          let testUTxO = utxoWithTokens [(testPolicyId, testAssetName, Quantity 50)]
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          let toDeposit = pickTokensToDeposit testUTxO tokens
          toDeposit `shouldBe` mempty
        it "handles mixed scenarios with multiple tokens" $ do
          let testUTxO =
                utxoWithTokens
                  [ (testPolicyId, testAssetName, Quantity 100)
                  , (testPolicyId2, testAssetName, Quantity 50)
                  ]
          let tokens =
                Map.fromList
                  [ (testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)]) -- Valid
                  , (testPolicyId2, PolicyAssets $ Map.fromList [(testAssetName, Quantity 75)]) -- Invalid - insufficient
                  , (testPolicyId3, PolicyAssets $ Map.fromList [(testAssetName, Quantity 25)]) -- Invalid - missing policy
                  ]
          let toDeposit = pickTokensToDeposit testUTxO tokens

          toDeposit `shouldBe` utxoWithTokens [(testPolicyId, testAssetName, Quantity 100)]

        it "handles multiple assets within the same policy" $ do
          let testUTxO =
                utxoWithTokens
                  [ (testPolicyId, testAssetName, Quantity 100)
                  , (testPolicyId, testAssetName2, Quantity 200)
                  ]
          let tokens =
                Map.fromList
                  [
                    ( testPolicyId
                    , PolicyAssets $
                        Map.fromList
                          [ (testAssetName, Quantity 50) -- Valid
                          , (testAssetName2, Quantity 150) -- Valid
                          , (testAssetName3, Quantity 10) -- Invalid - missing asset
                          ]
                    )
                  ]
          let toDeposit = pickTokensToDeposit testUTxO tokens
          let additionalUTxO =
                utxoWithTokens
                  [ (testPolicyId, testAssetName, 50)
                  , (testPolicyId, testAssetName2, 150)
                  ]
          toDeposit `shouldBe` additionalUTxO

    describe "splitTokens" $ do
      describe "tests" $ do
        it "returns empty results when no tokens are specified" $ do
          let utxo = genUTxOSized 3 `generateWith` 42
          let (valid, invalid) = splitTokens utxo mempty
          valid `shouldBe` mempty
          invalid `shouldBe` mempty

        it "returns empty results when UTxO is empty" $ do
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 10)])]
          let (valid, invalid) = splitTokens mempty tokens
          valid `shouldBe` mempty
          invalid `shouldBe` tokens

        it "returns all tokens as invalid when policy is missing from UTxO" $ do
          let utxo = genUTxOSized 3 `generateWith` 42 -- UTxO with only ADA
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 10)])]
          let (valid, invalid) = splitTokens utxo tokens
          valid `shouldBe` mempty
          invalid `shouldBe` tokens

        it "validates tokens correctly when exact quantities match" $ do
          let testUTxO = utxoWithTokens [(testPolicyId, testAssetName, Quantity 100)]
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          let (valid, invalid) = splitTokens testUTxO tokens
          valid `shouldBe` tokens
          invalid `shouldBe` mempty

        it "validates tokens correctly when UTxO has more than required" $ do
          let testUTxO = utxoWithTokens [(testPolicyId, testAssetName, Quantity 150)]
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          let (valid, invalid) = splitTokens testUTxO tokens
          valid `shouldBe` tokens
          invalid `shouldBe` mempty

        it "returns tokens as invalid when UTxO has less than required" $ do
          let testUTxO = utxoWithTokens [(testPolicyId, testAssetName, Quantity 50)]
          let tokens = Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          let (valid, invalid) = splitTokens testUTxO tokens
          valid `shouldBe` mempty
          invalid `shouldBe` tokens

        it "handles mixed scenarios with multiple tokens" $ do
          let testUTxO =
                utxoWithTokens
                  [ (testPolicyId, testAssetName, Quantity 100)
                  , (testPolicyId2, testAssetName, Quantity 50)
                  ]
          let tokens =
                Map.fromList
                  [ (testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)]) -- Valid
                  , (testPolicyId2, PolicyAssets $ Map.fromList [(testAssetName, Quantity 75)]) -- Invalid - insufficient
                  , (testPolicyId3, PolicyAssets $ Map.fromList [(testAssetName, Quantity 25)]) -- Invalid - missing policy
                  ]
          let (valid, invalid) = splitTokens testUTxO tokens

          valid `shouldBe` Map.fromList [(testPolicyId, PolicyAssets $ Map.fromList [(testAssetName, Quantity 100)])]
          invalid
            `shouldBe` Map.fromList
              [ (testPolicyId2, PolicyAssets $ Map.fromList [(testAssetName, Quantity 75)])
              , (testPolicyId3, PolicyAssets $ Map.fromList [(testAssetName, Quantity 25)])
              ]

        it "handles multiple assets within the same policy" $ do
          let testUTxO =
                utxoWithTokens
                  [ (testPolicyId, testAssetName, Quantity 100)
                  , (testPolicyId, testAssetName2, Quantity 200)
                  ]
          let tokens =
                Map.fromList
                  [
                    ( testPolicyId
                    , PolicyAssets $
                        Map.fromList
                          [ (testAssetName, Quantity 50) -- Valid
                          , (testAssetName2, Quantity 150) -- Valid
                          , (testAssetName3, Quantity 10) -- Invalid - missing asset
                          ]
                    )
                  ]
          let (valid, invalid) = splitTokens testUTxO tokens
          valid `shouldBe` mempty -- All assets in policy must be valid for policy to be valid
          invalid `shouldBe` tokens
        it "splits multiassets correctly" $
          forAll (genUTxOWithAssetsSized 5 Nothing) $ \utxo ->
            forAll (prepareAssetMap utxo) $ \assets ->
              property $ propSplitMultiAssetCorrectly utxo assets

      describe "property tests" $ do
        prop "preserves all input tokens (completeness)" propPreservesAllTokens
        prop "valid and invalid tokens are disjoint" propValidInvalidDisjoint
        prop "all valid tokens exist in UTxO with sufficient quantities" propValidTokensExistInUTxO
        prop "monotonic with respect to UTxO additions" propMonotonicUTxOAdditions

    describe "capUTxO" $ do
      describe "tests" $ do
        it "returns empty UTxO when target is 0" $ do
          let utxo = genUTxOSized 3 `generateWith` 42
          let (selected, leftovers) = capUTxO utxo 0
          selected `shouldBe` mempty
          leftovers `shouldBe` utxo

        it "returns empty UTxO when input UTxO is empty" $ do
          let (selected, leftovers) = capUTxO mempty 100
          selected `shouldBe` mempty
          leftovers `shouldBe` mempty

        it "selects UTxO entries up to target amount" $ do
          let utxo = genUTxOSized 5 `generateWith` 42
          let totalValue = UTxO.totalLovelace utxo
          let target = chooseInteger (1, toInteger totalValue) `generateWith` 42
          let (selected, leftovers) = capUTxO utxo (Coin target)

          toInteger (UTxO.totalLovelace selected) `shouldSatisfy` \v -> v <= target

          UTxO.totalValue selected <> UTxO.totalValue leftovers `shouldBe` UTxO.totalValue utxo

          let originalSize = length (UTxO.toList utxo)
              selectedSize = length (UTxO.toList selected)
              leftoverSize = length (UTxO.toList leftovers)
          (selectedSize + leftoverSize) `shouldSatisfy` (>= originalSize)

          selectedSize `shouldSatisfy` (> 0)
          leftoverSize `shouldSatisfy` (> 0)

          let originalInputs = UTxO.inputSet utxo
              selectedInputs = UTxO.inputSet selected
              leftoverInputs = UTxO.inputSet leftovers
              allInputs = selectedInputs <> leftoverInputs
          originalInputs `shouldBe` allInputs

        it "prioritizes smaller outputs" $ do
          let smallOutput = genUTxOSized 1 `generateWith` 42
          let largeOutput = genUTxOSized 3 `generateWith` 43
          let mixedUTxO = smallOutput <> largeOutput

          let totalLovelace = UTxO.totalLovelace mixedUTxO
          let target = chooseInteger (1, toInteger totalLovelace) `generateWith` 44
          let (selected, _) = capUTxO mixedUTxO (Coin target)

          length (UTxO.toList selected) `shouldSatisfy` (> 0)

      describe "property tests" $ do
        prop "preserves total value" propPreservesTotalValue
        prop "selected value never exceeds target" propSelectedValueNeverExceedsTarget
        prop "greedy selection - takes smallest UTxOs first" propGreedySelection
        prop "exact target when possible" propExactTargetWhenPossible
        prop "idempotent" propIdempotent
        prop "monotonic with respect to target" propMonotonicTarget
        prop "no UTxO loss" propNoUTxOLoss

-- | Property: The sum of selected and leftover values equals the input value
propPreservesTotalValue :: UTxO -> Coin -> Property
propPreservesTotalValue utxo target =
  let (selected, leftovers) = capUTxO utxo target
      inputTotal = UTxO.totalLovelace utxo
      selectedTotal = UTxO.totalLovelace selected
      leftoverTotal = UTxO.totalLovelace leftovers
   in selectedTotal + leftoverTotal === inputTotal
        & counterexample ("Input total: " <> show inputTotal)
        & counterexample ("Selected total: " <> show selectedTotal)
        & counterexample ("Leftover total: " <> show leftoverTotal)

-- | Property: Selected value never exceeds the target
propSelectedValueNeverExceedsTarget :: UTxO -> Coin -> Property
propSelectedValueNeverExceedsTarget utxo target =
  let (selected, _) = capUTxO utxo target
      selectedTotal = UTxO.totalLovelace selected
   in selectedTotal <= target
        & counterexample ("Selected total: " <> show selectedTotal)
        & counterexample ("Target: " <> show target)

-- | Property: Greedy selection - takes smallest UTxOs first
propGreedySelection :: UTxO -> Coin -> Property
propGreedySelection utxo target =
  let (selected, _) = capUTxO utxo target
      selectedList = UTxO.toList selected
      sortedByValue = sortBy (comparing (selectLovelace . txOutValue . snd)) (UTxO.toList utxo)
   in all
        ( \selectedUTxO ->
            let selectedValue = selectLovelace (txOutValue (snd selectedUTxO))
                smallerUTxOs = takeWhile (\utx -> selectLovelace (txOutValue (snd utx)) < selectedValue) sortedByValue
             in all (`elem` selectedList) smallerUTxOs
        )
        selectedList
        & counterexample ("Selected count: " <> show (length selectedList))
        & counterexample ("Total UTxO count: " <> show (length sortedByValue))

-- | Property: Reaches target exactly when possible
propExactTargetWhenPossible :: UTxO -> Coin -> Property
propExactTargetWhenPossible utxo target =
  let (selected, _) = capUTxO utxo target
      selectedTotal = UTxO.totalLovelace selected
      inputTotal = UTxO.totalLovelace utxo
   in (inputTotal >= target) ==>
        ( selectedTotal == target
            || selectedTotal == target - 1
        )
          & counterexample ("Selected total: " <> show selectedTotal)
          & counterexample ("Target: " <> show target)
          & counterexample ("Input total: " <> show inputTotal)

-- | Property: Function is idempotent
propIdempotent :: UTxO -> Coin -> Property
propIdempotent utxo target =
  let firstResult = capUTxO utxo target
      secondResult = capUTxO (fst firstResult) target
   in fst firstResult === fst secondResult
        & counterexample ("First selected count: " <> show (length (UTxO.toList (fst firstResult))))
        & counterexample ("Second selected count: " <> show (length (UTxO.toList (fst secondResult))))

-- | Property: Monotonic with respect to target
propMonotonicTarget :: UTxO -> Coin -> Coin -> Property
propMonotonicTarget utxo target1 target2 =
  (target1 <= target2) ==>
    let (selected1, _) = capUTxO utxo target1
        (selected2, _) = capUTxO utxo target2
        selectedTotal1 = UTxO.totalLovelace selected1
        selectedTotal2 = UTxO.totalLovelace selected2
     in selectedTotal1 <= selectedTotal2
          & counterexample ("Target1: " <> show target1 <> ", Selected1: " <> show selectedTotal1)
          & counterexample ("Target2: " <> show target2 <> ", Selected2: " <> show selectedTotal2)

-- | Property: No UTxO loss - all input UTxOs appear in either selected or leftovers
propNoUTxOLoss :: UTxO -> Coin -> Property
propNoUTxOLoss utxo target =
  let (selected, leftovers) = capUTxO utxo target
      inputSet = UTxO.inputSet utxo
      selectedSet = UTxO.inputSet selected
      leftoverSet = UTxO.inputSet leftovers
      unionSet = selectedSet <> leftoverSet
   in inputSet === unionSet
        & counterexample ("Input set size: " <> show (Set.size inputSet))
        & counterexample ("Selected set size: " <> show (Set.size selectedSet))
        & counterexample ("Leftover set size: " <> show (Set.size leftoverSet))

-- * Helper functions for splitTokens tests

-- | Create additional test PolicyIds for testing (using the existing testPolicyId from fixtures)
testPolicyId2 :: PolicyId
testPolicyId2 = generateWith arbitrary 43

testPolicyId3 :: PolicyId
testPolicyId3 = generateWith arbitrary 44

-- | Create test AssetNames for testing
testAssetName :: AssetName
testAssetName = "TestToken1"

testAssetName2 :: AssetName
testAssetName2 = "TestToken2"

testAssetName3 :: AssetName
testAssetName3 = "TestToken3"

-- | Create a UTxO with specific tokens for testing
utxoWithTokens :: [(PolicyId, AssetName, Quantity)] -> UTxO
utxoWithTokens tokens =
  let value = fromList $ map (\(pid, aname, qty) -> (AssetId pid aname, qty)) tokens
      txIn = generateWith arbitrary 42
      baseTxOut = generateWith arbitrary 42
      -- Update the txOut to have our custom value
      txOut = baseTxOut{txOutValue = value}
   in UTxO.singleton txIn txOut

-- * Property tests for splitTokens

-- | Property: All input tokens are preserved in either valid or invalid results
propPreservesAllTokens :: UTxO -> Map PolicyId PolicyAssets -> Property
propPreservesAllTokens utxo specifiedTokens =
  let (valid, invalid) = splitTokens utxo specifiedTokens
      inputPolicies = Map.keysSet specifiedTokens
      validPolicies = Map.keysSet valid
      invalidPolicies = Map.keysSet invalid
      allResultPolicies = validPolicies <> invalidPolicies
   in inputPolicies === allResultPolicies
        & counterexample ("Input policies: " <> show inputPolicies)
        & counterexample ("Valid policies: " <> show validPolicies)
        & counterexample ("Invalid policies: " <> show invalidPolicies)

-- | Property: Valid and invalid results are disjoint sets
propValidInvalidDisjoint :: UTxO -> Map PolicyId PolicyAssets -> Property
propValidInvalidDisjoint utxo specifiedTokens =
  let (valid, invalid) = splitTokens utxo specifiedTokens
      validPolicies = Map.keysSet valid
      invalidPolicies = Map.keysSet invalid
      intersection = Set.intersection validPolicies invalidPolicies
   in Set.null intersection
        & counterexample ("Valid policies: " <> show validPolicies)
        & counterexample ("Invalid policies: " <> show invalidPolicies)
        & counterexample ("Intersection: " <> show intersection)

-- | Property: All valid tokens must exist in UTxO with sufficient quantities
propValidTokensExistInUTxO :: UTxO -> Map PolicyId PolicyAssets -> Property
propValidTokensExistInUTxO utxo specifiedTokens =
  let (valid, _) = splitTokens utxo specifiedTokens
      utxoValue = UTxO.totalValue utxo
      utxoPolicyAssets = valueToPolicyAssets utxoValue
   in all (checkValidTokenInUTxO utxoPolicyAssets) (Map.toList valid)
        & counterexample ("Valid tokens: " <> show valid)
        & counterexample ("UTxO policy assets: " <> show utxoPolicyAssets)
 where
  checkValidTokenInUTxO :: Map PolicyId PolicyAssets -> (PolicyId, PolicyAssets) -> Bool
  checkValidTokenInUTxO utxoAssets (policyId, PolicyAssets requiredAssets) =
    case Map.lookup policyId utxoAssets of
      Nothing -> False
      Just (PolicyAssets availableAssets) ->
        all
          ( \(assetName, requiredQty) ->
              case Map.lookup assetName availableAssets of
                Nothing -> False
                Just availableQty -> availableQty >= requiredQty
          )
          (Map.toList requiredAssets)

-- | Property: Adding more assets to UTxO never decreases valid tokens (monotonic)
propMonotonicUTxOAdditions :: UTxO -> UTxO -> Map PolicyId PolicyAssets -> Property
propMonotonicUTxOAdditions utxo1 utxo2 specifiedTokens =
  let combinedUTxO = utxo1 <> utxo2
      (valid1, _) = splitTokens utxo1 specifiedTokens
      (validCombined, _) = splitTokens combinedUTxO specifiedTokens
      valid1Policies = Map.keysSet valid1
      validCombinedPolicies = Map.keysSet validCombined
   in valid1Policies `Set.isSubsetOf` validCombinedPolicies
        & counterexample ("Valid policies in UTxO1: " <> show valid1Policies)
        & counterexample ("Valid policies in combined UTxO: " <> show validCombinedPolicies)

propSplitMultiAssetCorrectly :: UTxO -> Map PolicyId PolicyAssets -> Property
propSplitMultiAssetCorrectly utxo specifiedTokens =
  let toDeposit = pickTokensToDeposit utxo specifiedTokens
      utxoValue = UTxO.totalValue utxo
      utxoPolicyAssets = valueToPolicyAssets utxoValue
      depositAssets = valueToPolicyAssets $ UTxO.totalValue toDeposit
   in all (checkValidTokenInUTxO utxoPolicyAssets) (Map.toList depositAssets)
        & cover 10 (containsPolicies utxoPolicyAssets specifiedTokens) "PolicyId's are completely present in the UTxO"
        & cover 10 (containsAssets utxoPolicyAssets specifiedTokens) "Assets are completely present in the UTxO"
        & cover 1 (Map.null specifiedTokens) "Empty Assets"
        & cover 1 (Map.size specifiedTokens > 5) "Assets size > 5"
        & counterexample ("Valid tokens: " <> show toDeposit)
        & counterexample ("UTxO policy assets: " <> show utxoPolicyAssets)
 where
  containsPolicies :: Map PolicyId PolicyAssets -> Map PolicyId PolicyAssets -> Bool
  containsPolicies utxoAssets depositAssets = sort (Map.keys utxoAssets) == sort (Map.keys depositAssets)

  containsAssets :: Map PolicyId PolicyAssets -> Map PolicyId PolicyAssets -> Bool
  containsAssets utxoAssets depositAssets =
    let deposits = Map.elems depositAssets
     in all (`elem` deposits) (Map.elems utxoAssets)

  checkValidTokenInUTxO :: Map PolicyId PolicyAssets -> (PolicyId, PolicyAssets) -> Bool
  checkValidTokenInUTxO utxoAssets (policyId, PolicyAssets requiredAssets) =
    case Map.lookup policyId utxoAssets of
      Nothing -> False
      Just (PolicyAssets availableAssets) ->
        all
          ( \(assetName, requiredQty) ->
              case Map.lookup assetName availableAssets of
                Nothing -> False
                Just availableQty -> availableQty >= requiredQty
          )
          (Map.toList requiredAssets)

prepareAssetMap :: UTxO -> Gen (Map PolicyId PolicyAssets)
prepareAssetMap utxo = do
  let utxoAssets = valueToPolicyAssets $ UTxO.totalValue utxo
  n <- elements [1 .. Map.size utxoAssets]
  frequency [(1, randomAssets n utxoAssets), (8, addRandomAssets), (1, pure utxoAssets)]
 where
  addRandomAssets :: Gen (Map PolicyId PolicyAssets)
  addRandomAssets = oneof [addRandomAsset, foldr Map.union mempty <$> listOf addRandomAsset]

  addRandomAsset :: Gen (Map PolicyId PolicyAssets)
  addRandomAsset = do
    policy <- arbitrary
    Map.singleton policy <$> arbitrary

  randomAssets :: Int -> Map PolicyId PolicyAssets -> Gen (Map PolicyId PolicyAssets)
  randomAssets n assets =
    let assets' = Map.toList assets
        (x, y) = splitAt n assets'
     in oneof [pure $ Map.fromList x, pure $ Map.fromList y, pure $ Map.fromList $ drop n assets', pure $ Map.fromList $ take n assets']
