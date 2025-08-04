module Hydra.Tx.DepositSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Set qualified as Set
import Hydra.Cardano.Api (Coin (..), UTxO, selectLovelace, txOutValue)
import Hydra.Tx.Deposit (capUTxO)
import Test.Hydra.Tx.Gen (genUTxOSized)
import Test.QuickCheck (Property, counterexample, (===), (==>))

spec :: Spec
spec =
  parallel $ do
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
          let totalValue = UTxO.totalValue utxo
          let target = toInteger (selectLovelace totalValue) `div` 2
          let (selected, leftovers) = capUTxO utxo (Coin target)

          toInteger (selectLovelace (UTxO.totalValue selected)) `shouldSatisfy` \v -> v <= target

          UTxO.totalValue selected <> UTxO.totalValue leftovers `shouldBe` totalValue

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

          let target = Coin $ toInteger (selectLovelace (UTxO.totalValue smallOutput)) + 1000
          let (selected, _) = capUTxO mixedUTxO target

          length (UTxO.toList selected) `shouldSatisfy` (> 1)

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
      inputTotal = UTxO.totalValue utxo
      selectedTotal = UTxO.totalValue selected
      leftoverTotal = UTxO.totalValue leftovers
      inputLovelace = selectLovelace inputTotal
      selectedLovelace = selectLovelace selectedTotal
      leftoverLovelace = selectLovelace leftoverTotal
   in selectedLovelace + leftoverLovelace === inputLovelace
        & counterexample ("Input total: " <> show inputTotal)
        & counterexample ("Selected total: " <> show selectedTotal)
        & counterexample ("Leftover total: " <> show leftoverTotal)

-- | Property: Selected value never exceeds the target
propSelectedValueNeverExceedsTarget :: UTxO -> Coin -> Property
propSelectedValueNeverExceedsTarget utxo target =
  let (selected, _) = capUTxO utxo target
      selectedTotal = UTxO.totalValue selected
   in selectLovelace selectedTotal <= target
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
      selectedTotal = UTxO.totalValue selected
      inputTotal = UTxO.totalValue utxo
   in (selectLovelace inputTotal >= target) ==>
        ( selectLovelace selectedTotal == target
            || selectLovelace selectedTotal == target - 1
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
        total1 = UTxO.totalValue selected1
        total2 = UTxO.totalValue selected2
     in selectLovelace total1 <= selectLovelace total2
          & counterexample ("Target1: " <> show target1 <> ", Selected1: " <> show total1)
          & counterexample ("Target2: " <> show target2 <> ", Selected2: " <> show total2)

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
