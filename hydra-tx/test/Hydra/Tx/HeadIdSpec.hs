module Hydra.Tx.HeadIdSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Test.QuickCheck.Monadic (monadicIO)
import Hydra.Tx.HeadId (headSeedToTxIn, txInToHeadSeed, mkHeadId, headIdToPolicyId, headIdToCurrencySymbol, currencySymbolToHeadId)
import Test.QuickCheck ((===), counterexample)
import Hydra.Contract.HeadTokens (headPolicyId)

spec :: Spec
spec =
  parallel $ do
    describe "HeadSeed (cardano)" $
      prop "headSeedToTxIn . txInToHeadSeed === id" $ \txIn -> do
        let headSeed = txInToHeadSeed txIn
        headSeedToTxIn headSeed === Just txIn
          & counterexample (show headSeed)

    describe "HeadId (cardano)" $ do
      prop "headIdToPolicyId . mkHeadId === id" $ \pid -> do
        let headId = mkHeadId pid
        headIdToPolicyId headId === Just pid
          & counterexample (show headId)

      prop "curencySymbolToHeadId . headIdToCurrencySymbol === id" $ \txIn -> monadicIO $ do
        let headId = mkHeadId $ headPolicyId txIn
        let cs = headIdToCurrencySymbol headId
        headId' <- currencySymbolToHeadId cs
        pure $ headId' === headId
