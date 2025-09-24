module Hydra.Tx.HeadIdSpec where

import Hydra.Prelude
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)

import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Tx.HeadId (currencySymbolToHeadId, headIdToCurrencySymbol, headIdToPolicyId, headSeedToTxIn, mkHeadId, txInToHeadSeed)
import Test.QuickCheck (counterexample, (===))
import Test.QuickCheck.Monadic (monadicIO)

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
