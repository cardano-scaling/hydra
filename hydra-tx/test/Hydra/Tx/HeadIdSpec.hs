module Hydra.Tx.HeadIdSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (counterexample, (===))
import "QuickCheck" Test.QuickCheck.Monadic (monadicIO)
import "hydra-cardano-api" Hydra.Cardano.Api.Gen ()
import "hydra-plutus" Hydra.Contract.HeadTokens (headPolicyId)
import "hydra-tx" Hydra.Tx.HeadId (currencySymbolToHeadId, headIdToCurrencySymbol, headIdToPolicyId, headSeedToTxIn, mkHeadId, txInToHeadSeed)

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
