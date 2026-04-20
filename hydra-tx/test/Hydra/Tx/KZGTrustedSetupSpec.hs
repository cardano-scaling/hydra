module Hydra.Tx.KZGTrustedSetupSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress, blsGenerator, blsInGroup, blsUncompress)
import Hydra.Tx.KZGTrustedSetup (g1Points, g2Points, maxAccumulatorSize)

spec :: Spec
spec = parallel $ do
  describe "KZGTrustedSetup (EIP-4844)" $ do
    it "g1Points has 65 entries" $
      length g1Points `shouldBe` 65

    it "g2Points has 65 entries" $
      length g2Points `shouldBe` 65

    it "maxAccumulatorSize is one less than the number of G2 points" $
      maxAccumulatorSize `shouldBe` length g2Points - 1

    it "first G2 point matches the BLS12-381 G2 generator (τ^0·G2 = G2)" $
      case g2Points of
        [] -> expectationFailure "g2Points is empty"
        (p : _) -> blsCompress p `shouldBe` blsCompress (blsGenerator :: Point2)

    it "all G1 points are in the prime-order G1 subgroup" $
      all blsInGroup g1Points `shouldBe` True

    it "all G2 points are in the prime-order G2 subgroup" $
      all blsInGroup g2Points `shouldBe` True

    it "G1 points round-trip through compression" $
      case g1Points of
        [] -> expectationFailure "g1Points is empty"
        (p : _) -> case blsUncompress (blsCompress p) of
          Left err -> expectationFailure $ "G1 decompression failed: " <> show err
          Right p' -> blsCompress (p' :: Point1) `shouldBe` blsCompress p

    it "G2 points round-trip through compression" $
      case g2Points of
        [] -> expectationFailure "g2Points is empty"
        (p : _) -> case blsUncompress (blsCompress p) of
          Left err -> expectationFailure $ "G2 decompression failed: " <> show err
          Right p' -> blsCompress (p' :: Point2) `shouldBe` blsCompress p
