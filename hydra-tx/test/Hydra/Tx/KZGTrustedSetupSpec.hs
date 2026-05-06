module Hydra.Tx.KZGTrustedSetupSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress, blsGenerator, blsInGroup, blsUncompress)
import Cardano.Crypto.Hash (Blake2b_224)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Contract.CRS (checkMembershipPairing)
import Hydra.Tx.Accumulator (build, createMembershipProof, crsG2Points, getAccumulatorCommitment, requiredCRSPointCount)
import Hydra.Tx.KZGTrustedSetup (g1BuiltinPoints, g1Points, g2Points, maxAccumulatorSize, maxFanoutBatchSize)
import PlutusTx.Builtins (bls12_381_G2_uncompress, byteStringToInteger, toBuiltin)

spec :: Spec
spec = parallel $ do
  describe "KZGTrustedSetup (EIP-4844)" $ do
    it "g1Points has 4096 entries" $
      length g1Points `shouldBe` 4096

    it "g2Points has 65 entries" $
      length g2Points `shouldBe` 65

    it "maxAccumulatorSize is 64 (EIP-4844 provides 65 G2 points)" $
      maxAccumulatorSize `shouldBe` 64

    it "maxAccumulatorSize matches the parsed G2 point count minus one" $
      maxAccumulatorSize `shouldBe` length g2Points - 1

    it "maxFanoutBatchSize is 4095 (EIP-4844 provides 4096 G1 points)" $
      maxFanoutBatchSize `shouldBe` 4095

    it "maxFanoutBatchSize matches the parsed G1 point count minus one" $
      maxFanoutBatchSize `shouldBe` length g1Points - 1

    it "first G1 point matches the BLS12-381 G1 generator (τ^0·G1 = G1, confirming monomial form)" $
      case g1Points of
        [] -> expectationFailure "g1Points is empty"
        (p : _) -> blsCompress p `shouldBe` blsCompress (blsGenerator :: Point1)

    it "first G2 point matches the BLS12-381 G2 generator (τ^0·G2 = G2)" $
      case g2Points of
        [] -> expectationFailure "g2Points is empty"
        (p : _) -> blsCompress p `shouldBe` blsCompress (blsGenerator :: Point2)

    it "all G2 points are in the prime-order G2 subgroup" $
      all blsInGroup g2Points `shouldBe` True

    it "first 3 G1 points are in the prime-order G1 subgroup" $
      all blsInGroup (take 3 g1Points) `shouldBe` True

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

  describe "End-to-end pairing check" $ do
    it "membership proof satisfies e(S(τ)·G1, proof) = e(G1, acc)" $ do
      let allElements = ["alpha", "beta", "gamma", "delta", "epsilon"] :: [ByteString]
          fullAcc = build allElements
          subsetElements = ["beta", "gamma"] :: [ByteString]
          crsSize = requiredCRSPointCount fullAcc
          crsG1 = take crsSize g1BuiltinPoints
          proofBytes = createMembershipProof subsetElements fullAcc (crsG2Points crsSize)
          proof = bls12_381_G2_uncompress (toBuiltin proofBytes)
          ints = map toInt subsetElements
      checkMembershipPairing (getAccumulatorCommitment fullAcc) proof crsG1 ints
        `shouldBe` True

    it "proof for one subset does not verify for a different subset" $ do
      let allElements = ["alpha", "beta", "gamma", "delta"] :: [ByteString]
          fullAcc = build allElements
          subsetA = ["alpha", "beta"] :: [ByteString]
          subsetB = ["gamma", "delta"] :: [ByteString]
          crsSize = requiredCRSPointCount fullAcc
          crsG1 = take crsSize g1BuiltinPoints
          proofBytes = createMembershipProof subsetA fullAcc (crsG2Points crsSize)
          proof = bls12_381_G2_uncompress (toBuiltin proofBytes)
          ints = map toInt subsetB
      checkMembershipPairing (getAccumulatorCommitment fullAcc) proof crsG1 ints
        `shouldBe` False

toInt :: ByteString -> Integer
toInt e = byteStringToInteger BigEndian . toBuiltin $ digest (Proxy @Blake2b_224) e
