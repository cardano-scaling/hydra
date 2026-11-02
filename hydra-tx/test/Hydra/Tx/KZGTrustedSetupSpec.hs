module Hydra.Tx.KZGTrustedSetupSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (Point1, Point2, blsCompress, blsGenerator, blsInGroup, blsUncompress)
import Cardano.Crypto.Hash (Blake2b_224)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Data.ByteString.Base16 qualified as Base16
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Contract.CRS (checkMembershipPairing)
import Hydra.Contract.KZGTrustedSetup (canonicalG2Points, deployedFanoutBatchSize, g1Points, g2BuiltinPoints, g2Points, maxAccumulatorSize, maxFanoutBatchSize)
import Hydra.Tx.Accumulator (build, createMembershipProof, crsG1Points, getAccumulatorCommitment, requiredCRSPointCount)
import Plutus.Crypto.BlsUtils qualified as Bls
import PlutusTx.Builtins (
  bls12_381_G1_scalarMul,
  bls12_381_G1_uncompress,
  bls12_381_G2_uncompress,
  bls12_381_finalVerify,
  bls12_381_millerLoop,
  byteStringToInteger,
  toBuiltin,
 )

spec :: Spec
spec = parallel $ do
  -- Extract the setup values once; a Left here means the embedded binary is
  -- corrupted or tampered with, which should crash the test suite loudly.
  let g1Pts = either (error . show) id g1Points
      g2Pts = either (error . show) id g2Points
      g2BuiltinPts = either (error . show) id g2BuiltinPoints

  describe "KZGTrustedSetup (Ethereum 32768-point)" $ do
    it "g1Points has 32768 entries" $
      length g1Pts `shouldBe` 32768

    it "g2Points has 65 entries" $
      length g2Pts `shouldBe` 65

    it "maxAccumulatorSize is 32767 (Ethereum 32768-point provides 32768 G1 points)" $
      maxAccumulatorSize `shouldBe` 32767

    it "maxAccumulatorSize matches the parsed G1 point count minus one" $
      maxAccumulatorSize `shouldBe` length g1Pts - 1

    it "maxFanoutBatchSize is 64 (Ethereum 32768-point provides 65 G2 points)" $
      maxFanoutBatchSize `shouldBe` 64

    it "maxFanoutBatchSize matches the parsed G2 point count minus one" $
      maxFanoutBatchSize `shouldBe` length g2Pts - 1

    -- The deployed cap, as opposed to the ceiling above. The node bounds every
    -- fanout search by this ('Hydra.Chain.Direct.Handlers'), so pin it against
    -- 'checkMembershipPairing' and the CRS the validator is compiled against
    -- rather than restating @defaultItems - 1@: the cap is only right if the
    -- canonical CRS verifies exactly this many outputs and no more.
    it "deployedFanoutBatchSize is exactly what the canonical CRS verifies" $ do
      let elements = [fromString (show i) | i <- [1 .. deployedFanoutBatchSize + 1]] :: [ByteString]
          fullAcc = build elements
          commitment = getAccumulatorCommitment fullAcc
          proofFor subset =
            bls12_381_G1_uncompress . toBuiltin . either error id $
              createMembershipProof subset fullAcc (crsG1Points $ requiredCRSPointCount fullAcc)
          atCap = take deployedFanoutBatchSize elements
      checkMembershipPairing commitment (proofFor atCap) canonicalG2Points (map toInt atCap)
        `shouldBe` True
      checkMembershipPairing commitment (proofFor elements) canonicalG2Points (map toInt elements)
        `shouldBe` False

    it "first G1 point matches the BLS12-381 G1 generator (τ^0·G1 = G1, confirming monomial form)" $
      case g1Pts of
        [] -> expectationFailure "g1Points is empty"
        (p : _) -> blsCompress p `shouldBe` blsCompress (blsGenerator :: Point1)

    it "first G2 point matches the BLS12-381 G2 generator (τ^0·G2 = G2)" $
      case g2Pts of
        [] -> expectationFailure "g2Points is empty"
        (p : _) -> blsCompress p `shouldBe` blsCompress (blsGenerator :: Point2)

    it "all G2 points are in the prime-order G2 subgroup" $
      all blsInGroup g2Pts `shouldBe` True

    it "all G1 points are in the prime-order G1 subgroup" $
      all blsInGroup g1Pts `shouldBe` True

    it "G1 points round-trip through compression" $
      case g1Pts of
        [] -> expectationFailure "g1Points is empty"
        (p : _) -> case blsUncompress (blsCompress p) of
          Left err -> expectationFailure $ "G1 decompression failed: " <> show err
          Right p' -> blsCompress (p' :: Point1) `shouldBe` blsCompress p

    it "G2 points round-trip through compression" $
      case g2Pts of
        [] -> expectationFailure "g2Points is empty"
        (p : _) -> case blsUncompress (blsCompress p) of
          Left err -> expectationFailure $ "G2 decompression failed: " <> show err
          Right p' -> blsCompress (p' :: Point2) `shouldBe` blsCompress p

  describe "End-to-end pairing check" $ do
    it "membership proof satisfies e(commitment_G1, G2) = e(proof_G1, P_S(τ)·G2)" $ do
      let allElements = ["alpha", "beta", "gamma", "delta", "epsilon"] :: [ByteString]
          fullAcc = build allElements
          subsetElements = ["beta", "gamma"] :: [ByteString]
          crsSize = requiredCRSPointCount fullAcc
          crsG2 = take crsSize g2BuiltinPts
      proofBytes <- either error pure $ createMembershipProof subsetElements fullAcc (crsG1Points crsSize)
      let proof = bls12_381_G1_uncompress (toBuiltin proofBytes)
          ints = map toInt subsetElements
      checkMembershipPairing (getAccumulatorCommitment fullAcc) proof crsG2 ints
        `shouldBe` True

    it "proof fails when subset element is not in accumulator" $ do
      let allElements = ["alpha", "beta", "gamma"] :: [ByteString]
          fullAcc = build allElements
          foreignElement = ["omega"] :: [ByteString]
          crsSize = requiredCRSPointCount fullAcc
      createMembershipProof foreignElement fullAcc (crsG1Points crsSize)
        `shouldSatisfy` isLeft

    it "proof for one subset does not verify for a different subset" $ do
      let allElements = ["alpha", "beta", "gamma", "delta"] :: [ByteString]
          fullAcc = build allElements
          subsetA = ["alpha", "beta"] :: [ByteString]
          subsetB = ["gamma", "delta"] :: [ByteString]
          crsSize = requiredCRSPointCount fullAcc
          crsG2 = take crsSize g2BuiltinPts
          ints = map toInt subsetB
      proofBytes <- either error pure $ createMembershipProof subsetA fullAcc (crsG1Points crsSize)
      let proof = bls12_381_G1_uncompress (toBuiltin proofBytes)
      checkMembershipPairing (getAccumulatorCommitment fullAcc) proof crsG2 ints
        `shouldBe` False

  describe "32768-point migration" $ do
    forM_ [4096, maxAccumulatorSize] $ \n ->
      it ("verifies a full fanout batch from " <> show n <> " elements") $ do
        let elements = show <$> [1 .. n] :: [ByteString]
            acc = build elements
            subset = take deployedFanoutBatchSize elements
        proofBytes <- either error pure $ createMembershipProof subset acc (crsG1Points $ n + 1)
        let proof = bls12_381_G1_uncompress (toBuiltin proofBytes)
        checkMembershipPairing (getAccumulatorCommitment acc) proof canonicalG2Points (map toInt subset)
          `shouldBe` True
        checkMembershipPairing (getAccumulatorCommitment acc) proof canonicalG2Points (map toInt ("foreign" : drop 1 subset))
          `shouldBe` False

    it "rejects a new-setup proof against the old EIP-4844 G2 powers" $ do
      let acc = build ["alpha", "beta"]
          subset = ["alpha"]
          -- First two G2 points from Ethereum's final 4096-point setup.
          oldG2 =
            map
              (bls12_381_G2_uncompress . toBuiltin . either (error . toText) id . Base16.decode)
              [ "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"
              , "b5bfd7dd8cdeb128843bc287230af38926187075cbfbefa81009a2ce615ac53d2914e5870cb452d2afaaab24f3499f72185cbfee53492714734429b7b38608e23926c911cceceac9a36851477ba4c60b087041de621000edc98edada20c1def2"
              ]
      proofBytes <- either error pure $ createMembershipProof subset acc (crsG1Points 3)
      let proof = bls12_381_G1_uncompress (toBuiltin proofBytes)
      checkMembershipPairing (getAccumulatorCommitment acc) proof canonicalG2Points (map toInt subset)
        `shouldBe` True
      checkMembershipPairing (getAccumulatorCommitment acc) proof oldG2 (map toInt subset)
        `shouldBe` False

  -- 'getG2Commitment' pairs polynomial coefficients with CRS points using
  -- 'zipWith', which silently drops whatever the CRS cannot cover. A subset of
  -- N elements yields N+1 coefficients, so it must satisfy N < length crsG2;
  -- 'checkMembershipPairing' rejects anything else rather than verify a
  -- truncated, lower-degree polynomial. On mainnet the CRS datum is pinned to
  -- the canonical 'defaultItems' = 30 points, which is what caps a fanout batch
  -- at 29 outputs.
  describe "CRS length guard" $ do
    let allElements = ["alpha", "beta", "gamma", "delta", "epsilon"] :: [ByteString]
        fullAcc = build allElements
        commitment = getAccumulatorCommitment fullAcc
        subsetElements = ["alpha", "beta", "gamma", "delta"] :: [ByteString]
        ints = map toInt subsetElements
        genuineProof =
          bls12_381_G1_uncompress . toBuiltin . either error id $
            createMembershipProof subsetElements fullAcc (crsG1Points $ requiredCRSPointCount fullAcc)

    it "accepts a subset that exactly fills the CRS (N + 1 == length crsG2)" $ do
      let crsG2 = take (length ints + 1) g2BuiltinPts
      checkMembershipPairing commitment genuineProof crsG2 ints
        `shouldBe` True

    it "rejects a subset one element past the CRS (N == length crsG2)" $ do
      -- Same subset and same genuine proof as above; only the CRS is one point
      -- shorter, which is exactly where the polynomial stops fitting.
      let crsG2 = take (length ints) g2BuiltinPts
      checkMembershipPairing commitment genuineProof crsG2 ints
        `shouldBe` False

    it "rejects an empty CRS" $
      checkMembershipPairing commitment genuineProof [] ints
        `shouldBe` False

    -- Truncation is not merely "verifies the wrong identity and fails anyway",
    -- it is forgeable. With a single CRS point the subset polynomial X + s
    -- truncates to the constant s, so the identity degenerates to
    -- e(A, G2) = e(proof, s·G2) — which proof = s⁻¹·A satisfies. That forgery
    -- needs nothing but the public commitment from the datum: no witness, no
    -- CRS secret, and s never added to the accumulator.
    it "rejects a forged proof that the truncated pairing accepts" $ do
      let crsG2 = take 1 g2BuiltinPts
          s = toInt "omega" -- never added to the accumulator
          forgedProof = bls12_381_G1_scalarMul (Bls.unScalar . Bls.recip $ Bls.mkScalar s) commitment
          truncatedPolyG2 = Bls.getG2Commitment crsG2 (Bls.getFinalPoly [Bls.mkScalar s])
      case crsG2 of
        [] -> expectationFailure "g2BuiltinPoints is empty"
        (g2 : _) -> do
          -- The truncated identity really does hold for the forged proof ...
          bls12_381_finalVerify
            (bls12_381_millerLoop commitment g2)
            (bls12_381_millerLoop forgedProof truncatedPolyG2)
            `shouldBe` True
          -- ... so the length guard is the only thing standing between the
          -- validator and accepting it.
          checkMembershipPairing commitment forgedProof crsG2 [s]
            `shouldBe` False

toInt :: ByteString -> Integer
toInt e = byteStringToInteger BigEndian . toBuiltin $ digest (Proxy @Blake2b_224) e
