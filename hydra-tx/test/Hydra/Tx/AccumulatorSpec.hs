module Hydra.Tx.AccumulatorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Accumulator qualified
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (blsCompress)
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api (Tx, UTxO)
import Hydra.Contract.CRS (checkMembershipPairing)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.KZGTrustedSetup (g1BuiltinPoints, g2BuiltinPoints)
import Hydra.Tx.Accumulator (
  applyUTxODelta,
  build,
  buildFromUTxO,
  computeG1CommitmentBytes,
  createMembershipProofFromUTxO,
  crsG1Points,
  crsG2Points,
  defaultItems,
  getAccumulatorCommitment,
  getAccumulatorHash,
  requiredCRSPointCount,
  unHydraAccumulator,
 )
import Hydra.Tx.IsTx (IsTx (outputsOfUTxO, utxoToElement))
import Plutus.Crypto.BlsUtils (getFinalPoly, getG1Commitment, mkScalar)
import PlutusTx.Builtins (bls12_381_G1_compress, bls12_381_G1_uncompress, bls12_381_G2_uncompress, byteStringToInteger, fromBuiltin, toBuiltin)
import Test.Hydra.Tx.Gen (genTxOutAdaOnly, genUTxOWithSimplifiedAddresses)
import Test.QuickCheck (counterexample, forAll, property, resize, sublistOf, suchThat, (.&&.), (===), (==>))

spec :: Spec
spec = parallel $ do
  let g2BuiltinPts = either (error . show) id g2BuiltinPoints

  -- The commitment is computed through the rust-accumulator FFI; these
  -- properties pin it bit-for-bit against the PlutusTx path the codebase used
  -- before, kept below as 'referenceCommitmentBytes'.
  describe "G1 commitment" $ do
    prop "rust FFI commitment equals the PlutusTx reference (UTxO sets)" $
      forAll (resize 30 genUTxOWithSimplifiedAddresses) $ \utxo ->
        let acc = unHydraAccumulator $ buildFromUTxO @Tx utxo
         in computeG1CommitmentBytes acc === referenceCommitmentBytes acc

    prop "rust FFI commitment equals the PlutusTx reference (raw elements incl. duplicates and empty)" $
      forAll (resize 64 arbitrary) $ \(ints :: [Int]) ->
        let els = show <$> ints
            acc = unHydraAccumulator $ build (els <> take 3 els <> [""])
         in computeG1CommitmentBytes acc === referenceCommitmentBytes acc

    prop "applyUTxODelta equals a fresh build on the new set" $
      forAll (resize 20 genUTxOWithSimplifiedAddresses) $ \base ->
        forAll (sublistOf (UTxO.toList base)) $ \dropped ->
          forAll (resize 10 genUTxOWithSimplifiedAddresses) $ \grown ->
            let prevU = base
                nextU = UTxO.difference base (UTxO.fromList dropped) <> grown
                delta = applyUTxODelta @Tx (buildFromUTxO @Tx prevU) prevU nextU
                fresh = buildFromUTxO @Tx nextU
             in (unHydraAccumulator delta === unHydraAccumulator fresh)
                  .&&. (getAccumulatorHash delta === getAccumulatorHash fresh)

    prop "applyUTxODelta decrements duplicated outputs correctly" $
      forAll arbitrary $ \(txIn1, txIn2) ->
        txIn1 /= txIn2 ==>
          forAll (genTxOutAdaOnly =<< arbitrary) $ \txOut ->
            -- Two inputs carrying byte-identical outputs collapse to one
            -- accumulator element with count 2; consuming one of them must
            -- decrement, not delete.
            let prevU = UTxO.fromList [(txIn1, txOut), (txIn2, txOut)]
                nextU = UTxO.fromList [(txIn2, txOut)]
                delta = applyUTxODelta @Tx (buildFromUTxO @Tx prevU) prevU nextU
             in unHydraAccumulator delta === unHydraAccumulator (buildFromUTxO @Tx nextU)

    prop "accumulator hash is the blake2b of the commitment in the datum" $
      -- Off-chain mirror of the on-chain mustMatchAccumulatorCommitmentHash
      -- check: the signed hash must bind to the G1 commitment stored in
      -- close/contest datums, whatever caching sits between them.
      forAll (resize 10 genUTxOWithSimplifiedAddresses) $ \utxo ->
        let acc = buildFromUTxO @Tx utxo
         in getAccumulatorHash acc
              === digest (Proxy @Blake2b_256) (fromBuiltin (bls12_381_G1_compress (getAccumulatorCommitment acc)))

    it "empty accumulator commits to the G1 generator" $
      case crsG1Points 1 of
        [g1] -> computeG1CommitmentBytes (unHydraAccumulator $ build []) `shouldBe` blsCompress g1
        _ -> expectationFailure "expected exactly one CRS point"

    it "oversized accumulator errors when the commitment is forced" $ do
      let acc = unHydraAccumulator $ build (show <$> [1 .. (4096 :: Int)])
      (pure $! computeG1CommitmentBytes acc) `shouldThrow` anyErrorCall

    describe "golden commitments (recorded from the PlutusTx path before the FFI swap)" $
      forM_ goldenCases $ \(caseName, els, expectedHex) ->
        it ("matches golden for " <> caseName) $ do
          let acc = unHydraAccumulator $ build els
          Base16.encode (computeG1CommitmentBytes acc) `shouldBe` expectedHex

  -- checkMembershipPairing is the on-chain pairing check run by the partial
  -- fanout validator to confirm a batch of UTxOs belongs to the full snapshot.
  describe "UTxO membership proofs" $ do
    prop "membership proof for a UTxO subset satisfies the KZG pairing check" $
      forAll (resize 5 genUTxOWithSimplifiedAddresses `suchThat` (not . null . UTxO.toList)) $ \fullUTxO ->
        forAll (sublistOf (UTxO.toList fullUTxO) `suchThat` (not . null)) $ \subsetList ->
          let subsetUTxO = UTxO.fromList subsetList
              fullAcc = buildFromUTxO @Tx fullUTxO
              crsSize = requiredCRSPointCount fullAcc
              crsG2 = take crsSize g2BuiltinPts
           in case createMembershipProofFromUTxO @Tx subsetUTxO fullAcc (crsG1Points crsSize) of
                Left err ->
                  property False
                    & counterexample ("createMembershipProofFromUTxO failed: " <> toString err)
                Right proofBytes ->
                  let proof = bls12_381_G1_uncompress (toBuiltin proofBytes)
                   in checkMembershipPairing (getAccumulatorCommitment fullAcc) proof crsG2 (utxoScalars subsetUTxO)
                        === True

    prop "membership proof for one UTxO subset does not verify for a different subset" $
      forAll (resize 5 genUTxOWithSimplifiedAddresses `suchThat` (\u -> length (UTxO.toList u) >= 2)) $ \fullUTxO ->
        let pairs = UTxO.toList fullUTxO
            half = length pairs `div` 2
            subsetA = UTxO.fromList $ take half pairs
            subsetB = UTxO.fromList $ drop half pairs
            fullAcc = buildFromUTxO @Tx fullUTxO
            crsSize = requiredCRSPointCount fullAcc
            crsG2 = take crsSize g2BuiltinPts
         in case createMembershipProofFromUTxO @Tx subsetA fullAcc (crsG1Points crsSize) of
              Left err ->
                property False
                  & counterexample ("createMembershipProofFromUTxO failed: " <> toString err)
              Right proofBytes ->
                let proof = bls12_381_G1_uncompress (toBuiltin proofBytes)
                 in checkMembershipPairing (getAccumulatorCommitment fullAcc) proof crsG2 (utxoScalars subsetB)
                      === False

  -- Ties the off-chain publisher path (createCRSG2Datum via crsG2Points defaultItems) to
  -- the on-chain validator's canonical CRS points, so a fanout accepts exactly the CRS
  -- datum the script registry publishes and nothing else.
  describe "canonical CRS datum" $
    it "on-chain canonical hash matches the published CRS datum" $
      let publishedCRSDatum =
            bls12_381_G2_uncompress . toBuiltin . blsCompress <$> crsG2Points defaultItems
       in Head.canonicalCRSDatumHash `shouldBe` Head.hashCRSDatum publishedCRSDatum

-- | The PlutusTx commitment implementation this codebase used before
-- switching to the rust FFI, kept verbatim as an equivalence oracle.
referenceCommitmentBytes :: Accumulator.Accumulator -> ByteString
referenceCommitmentBytes acc =
  let expandedElems = concatMap (\(h, count) -> replicate count h) $ Map.elems acc
      n = length expandedElems
      crsG1 = take (n + 1) $ either (error . show) id g1BuiltinPoints
   in fromBuiltin . bls12_381_G1_compress $
        getG1Commitment crsG1 . getFinalPoly . map (mkScalar . byteStringToInteger BigEndian . toBuiltin) $
          expandedElems

-- | Deterministic element sets with expected commitment (compressed G1, hex),
-- recorded from the PlutusTx path before the FFI swap. These also guard
-- future haskell-accumulator / rust-accumulator / plutus-accumulator bumps
-- against silent behavior changes.
goldenCases :: [(String, [ByteString], ByteString)]
goldenCases =
  [ ("0 elements", goldenElements 0, "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb")
  , ("1 element", goldenElements 1, "82d3e038e43f9ee73805cc8a6078365b8140a4012fa92643c2b3732d51cecf1fe0a1928d62fbf39878763549951bdb5a")
  , ("3 elements with a duplicate", goldenElements 2 <> take 1 (goldenElements 1), "91211024a7d57d2648b42394f87d2740fe5ebea49ef943aae048acf5a1b59c8bbd783801d9418af9a37e4673f3f5f1fe")
  , ("100 elements", goldenElements 100, "ac19336fad62fc5bfb9ec509bd13ecae9c8eabd4f1a5af163a0ecb71684f3f12933bac6180c52727dcf5bd44a1c2890f")
  , ("1000 elements", goldenElements 1000, "a49e01e30e499a14b161e7fab22b80d8a49d787cd80d0298b6a445b69f506003d3ad764704de29fc9ea22a5f7de7afe9")
  ]

goldenElements :: Int -> [ByteString]
goldenElements n = ["element-" <> show i | i <- [1 .. n]]

utxoScalars :: UTxO -> [Integer]
utxoScalars utxo = toInt <$> filter (/= mempty) (utxoToElement @Tx <$> outputsOfUTxO @Tx utxo)

toInt :: ByteString -> Integer
toInt e = byteStringToInteger BigEndian . toBuiltin $ digest (Proxy @Blake2b_224) e
