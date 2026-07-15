module Hydra.Tx.AccumulatorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.EllipticCurve.BLS12_381.Internal (blsCompress)
import Cardano.Crypto.Hash (Blake2b_224)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api (Tx, UTxO)
import Hydra.Contract.CRS (checkMembershipPairing)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.KZGTrustedSetup (g2BuiltinPoints)
import Hydra.Tx.Accumulator (
  buildFromUTxO,
  createMembershipProofFromUTxO,
  crsG1Points,
  crsG2Points,
  defaultItems,
  getAccumulatorCommitment,
  requiredCRSPointCount,
 )
import Hydra.Tx.IsTx (IsTx (outputsOfUTxO, utxoToElement))
import PlutusTx.Builtins (bls12_381_G1_uncompress, bls12_381_G2_uncompress, byteStringToInteger, toBuiltin)
import Test.Hydra.Tx.Gen (genUTxOWithSimplifiedAddresses)
import Test.QuickCheck (counterexample, forAll, property, resize, sublistOf, suchThat, (===))

spec :: Spec
spec = parallel $ do
  let g2BuiltinPts = either (error . show) id g2BuiltinPoints

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

utxoScalars :: UTxO -> [Integer]
utxoScalars utxo = toInt <$> filter (/= mempty) (utxoToElement @Tx <$> outputsOfUTxO @Tx utxo)

toInt :: ByteString -> Integer
toInt e = byteStringToInteger BigEndian . toBuiltin $ digest (Proxy @Blake2b_224) e
