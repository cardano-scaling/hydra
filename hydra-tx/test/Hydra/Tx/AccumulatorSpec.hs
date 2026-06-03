module Hydra.Tx.AccumulatorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.Hash (Blake2b_224)
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import GHC.ByteOrder (ByteOrder (BigEndian))
import Hydra.Cardano.Api (Tx, UTxO)
import Hydra.Contract.CRS (checkMembershipPairing)
import Hydra.Tx.Accumulator (
  buildFromUTxO,
  createMembershipProofFromUTxO,
  crsG1Points,
  getAccumulatorCommitment,
  requiredCRSPointCount,
 )
import Hydra.Tx.IsTx (IsTx (outputsOfUTxO, utxoToElement))
import Hydra.Tx.KZGTrustedSetup (g2BuiltinPoints)
import PlutusTx.Builtins (bls12_381_G1_uncompress, byteStringToInteger, toBuiltin)
import Test.Hydra.Tx.Gen (genUTxOWithSimplifiedAddresses)
import Test.QuickCheck (counterexample, forAll, property, resize, sublistOf, suchThat, (===))

spec :: Spec
spec = parallel $ do
  let g2BuiltinPts = either (error . show) id g2BuiltinPoints

  describe "UTxO membership proofs" $ do
    -- BLS pairing is expensive; keep UTxO sets small to avoid slow tests.
    prop "membership proof satisfies pairing check for a UTxO subset" $
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

    -- BLS pairing is expensive; keep UTxO sets small to avoid slow tests.
    prop "proof for one subset does not verify for a disjoint subset" $
      forAll (resize 10 genUTxOWithSimplifiedAddresses `suchThat` (\u -> length (UTxO.toList u) >= 2)) $ \fullUTxO ->
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

utxoScalars :: UTxO -> [Integer]
utxoScalars utxo = toInt <$> filter (/= mempty) (utxoToElement @Tx <$> outputsOfUTxO @Tx utxo)

toInt :: ByteString -> Integer
toInt e = byteStringToInteger BigEndian . toBuiltin $ digest (Proxy @Blake2b_224) e
