{-# LANGUAGE DuplicateRecordFields #-}

-- | Benchmark suite for the BLS accumulator implementation.
--
-- This suite measures the performance of accumulator operations with realistic
-- UTxO sets to understand the performance implications of using accumulators
-- for snapshot signing and partial fanout.
module Main where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Codec.Serialise (serialise)
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Hydra.Cardano.Api
import Hydra.Tx.Accumulator (
  buildFromUTxO,
  createMembershipProof,
  createMembershipProofFromUTxO,
  generateCRS,
  getAccumulatorHash,
  unHydraAccumulator,
 )
import Hydra.Tx.IsTx (IsTx (..))
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck (generate)

main :: IO ()
main = do
  putTextLn "=== Accumulator Benchmark Suite ==="
  putTextLn "Generating test data..."

  -- NOTE: All UTxO sizes are capped at 64 because the EIP-4844 KZG trusted
  -- setup provides only 65 G2 points. Accumulators of n elements require n+1
  -- G2 CRS points for polynomial commitment, so the maximum is 64 elements.

  -- Generate UTxO sets of various sizes (all ≤ 64)
  utxo5 <- generateUTxO 5
  utxo10 <- generateUTxO 10
  utxo20 <- generateUTxO 20
  utxo40 <- generateUTxO 40
  utxo60 <- generateUTxO 60

  putTextLn "Generated UTxO sets: 5, 10, 20, 40, 60"

  -- Pre-build accumulators and force them
  let !acc5 = buildFromUTxO @Tx utxo5
      !acc10 = buildFromUTxO @Tx utxo10
      !acc20 = buildFromUTxO @Tx utxo20
      !acc40 = buildFromUTxO @Tx utxo40
      !acc60 = buildFromUTxO @Tx utxo60

  putTextLn "Pre-built accumulators"

  -- Pre-generate CRS of various sizes and force them (max 65)
  let !crs10 = generateCRS 10
      !crs20 = generateCRS 20
      !crs40 = generateCRS 40
      !crs61 = generateCRS 61

  putTextLn "Pre-generated CRS"

  -- Generate subsets for membership proofs
  let !subset2_from5 = generateSubset utxo5 2
  let !subset5_from10 = generateSubset utxo10 5
  let !subset10_from20 = generateSubset utxo20 10
  let !subset20_from40 = generateSubset utxo40 20
  let !subset30_from60 = generateSubset utxo60 30

  putTextLn "Generated subsets for membership proofs"

  -- Extract individual elements for low-level proof testing
  let !elements10 = toPairList @Tx utxo10
      !elements20 = toPairList @Tx utxo20
      !serialized10 = utxoToElement @Tx <$> elements10
      !serialized20 = utxoToElement @Tx <$> elements20

  putTextLn "Starting benchmarks..."
  putTextLn ""

  defaultMain
    [ bgroup
        "1. Build Accumulator from UTxO"
        [ bench "5 UTxOs" $ whnf (buildFromUTxO @Tx) utxo5
        , bench "10 UTxOs" $ whnf (buildFromUTxO @Tx) utxo10
        , bench "20 UTxOs" $ whnf (buildFromUTxO @Tx) utxo20
        , bench "40 UTxOs" $ whnf (buildFromUTxO @Tx) utxo40
        , bench "60 UTxOs" $ whnf (buildFromUTxO @Tx) utxo60
        ]
    , bgroup
        "2. UTxO to Elements Conversion"
        [ bench "Extract 10 TxOuts" $ whnf (toPairList @Tx) utxo10
        , bench "Extract 20 TxOuts" $ whnf (toPairList @Tx) utxo20
        , bench "Extract 60 TxOuts" $ whnf (toPairList @Tx) utxo60
        , bench "Serialize 10 TxOuts" $ whnf (fmap (utxoToElement @Tx)) elements10
        , bench "Serialize 20 TxOuts" $ whnf (fmap (utxoToElement @Tx)) elements20
        ]
    , bgroup
        "3. Create Membership Proofs"
        [ bench "2 from 5" $ nf (\s -> createMembershipProofFromUTxO @Tx s acc5 crs10) subset2_from5
        , bench "5 from 10" $ nf (\s -> createMembershipProofFromUTxO @Tx s acc10 crs10) subset5_from10
        , bench "10 from 20" $ nf (\s -> createMembershipProofFromUTxO @Tx s acc20 crs20) subset10_from20
        , bench "20 from 40" $ nf (\s -> createMembershipProofFromUTxO @Tx s acc40 crs40) subset20_from40
        , bench "30 from 60" $ nf (\s -> createMembershipProofFromUTxO @Tx s acc60 crs61) subset30_from60
        ]
    , bgroup
        "4. Create Membership Proofs (Low-level)"
        [ bench "5 elements from 10" $ nf (\s -> createMembershipProof s acc10 crs10) (take 5 serialized10)
        , bench "10 elements from 20" $ nf (\s -> createMembershipProof s acc20 crs20) (take 10 serialized20)
        , bench "15 elements from 20" $ nf (\s -> createMembershipProof s acc20 crs20) (take 15 serialized20)
        ]
    , bgroup
        "5. Accumulator Hashing"
        [ bench "Hash 5 UTxOs" $ nf getAccumulatorHash acc5
        , bench "Hash 10 UTxOs" $ nf getAccumulatorHash acc10
        , bench "Hash 40 UTxOs" $ nf getAccumulatorHash acc40
        , bench "Hash 60 UTxOs" $ nf getAccumulatorHash acc60
        ]
    , bgroup
        "6. Accumulator Serialization"
        [ bench "Serialize accumulator (10)" $ nf (serialise . unHydraAccumulator) acc10
        , bench "Serialize accumulator (20)" $ nf (serialise . unHydraAccumulator) acc20
        , bench "Serialize accumulator (60)" $ nf (serialise . unHydraAccumulator) acc60
        ]
    , bgroup
        "7. CRS Loading (from EIP-4844 trusted setup)"
        -- Point2 lacks NFData, so we force the full spine via length
        [ bench "CRS size 10" $ whnf (length . generateCRS) 10
        , bench "CRS size 20" $ whnf (length . generateCRS) 20
        , bench "CRS size 40" $ whnf (length . generateCRS) 40
        , bench "CRS size 65 (max)" $ whnf (length . generateCRS) 65
        ]
    , bgroup
        "8. End-to-End Snapshot Simulation"
        [ bench "Full cycle: 20 UTxOs" $ nf fullSnapshotCycle utxo20
        , bench "Full cycle: 60 UTxOs" $ nf fullSnapshotCycle utxo60
        , bench "Partial fanout: 10 from 20" $ nf (partialFanoutCycle utxo20) subset10_from20
        , bench "Partial fanout: 20 from 40" $ nf (partialFanoutCycle utxo40) subset20_from40
        ]
    ]

-- | Generate a UTxO set of specified size with realistic transaction outputs.
generateUTxO :: Int -> IO UTxO
generateUTxO n = generate $ genUTxOAdaOnlyOfSize n

-- | Generate a subset of a given UTxO.
-- This simulates selecting UTxOs for partial fanout.
generateSubset :: UTxO -> Int -> UTxO
generateSubset utxo n =
  let allPairs = UTxO.toList utxo
   in if n >= length allPairs
        then utxo
        else
          let subsetPairs = take n allPairs
           in UTxO.fromList subsetPairs

-- | Simulate the full snapshot creation cycle:
-- 1. Build accumulator from UTxO
-- 2. Hash the accumulator
-- 3. Serialize for signing
fullSnapshotCycle :: UTxO -> ByteString
fullSnapshotCycle utxo =
  let accumulator = buildFromUTxO @Tx utxo
   in getAccumulatorHash accumulator

-- | Simulate a partial fanout operation:
-- 1. Build accumulator from full UTxO
-- 2. Create membership proof for subset
-- 3. Return the proof
partialFanoutCycle :: UTxO -> UTxO -> ByteString
partialFanoutCycle fullUtxo subsetUtxo =
  let accumulator = buildFromUTxO @Tx fullUtxo
      crs = generateCRS (UTxO.size fullUtxo + 1)
   in createMembershipProofFromUTxO @Tx subsetUtxo accumulator crs
