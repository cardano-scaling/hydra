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
  defaultCRS,
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

  -- Generate UTxO sets of various sizes
  utxo10 <- generateUTxO 10
  utxo50 <- generateUTxO 50
  utxo100 <- generateUTxO 100
  utxo500 <- generateUTxO 500
  utxo1000 <- generateUTxO 1000
  utxo5000 <- generateUTxO 5000
  utxo10000 <- generateUTxO 10000

  putTextLn "Generated UTxO sets: 10, 50, 100, 500, 1000, 5000, 10000"

  -- Pre-build accumulators for membership proof tests
  let acc10 = buildFromUTxO @Tx utxo10
      acc50 = buildFromUTxO @Tx utxo50
      acc100 = buildFromUTxO @Tx utxo100
      acc500 = buildFromUTxO @Tx utxo500
      acc1000 = buildFromUTxO @Tx utxo1000
      acc5000 = buildFromUTxO @Tx utxo5000
      acc10000 = buildFromUTxO @Tx utxo10000

  putTextLn "Pre-built accumulators"

  -- Generate subsets for membership proofs
  -- Testing realistic scenarios: proving 10-20% of UTxOs
  let subset5_from50 = generateSubset utxo50 5
  let subset10_from100 = generateSubset utxo100 10
  let subset50_from500 = generateSubset utxo500 50
  let subset100_from1000 = generateSubset utxo1000 100
  let subset500_from5000 = generateSubset utxo5000 500
  let subset1000_from10000 = generateSubset utxo10000 1000

  putTextLn "Generated subsets for membership proofs"

  -- Extract individual elements for low-level proof testing
  let elements10 = toPairList @Tx utxo10
      elements100 = toPairList @Tx utxo100
      serialized10 = utxoToElement @Tx <$> elements10
      serialized100 = utxoToElement @Tx <$> elements100

  putTextLn "Starting benchmarks..."
  putTextLn ""

  defaultMain
    [ bgroup
        "1. Build Accumulator from UTxO"
        [ bench "10 UTxOs" $ whnf (buildFromUTxO @Tx) utxo10
        , bench "50 UTxOs" $ whnf (buildFromUTxO @Tx) utxo50
        , bench "100 UTxOs" $ whnf (buildFromUTxO @Tx) utxo100
        , bench "500 UTxOs" $ whnf (buildFromUTxO @Tx) utxo500
        , bench "1000 UTxOs" $ whnf (buildFromUTxO @Tx) utxo1000
        , bench "5000 UTxOs" $ whnf (buildFromUTxO @Tx) utxo5000
        , bench "10000 UTxOs" $ whnf (buildFromUTxO @Tx) utxo10000
        ]
    , bgroup
        "2. UTxO to Elements Conversion"
        [ bench "Extract 10 TxOuts" $ whnf (toPairList @Tx) utxo10
        , bench "Extract 100 TxOuts" $ whnf (toPairList @Tx) utxo100
        , bench "Extract 1000 TxOuts" $ whnf (toPairList @Tx) utxo1000
        , bench "Serialize 10 TxOuts" $ whnf (fmap (utxoToElement @Tx)) elements10
        , bench "Serialize 100 TxOuts" $ whnf (fmap (utxoToElement @Tx)) elements100
        ]
    , bgroup
        "3. Create Membership Proofs"
        [ bench "5 from 50" $ whnf rnf $ createMembershipProofFromUTxO @Tx subset5_from50 acc50 (generateCRS 50)
        , bench "10 from 100" $ whnf rnf $ createMembershipProofFromUTxO @Tx subset10_from100 acc100 (generateCRS 100)
        , bench "50 from 500" $ whnf rnf $ createMembershipProofFromUTxO @Tx subset50_from500 acc500 (generateCRS 500)
        , bench "100 from 1000" $ whnf rnf $ createMembershipProofFromUTxO @Tx subset100_from1000 acc1000 (generateCRS 1000)
        , bench "500 from 5000" $ whnf rnf $ createMembershipProofFromUTxO @Tx subset500_from5000 acc5000 (generateCRS 5000)
        , bench "1000 from 10000" $ whnf rnf $ createMembershipProofFromUTxO @Tx subset1000_from10000 acc10000 (generateCRS 10000)
        ]
    , bgroup
        "4. Create Membership Proofs (Low-level)"
        [ bench "5 elements from 10" $ whnf rnf $ createMembershipProof (take 5 serialized10) acc10 (generateCRS 10)
        , bench "10 elements from 100" $ whnf rnf $ createMembershipProof (take 10 serialized100) acc100 (generateCRS 100)
        , bench "50 elements from 100" $ whnf rnf $ createMembershipProof (take 50 serialized100) acc100 (generateCRS 100)
        ]
    , bgroup
        "5. Accumulator Hashing"
        [ bench "Hash 10 UTxOs" $ nf getAccumulatorHash acc10
        , bench "Hash 100 UTxOs" $ nf getAccumulatorHash acc100
        , bench "Hash 1000 UTxOs" $ nf getAccumulatorHash acc1000
        , bench "Hash 10000 UTxOs" $ nf getAccumulatorHash acc10000
        ]
    , bgroup
        "6. Accumulator Serialization"
        [ bench "Serialize accumulator (10)" $ nf (serialise . unHydraAccumulator) acc10
        , bench "Serialize accumulator (100)" $ nf (serialise . unHydraAccumulator) acc100
        , bench "Serialize accumulator (1000)" $ nf (serialise . unHydraAccumulator) acc1000
        , bench "Serialize accumulator (10000)" $ nf (serialise . unHydraAccumulator) acc10000
        ]
    , bgroup
        "7. CRS Generation"
        [ bench "CRS size 10" $ whnf generateCRS 10
        , bench "CRS size 100" $ whnf generateCRS 100
        , bench "CRS size 1000" $ whnf generateCRS 1000
        , bench "CRS size 5000" $ whnf generateCRS 5000
        , bench "CRS size 10000" $ whnf generateCRS 10000
        ]
    , bgroup
        "8. End-to-End Snapshot Simulation"
        [ bench "Full cycle: 100 UTxOs" $ nf rnf (fullSnapshotCycle utxo100)
        , bench "Full cycle: 1000 UTxOs" $ nf rnf (fullSnapshotCycle utxo1000)
        , bench "Partial fanout: 100 from 1000" $ nf rnf (partialFanoutCycle utxo1000 subset100_from1000)
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
   in createMembershipProofFromUTxO @Tx subsetUtxo accumulator defaultCRS
