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
  crsG1Points,
  getAccumulatorHash,
  unHydraAccumulator,
 )
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.KZGTrustedSetup (fanoutChunkSize)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck (generate)

--- TODO: Get this benchmark to be ran as part of the GitHub "benchmark"
--comment thing
main :: IO ()
main = do
  putTextLn "=== Accumulator Benchmark Suite ==="
  putTextLn "Generating test data..."

  -- NOTE: The accumulator now uses G1 points for commitment (4096 available from
  -- EIP-4844), so the maximum is 4095 elements. We benchmark a wide range to show
  -- how build/hash/proof costs scale.

  -- Generate UTxO sets across the full useful range
  utxo10 <- generateUTxO 10
  utxo50 <- generateUTxO 50
  utxo100 <- generateUTxO 100
  utxo500 <- generateUTxO 500
  utxo1000 <- generateUTxO 1000
  utxo2000 <- generateUTxO 2000
  utxo4000 <- generateUTxO 4000

  putTextLn "Generated UTxO sets: 10, 50, 100, 500, 1000, 2000, 4000"

  -- Pre-build accumulators and force them
  let !acc10 = buildFromUTxO @Tx utxo10
      !acc50 = buildFromUTxO @Tx utxo50
      !acc100 = buildFromUTxO @Tx utxo100
      !acc500 = buildFromUTxO @Tx utxo500
      !acc1000 = buildFromUTxO @Tx utxo1000
      !acc2000 = buildFromUTxO @Tx utxo2000
      !acc4000 = buildFromUTxO @Tx utxo4000

  putTextLn "Pre-built accumulators"

  -- Pre-generate G1 CRS of various sizes and force them
  let !crs11 = crsG1Points 11
      !crs51 = crsG1Points 51
      !crs101 = crsG1Points 101
      !crs501 = crsG1Points 501
      !crs1001 = crsG1Points 1001
      !crs2001 = crsG1Points 2001
      !crs4001 = crsG1Points 4001

  putTextLn "Pre-generated CRS"

  -- Generate fanoutChunkSize subsets for membership proofs
  let !subsetChunk_from50 = generateSubset utxo50 fanoutChunkSize
  let !subsetChunk_from100 = generateSubset utxo100 fanoutChunkSize
  let !subsetChunk_from500 = generateSubset utxo500 fanoutChunkSize
  let !subsetChunk_from1000 = generateSubset utxo1000 fanoutChunkSize
  let !subsetChunk_from2000 = generateSubset utxo2000 fanoutChunkSize
  let !subsetChunk_from4000 = generateSubset utxo4000 fanoutChunkSize

  putTextLn "Generated subsets for membership proofs"

  -- Extract individual elements for low-level proof testing
  let !elements10 = toPairList @Tx utxo10
      !elements100 = toPairList @Tx utxo100
      !serialized10 = utxoToElement @Tx <$> elements10
      !serialized100 = utxoToElement @Tx <$> elements100

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
        , bench "2000 UTxOs" $ whnf (buildFromUTxO @Tx) utxo2000
        , bench "4000 UTxOs" $ whnf (buildFromUTxO @Tx) utxo4000
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
        "3. Create Membership Proofs (fanoutChunkSize batch)"
        [ bench "fanoutChunkSize from 50" $ nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc50 crs51) subsetChunk_from50
        , bench "fanoutChunkSize from 100" $ nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc100 crs101) subsetChunk_from100
        , bench "fanoutChunkSize from 500" $ nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc500 crs501) subsetChunk_from500
        , bench "fanoutChunkSize from 1000" $ nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc1000 crs1001) subsetChunk_from1000
        , bench "fanoutChunkSize from 2000" $ nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc2000 crs2001) subsetChunk_from2000
        , bench "fanoutChunkSize from 4000" $ nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc4000 crs4001) subsetChunk_from4000
        ]
    , bgroup
        "4. Create Membership Proofs (Low-level, variable batch size)"
        [ bench "5 from 10" $ nf (\s -> unsafeProof $ createMembershipProof s acc10 crs11) (take 5 serialized10)
        , bench "15 from 100" $ nf (\s -> unsafeProof $ createMembershipProof s acc100 crs101) (take 15 serialized100)
        , bench "30 from 100" $ nf (\s -> unsafeProof $ createMembershipProof s acc100 crs101) (take 30 serialized100)
        , bench "60 from 100" $ nf (\s -> unsafeProof $ createMembershipProof s acc100 crs101) (take 60 serialized100)
        ]
    , bgroup
        "5. Accumulator Hashing (blake2b of compressed G1 commitment)"
        [ bench "Hash 10 UTxOs" $ nf getAccumulatorHash acc10
        , bench "Hash 100 UTxOs" $ nf getAccumulatorHash acc100
        , bench "Hash 500 UTxOs" $ nf getAccumulatorHash acc500
        , bench "Hash 1000 UTxOs" $ nf getAccumulatorHash acc1000
        , bench "Hash 2000 UTxOs" $ nf getAccumulatorHash acc2000
        , bench "Hash 4000 UTxOs" $ nf getAccumulatorHash acc4000
        ]
    , bgroup
        "6. Accumulator Serialization"
        [ bench "Serialize accumulator (10)" $ nf (serialise . unHydraAccumulator) acc10
        , bench "Serialize accumulator (100)" $ nf (serialise . unHydraAccumulator) acc100
        , bench "Serialize accumulator (1000)" $ nf (serialise . unHydraAccumulator) acc1000
        , bench "Serialize accumulator (4000)" $ nf (serialise . unHydraAccumulator) acc4000
        ]
    , bgroup
        "7. CRS Loading (G1 powers of tau from EIP-4844 trusted setup)"
        -- Point1 lacks NFData, so we force the full spine via length
        [ bench "CRS size 11" $ whnf (length . crsG1Points) 11
        , bench "CRS size 101" $ whnf (length . crsG1Points) 101
        , bench "CRS size 501" $ whnf (length . crsG1Points) 501
        , bench "CRS size 1001" $ whnf (length . crsG1Points) 1001
        , bench "CRS size 2001" $ whnf (length . crsG1Points) 2001
        , bench "CRS size 4001" $ whnf (length . crsG1Points) 4001
        ]
    , bgroup
        "8. End-to-End Snapshot Simulation"
        [ bench "Full cycle: 100 UTxOs" $ nf fullSnapshotCycle utxo100
        , bench "Full cycle: 1000 UTxOs" $ nf fullSnapshotCycle utxo1000
        , bench "Full cycle: 4000 UTxOs" $ nf fullSnapshotCycle utxo4000
        , bench "Partial fanout: fanoutChunkSize from 50" $ nf (partialFanoutCycle utxo50) subsetChunk_from50
        , bench "Partial fanout: fanoutChunkSize from 500" $ nf (partialFanoutCycle utxo500) subsetChunk_from500
        , bench "Partial fanout: fanoutChunkSize from 4000" $ nf (partialFanoutCycle utxo4000) subsetChunk_from4000
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

-- | Unwrap a proof result, crashing on error. Only for benchmark use where
-- inputs are always valid by construction.
unsafeProof :: Either Text ByteString -> ByteString
unsafeProof = either error id

-- | Simulate a partial fanout operation:
-- 1. Build accumulator from full UTxO
-- 2. Create membership proof for subset
-- 3. Return the proof
partialFanoutCycle :: UTxO -> UTxO -> ByteString
partialFanoutCycle fullUtxo subsetUtxo =
  let accumulator = buildFromUTxO @Tx fullUtxo
      crs = crsG1Points (UTxO.size fullUtxo + 1)
   in unsafeProof $ createMembershipProofFromUTxO @Tx subsetUtxo accumulator crs
