{-# LANGUAGE DuplicateRecordFields #-}

-- | Benchmark suite for the BLS accumulator implementation.
--
-- This suite measures the performance of accumulator operations with realistic
-- UTxO sets to understand the performance implications of using accumulators
-- for snapshot signing and partial fanout.
--
-- Benchmarking rules:
--
--  * 'HydraAccumulator' memoizes its hash per value, so never measure a
--    cache-reading function (like 'getAccumulatorHash') applied to an
--    argument shared across iterations; either rebuild the accumulator inside
--    the measured function or measure one that recomputes unconditionally
--    (like 'getAccumulatorCommitment').
--
--  * Plutus builtin wrappers ('BuiltinBLS12_381_G1_Element' et al) are lazy:
--    WHNF of a commitment point does none of the BLS math. Always force
--    through compression to bytes.
module Main where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Codec.Serialise (serialise)
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Hydra.Cardano.Api
import Hydra.Tx.Accumulator (
  buildFromUTxO,
  computeG1CommitmentBytes,
  createMembershipProof,
  createMembershipProofFromUTxO,
  crsG1Points,
  getAccumulatorHash,
  unHydraAccumulator,
 )
import Hydra.Tx.IsTx (IsTx (..))
import Test.Hydra.Tx.Fixture (fanoutChunkSize)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck (generate)

--- TODO: Get this benchmark to be ran as part of the GitHub "benchmark"
-- comment thing
main :: IO ()
main = do
  -- A single uncached commitment at 4000 UTxOs currently takes minutes, so the
  -- largest sizes are opt-in:
  --
  -- > BENCH_MAX_UTXO=4000 cabal bench hydra-tx:accumulator-bench
  maxN <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "BENCH_MAX_UTXO"
  let sizes = filter (<= maxN) [10, 50, 100, 500, 1000, 2000, 4000]

  putTextLn "=== Accumulator Benchmark Suite ==="
  putTextLn $ "Generating UTxO sets: " <> show sizes

  fixtures <- forM sizes $ \n -> do
    utxo <- generate $ genUTxOAdaOnlyOfSize n
    let acc = buildFromUTxO @Tx utxo
        crs = crsG1Points (n + 1)
        subset = generateSubset utxo fanoutChunkSize
    -- Deep-force the accumulator map (but not the memoized hash) and the CRS
    -- spine (Point1 has no NFData) so benchmarks measure only their own work.
    let !_ = unHydraAccumulator acc `deepseq` (length crs + UTxO.size subset)
    pure (n, utxo, acc, crs, subset)

  -- Fixed-size fixtures for element conversion and low-level proof benches
  utxo10 <- generate $ genUTxOAdaOnlyOfSize 10
  utxo100 <- generate $ genUTxOAdaOnlyOfSize 100
  let elements10 = outputsOfUTxO @Tx utxo10
      elements100 = outputsOfUTxO @Tx utxo100
      serialized10 = utxoToElement @Tx <$> elements10
      serialized100 = utxoToElement @Tx <$> elements100
      acc10 = buildFromUTxO @Tx utxo10
      acc100 = buildFromUTxO @Tx utxo100
      crs11 = crsG1Points 11
      crs101 = crsG1Points 101
  let !_ =
        (serialized10, serialized100)
          `deepseq` unHydraAccumulator acc10
          `deepseq` unHydraAccumulator acc100
          `deepseq` (length crs11 + length crs101)

  putTextLn "Starting benchmarks..."
  putTextLn ""

  defaultMain
    [ bgroup
        "1. Build Accumulator Map from UTxO"
        -- Forces per-TxOut plutus-Data serialization + sha256 + blake2b224 and
        -- the Map, but not the G1 commitment (see group 5 and 8 for that).
        [ bench (show n <> " UTxOs") $ nf (unHydraAccumulator . buildFromUTxO @Tx) utxo
        | (n, utxo, _, _, _) <- fixtures
        ]
    , bgroup
        "2. UTxO to Elements Conversion"
        [ bench "Extract 10 TxOuts" $ whnf (length . outputsOfUTxO @Tx) utxo10
        , bench "Extract 100 TxOuts" $ whnf (length . outputsOfUTxO @Tx) utxo100
        , bench "Serialize 10 TxOuts" $ nf (map (utxoToElement @Tx)) elements10
        , bench "Serialize 100 TxOuts" $ nf (map (utxoToElement @Tx)) elements100
        ]
    , bgroup
        "3. Create Membership Proofs (fanoutChunkSize batch)"
        [ bench ("fanoutChunkSize from " <> show n) $
          nf (\s -> unsafeProof $ createMembershipProofFromUTxO @Tx s acc crs) subset
        | (n, _, acc, crs, subset) <- fixtures
        , n >= 50
        ]
    , bgroup
        "4. Create Membership Proofs (Low-level, variable batch size)"
        [ bench "5 from 10" $ nf (\s -> unsafeProof $ createMembershipProof s acc10 crs11) (take 5 serialized10)
        , bench "15 from 100" $ nf (\s -> unsafeProof $ createMembershipProof s acc100 crs101) (take 15 serialized100)
        , bench "30 from 100" $ nf (\s -> unsafeProof $ createMembershipProof s acc100 crs101) (take 30 serialized100)
        , bench "60 from 100" $ nf (\s -> unsafeProof $ createMembershipProof s acc100 crs101) (take 60 serialized100)
        ]
    , bgroup
        "5. Commitment (uncached: polynomial expansion + MSM)"
        -- computeG1CommitmentBytes recomputes on every call (it does not read
        -- the memoized hash) and returns strict bytes, so WHNF forces the
        -- whole computation. This is the per-snapshot signing cost minus the
        -- map build.
        [ bench (show n <> " UTxOs") $
          whnf (computeG1CommitmentBytes . unHydraAccumulator) acc
        | (n, _, acc, _, _) <- fixtures
        ]
    , bgroup
        "6. Accumulator Serialization"
        [ bench ("Serialize accumulator (" <> show n <> ")") $ nf (serialise . unHydraAccumulator) acc
        | (n, _, acc, _, _) <- fixtures
        ]
    , bgroup
        "7. CRS Loading (G1 powers of tau from EIP-4844 trusted setup)"
        -- Point1 lacks NFData, so we force the full spine via length
        [ bench ("CRS size " <> show (n + 1)) $ whnf (length . crsG1Points) (n + 1)
        | (n, _, _, _, _) <- fixtures
        ]
    , bgroup
        "8. End-to-End Snapshot Simulation"
        ( [ bench ("Full cycle: " <> show n <> " UTxOs") $ nf fullSnapshotCycle utxo
          | (n, utxo, _, _, _) <- fixtures
          ]
            <> [ bench ("Partial fanout: fanoutChunkSize from " <> show n) $ nf (partialFanoutCycle utxo) subset
               | (n, utxo, _, _, subset) <- fixtures
               , n >= 50
               ]
        )
    ]

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
--
-- The accumulator is rebuilt inside the measured function, so its memoized
-- hash is computed fresh on every iteration.
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
