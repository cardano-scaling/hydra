-- | Measure content-based fanout selection independently of proof generation.
-- Fixtures use fixed seeds and are forced and checked before timing.
module Main where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, whnf)
import Data.Set qualified as Set
import Hydra.Cardano.Api (CtxUTxO, Tx, TxIn (..), TxIx (..), TxOut, UTxO, forceUTxO)
import Hydra.HeadLogic (isSubMultisetOf, sameOutputs)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  maxN <- fromMaybe 1000 . (>>= readMaybe) <$> lookupEnv "BENCH_MAX_UTXO"
  when (maxN < 100) $ die "BENCH_MAX_UTXO must be at least 100"
  benches <- traverse benchSize $ filter (<= maxN) [100, 1000, 4000, 16000]
  defaultMain benches

benchSize :: Int -> IO Benchmark
benchSize n = do
  let generated = UTxO.toList $ unGen (genUTxOAdaOnlyOfSize (n + 1)) (mkQCGen (4200 + n)) 30
  case generated of
    (TxIn txid _, extra) : entries -> do
      let outputs = snd <$> entries
          make :: Int -> [TxOut CtxUTxO] -> UTxO
          make offset outs = forceUTxO $ UTxO.fromList $ zipWith (\i out -> (TxIn txid (TxIx $ fromIntegral i), out)) [offset ..] outs
          full = make 0 outputs
          repeated = make 0 (replicate n extra)
          sizes = Set.toAscList $ Set.fromList [1, min 64 n, n]
      unless (length outputs == n && Set.size (Set.fromList (extra : outputs)) == n + 1) $
        die "fanout fixture must contain exactly n distinct outputs plus one absent output"
      selections <- forM sizes $ \k -> do
        let selected = take k outputs
        checks <-
          sequence
            [ membership "original-references" True (make 0 selected) full
            , membership "renamed-references" True (make n selected) full
            , membership "duplicate-outputs" True (make n (replicate k extra)) repeated
            , membership "missing-last-output" False (make n (take (k - 1) outputs <> [extra])) full
            ]
        pure $ bgroup ("selected-" <> show k) checks
      excess <- membership "excess-duplicate" False (make n (replicate (n + 1) extra)) repeated
      equal <- equality "same-outputs-renamed" (make n outputs) full
      identical <- equality "same-outputs-identical" full full
      pure $ bgroup ("fanout/utxo-" <> show n) (selections <> [excess, equal, identical])
    [] -> die "empty fanout fixture"

membership :: String -> Bool -> UTxO -> UTxO -> IO Benchmark
membership name expected selection full = do
  -- Traversing both maps forces the prepared inputs even for future algorithms
  -- that return before visiting every output of an invalid selection.
  let !selectionSize = UTxO.size selection
      !fullSize = UTxO.size full
  unless (selectionSize > 0 && fullSize > 0 && isSubMultisetOf @Tx selection full == expected) $
    die $
      "unexpected fanout membership result: " <> name
  pure $ bench name $ whnf (uncurry (isSubMultisetOf @Tx)) (selection, full)

equality :: String -> UTxO -> UTxO -> IO Benchmark
equality name selection full = do
  unless (sameOutputs @Tx selection full) $ die $ "unexpected fanout equality result: " <> name
  pure $ bench name $ whnf (uncurry (sameOutputs @Tx)) (selection, full)
