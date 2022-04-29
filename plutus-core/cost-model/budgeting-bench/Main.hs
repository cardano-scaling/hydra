{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators       #-}
-- See CostModelGeneration.md
module Main (main) where

import           CriterionExtensions        (criterionMainWith)

import qualified Benchmarks.Bool
import qualified Benchmarks.ByteStrings
import qualified Benchmarks.CryptoAndHashes
import qualified Benchmarks.Data
import qualified Benchmarks.Integers
import qualified Benchmarks.Lists
import qualified Benchmarks.Misc
import qualified Benchmarks.Pairs
import qualified Benchmarks.Strings
import qualified Benchmarks.Tracing
import qualified Benchmarks.Unit

import           Criterion.Main
import           System.Random              (getStdGen)

---------------- Miscellaneous ----------------

{- Creates the .csv file consumed by create-cost-model. The data in this file is
   the time taken for all the builtin operations, as measured by criterion.  See
   also 'CostModelGeneration.md'. -}

{- Experimentation and examination of implementations suggests that the cost
   models for certain builtins can be re-used for others, and we do this in
   models.R.  Specifically, we re-use the cost models for the functions on the
   left below for the functions on the right as well.  Because of this we don't
   benchmark the functions on the right; the benchmarks take a long time to run,
   so this speeds things up a lot.

   AddInteger:            SubtractInteger
   DivideInteger:         RemainderInteger, QuotientInteger, ModInteger
-}

main :: IO ()
main = do
  gen <- System.Random.getStdGen  -- We use the initial state of gen repeatedly below, but that doesn't matter.

  criterionMainWith
       True
       defaultConfig $
{-
           Benchmarks.Bool.makeBenchmarks            gen
       <>  Benchmarks.ByteStrings.makeBenchmarks     gen
       <>  Benchmarks.CryptoAndHashes.makeBenchmarks gen
       <>  Benchmarks.Integers.makeBenchmarks        gen
       <>  Benchmarks.Misc.makeBenchmarks            gen
       <>  Benchmarks.Pairs.makeBenchmarks           gen
       <>  Benchmarks.Strings.makeBenchmarks         gen
       <>  Benchmarks.Tracing.makeBenchmarks         gen
       <>  Benchmarks.Unit.makeBenchmarks            gen
 -}
        Benchmarks.Data.makeBenchmarks            gen
    <>  Benchmarks.Lists.makeBenchmarks           gen


  {- Run the nop benchmarks with a large time limit (30 seconds) in an attempt to
     get accurate results. -}
  -- FIXME: this doesn't quite work.  If you specify a benchmark name on the
  -- command line and it's in the first group then it'll run but you'll get an
  -- error when the argument gets passed to the nop benchmarks below (but the
  -- data will still be generated and saved in benching.csv).

