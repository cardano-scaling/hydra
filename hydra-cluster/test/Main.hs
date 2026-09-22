module Main where

import Hydra.Prelude

import HydraNode (scaleWaitTime)
import Test.BlockfrostChainSpec qualified
import Test.CardanoClientSpec qualified
import Test.CardanoNodeSpec qualified
import Test.ChainObserverSpec qualified
import Test.DirectChainSpec qualified
import Test.EndToEndSpec qualified
import Test.GeneratorSpec qualified
import Test.Hydra.Cluster.CardanoCliSpec qualified
import Test.Hydra.Cluster.FaucetSpec qualified
import Test.Hydra.Cluster.MithrilSpec qualified
import Test.Hydra.Cluster.UtilSpec qualified
import Test.Hydra.TastyMain (hydraTestTree, runHydraTests, testSpec)
import Test.OfflineChainSpec qualified
import Test.Tasty (Timeout, localOption, mkTimeout)
import Test.Tasty.Runners (NumThreads (..))

-- Most tests in this suite each spawn a cardano-node devnet plus 3-6
-- hydra-nodes. Running two such tests concurrently oversubscribes the CPU
-- and breaks any test whose @waitMatch@ / @waitForAllMatch@ budget is
-- expressed in @blockTime@ units (often @10 * blockTime ≈ 1.5s@, which is
-- fine when one cardano-node has the box to itself but not when it has to
-- share). At NumThreads >= 2, ~1-3% of tests flake on each run with errors
-- like "waitMatch did not match within 1.5s" while the trace shows the
-- node still @CatchingUp@ with 10s of drift. Until those budgets are
-- loosened or the tests are refactored to share a single cardano-node, we
-- default to NumThreads 1.
--
-- Users on beefy machines can override via @--num-threads N@ on the
-- command line — tasty's @applyTopLevelPlusTestOptions@ layers CLI args
-- over the in-tree default, so the CLI value wins.
main :: IO ()
main = do
  tree <-
    hydraTestTree
      "hydra-cluster"
      [ testSpec "BlockfrostChain" Test.BlockfrostChainSpec.spec
      , testSpec "CardanoClient" Test.CardanoClientSpec.spec
      , testSpec "CardanoNode" Test.CardanoNodeSpec.spec
      , testSpec "ChainObserver" Test.ChainObserverSpec.spec
      , testSpec "DirectChain" Test.DirectChainSpec.spec
      , testSpec "EndToEnd" Test.EndToEndSpec.spec
      , testSpec "Generator" Test.GeneratorSpec.spec
      , testSpec "Hydra.Cluster.CardanoCli" Test.Hydra.Cluster.CardanoCliSpec.spec
      , testSpec "Hydra.Cluster.Faucet" Test.Hydra.Cluster.FaucetSpec.spec
      , testSpec "Hydra.Cluster.Mithril" Test.Hydra.Cluster.MithrilSpec.spec
      , testSpec "Hydra.Cluster.Util" Test.Hydra.Cluster.UtilSpec.spec
      , testSpec "OfflineChain" Test.OfflineChainSpec.spec
      ]
  -- hydraTestTree's own default per-test timeout is a fixed 900s, sized
  -- against the direct/local backend's failAfter budgets. It doesn't know
  -- about scaleWaitTime's 3x Blockfrost multiplier (and HYDRA_TEST_WAIT_MULTIPLIER
  -- on top of that), so a Blockfrost test chaining several scaled waits can
  -- need more than 900s to reach its own budgeted conclusion, and gets cut
  -- off early by the fixed backstop instead. Scale the backstop by the same
  -- factor so it stays a backstop against actual hangs, not against a slow
  -- but otherwise healthy backend. A concrete Timeout set here wins over
  -- hydraTestTree's default (see its NoTimeout -> ... case).
  backstop <- scaledBackstopTimeout
  runHydraTests "hydra-cluster" (localOption (NumThreads 1) (localOption backstop tree))

-- | The 900s default backstop timeout, scaled like 'scaleWaitTime'.
scaledBackstopTimeout :: IO Timeout
scaledBackstopTimeout = do
  scaled <- scaleWaitTime 900
  pure . mkTimeout . round $ scaled * 1_000_000
