module Main where

import Hydra.Prelude

import Test.BlockfrostChainSpec qualified
import Test.CardanoClientSpec qualified
import Test.CardanoNodeSpec qualified
import Test.ChainObserverSpec qualified
import Test.DirectChainSpec qualified
import Test.EndToEndSpec qualified
import Test.GeneratorSpec qualified
import Test.Hydra.Cluster.CardanoCliSpec qualified
import Test.Hydra.Cluster.FaucetSpec qualified
import Test.Hydra.Cluster.HydraClientSpec qualified
import Test.Hydra.Cluster.MithrilSpec qualified
import Test.Hydra.TastyMain (hydraTestTree, runHydraTests, testSpec)
import Test.OfflineChainSpec qualified
import Test.Tasty (localOption)
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
      , testSpec "Hydra.Cluster.HydraClient" Test.Hydra.Cluster.HydraClientSpec.spec
      , testSpec "Hydra.Cluster.Mithril" Test.Hydra.Cluster.MithrilSpec.spec
      , testSpec "OfflineChain" Test.OfflineChainSpec.spec
      ]
  runHydraTests "hydra-cluster" (localOption (NumThreads 1) tree)
