module Test.CardanoNodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (
  RunningNode (..),
  getCardanoNodeVersion,
  newDevnetConfig,
  withCardanoNodeDevnet,
 )

import Hydra.Logging (showLogsOnFailure)
import System.Directory (doesFileExist)

spec :: Spec
spec = do
  -- NOTE: We also hard-code the cardano-node version here to allow prevent
  -- false positives test errors in case someone uses an "untested" /
  -- different than in shell.nix version of cardano-node and cardano-cli.
  it "has expected cardano-node version available" $
    getCardanoNodeVersion >>= (`shouldContain` "1.35.0")

  it "withCardanoNodeDevnet does start a node within 3 seconds" $
    failAfter 3 $
      showLogsOnFailure $ \tr -> do
        withTempDir "hydra-cluster" $ \tmp -> do
          config <- newDevnetConfig tmp
          withCardanoNodeDevnet tr config $ \RunningNode{nodeSocket} -> do
            -- TODO: assert blocks are produced
            doesFileExist nodeSocket `shouldReturn` True
