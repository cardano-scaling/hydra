module Hydra.Node.RunSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (generate)

import Hydra.Node.Run (ConfigurationException, run)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainConfig (..),
  RunOptions (..),
  defaultCardanoChainConfig,
  defaultRunOptions,
 )
import Test.Hydra.Options (genFilePath)

spec :: Spec
spec =
  it "throws exception given options are invalid" $ do
    cardanoKeys <- generate $ replicateM 1 (genFilePath "vk")
    hydraVerificationKeys <- generate $ replicateM 2 (genFilePath "vk")
    run
      defaultRunOptions
        { chainConfig = Cardano defaultCardanoChainConfig{cardanoVerificationKeys = cardanoKeys}
        , hydraVerificationKeys
        }
      `shouldThrow` aConfigurationException

aConfigurationException :: Selector ConfigurationException
aConfigurationException = const True
