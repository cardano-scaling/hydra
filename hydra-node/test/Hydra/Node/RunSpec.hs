module Hydra.Node.RunSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Node.Run (ConfigurationException, run)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainConfig (..),
  RunOptions (..),
  defaultCardanoChainConfig,
  defaultRunOptions,
 )
import Test.Hydra.Options (genFilePath)
import Test.QuickCheck (generate)

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
