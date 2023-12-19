module Hydra.Node.RunSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Node.Run (ConfigurationException, run)
import Hydra.Options (ChainConfig (..), RunOptions (..), defaultDirectChainConfig, defaultRunOptions, genFilePath)
import Test.QuickCheck (generate)

spec :: Spec
spec =
  it "throws exception given options are invalid" $ do
    cardanoKeys <- generate $ replicateM 1 (genFilePath "vk")
    hydraVerificationKeys <- generate $ replicateM 2 (genFilePath "vk")
    let chainConfiguration = defaultDirectChainConfig{cardanoVerificationKeys = cardanoKeys}
        options = defaultRunOptions{chainConfig = chainConfiguration, hydraVerificationKeys}

    run options `shouldThrow` aConfigurationException

aConfigurationException :: Selector ConfigurationException
aConfigurationException = const True
