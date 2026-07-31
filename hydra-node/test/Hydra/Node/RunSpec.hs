module Hydra.Node.RunSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.BaseTypes (Globals (..))
import Cardano.Slotting.EpochInfo (epochInfoEpoch, epochInfoSlotToUTCTime)
import Hydra.Cardano.Api (SlotNo (..))
import Hydra.Chain.Offline (loadGenesisFile)
import Hydra.Node.Run (ConfigurationException, newGlobalsWithEraHistory, run)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainConfig (..),
  RunOptions (..),
  defaultCardanoChainConfig,
  defaultRunOptions,
 )
import Test.Hydra.Ledger.Cardano.Fixtures (eraHistoryWithHorizonAt)
import Test.Hydra.Options (genFilePath)
import Test.QuickCheck (generate)

spec :: Spec
spec = do
  it "throws exception given options are invalid" $ do
    cardanoKeys <- generate $ replicateM 1 (genFilePath "vk")
    hydraVerificationKeys <- generate $ replicateM 2 (genFilePath "vk")
    run
      defaultRunOptions
        { chainConfig = Cardano defaultCardanoChainConfig{fuelVerificationKeys = cardanoKeys}
        , hydraVerificationKeys
        }
      `shouldThrow` aConfigurationException

  describe "newGlobalsWithEraHistory" $
    it "converts slots beyond the queried era history horizon" $ do
      genesisParameters <- loadGenesisFile Nothing
      globals <- newGlobalsWithEraHistory genesisParameters (eraHistoryWithHorizonAt (SlotNo 100))
      let beyondHorizon = SlotNo 101
      epochInfoSlotToUTCTime (epochInfo globals) (systemStart globals) beyondHorizon
        `shouldSatisfy` isRight
      epochInfoEpoch (epochInfo globals) beyondHorizon
        `shouldSatisfy` isRight

aConfigurationException :: Selector ConfigurationException
aConfigurationException = const True
