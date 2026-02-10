module Hydra.NetworkVersionsSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (Property, counterexample, forAll, property)
import "base" Data.Version (Version, makeVersion)

import Hydra.NetworkVersions (hydraNodeVersion, parseNetworkTxIds)

spec :: Spec
spec =
  describe "NetworkVersions" $ do
    it "parseNetworkTxIds produces list TxId" $ do
      let networks = ["mainnet", "preview", "preprod"]
      let versions = makeVersion . (\v -> [0, v, 0]) <$> [13 .. 21]
      forM_ networks $ \network ->
        forM_ versions $ \version -> do
          case parseNetworkTxIds version network of
            Left err -> failure $ "Failed to parse network tx ids: " <> err
            Right txIds -> txIds `shouldSatisfy` not . null

    prop "parseNetworkTxIds works with expected versions and networks" $
      forAll arbitrary $ \version ->
        forAll arbitrary $ \network ->
          propParseNetworkTxIds version network

propParseNetworkTxIds :: Version -> String -> Property
propParseNetworkTxIds version network = do
  let varlidNetworks = ["mainnet", "preview", "preprod"]
  let validVersions = (makeVersion . (\v -> [0, v, 0]) <$> [13 .. 21]) <> [hydraNodeVersion]
  case parseNetworkTxIds version network of
    Left err ->
      if network `elem` varlidNetworks && version `elem` validVersions
        then property False & counterexample ("error: " <> err)
        else property True
    Right txIds -> property $ not (null txIds)
