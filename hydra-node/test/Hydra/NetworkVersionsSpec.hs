module Hydra.NetworkVersionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Version (Version (..), makeVersion)
import Hydra.NetworkVersions (hydraNodeVersion, parseNetworkTxIds)
import Hydra.Version (embeddedRevision, gitRevision, unknownVersion)
import Test.QuickCheck (Property, counterexample, forAll, property)

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

    -- Only the executables are patched by nix, never this test binary, so the
    -- embedded array still holds the placeholder here. A 'Just' means the
    -- placeholder in 'Hydra.Version' has drifted from 'cbits/revision.c' again:
    -- every unpatched build would then report the placeholder as its revision,
    -- and 'gitRevision' would never be consulted.
    it "finds no embedded revision in an unpatched binary" $
      embeddedRevision `shouldBe` Nothing

    -- With no embedded revision (see above), the version tag is the git
    -- revision, or 'unknownVersion' where git information is unavailable, as in
    -- a nix build whose source has no '.git'. Before the placeholder fix this
    -- reported the 'cbits/revision.c' placeholder in both cases, so this still
    -- catches that regression without depending on git being present.
    it "falls back to the git revision" $
      case hydraNodeVersion of
        Version _ tags -> tags `shouldBe` [fromMaybe unknownVersion gitRevision]

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
