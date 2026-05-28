{-# LANGUAGE DuplicateRecordFields #-}

-- | Golden tests of hydra-plutus scripts.
--
-- This test suite ensures we do not accidentally change scripts and also
-- persists the plutus scripts of the Hydra protocol as blobs in the repository.
--
-- This is also crucial in case we cannot reproduce them exactly as they were
-- originally compiled using plutus-tx; which is not unlikely given we need to
-- have the exact same version of plutus-tx, all its dependencies, and GHC.
module Hydra.Plutus.GoldenSpec where

import Hydra.Prelude

import Hydra.Cardano.Api (
  File (..),
  PlutusScript,
  Script,
  hashScript,
  readFileTextEnvelope,
  writeFileTextEnvelope,
  pattern PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Contract.CRS qualified as CRS
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Version (gitDescribe)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import System.Process.Typed (runProcess_, shell)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit (assertFailure, testCase)

aikenBuildCommand :: String
aikenBuildCommand = "aiken build -t compact"

tests :: TestTree
tests =
  testGroup
    "Hydra.Plutus.Golden"
    [ testCase "Plutus blueprint is up-to-date" $ do
        -- Running aiken -t compact should not change plutus.json
        existing <- readFileBS "plutus.json"
        runProcess_ $ shell aikenBuildCommand
        actual <- readFileBS "plutus.json"
        -- Undo any changes made by aiken
        writeFileBS "plutus.json" existing
        when (actual /= existing) $
          assertFailure $
            "Plutus blueprint in plutus.json is not up-to-date. Run "
              <> show aikenBuildCommand
              <> " to update it."
    , goldenScript "Head validator script" "vHead" Head.validatorScript
    , goldenScript
        "Head minting policy script"
        "mHead"
        (PlutusScriptSerialised $ serialiseCompiledCode HeadTokens.unappliedMintingPolicy)
    , goldenScript "CRS script" "vCRS" CRS.validatorScript
    ]

-- | Write a golden script on first run and ensure it stays the same on
-- subsequent runs. The on-disk representation is cardano-api's text-envelope
-- JSON; comparison is by 'Script' equality (ignoring the description metadata,
-- which embeds a git describe and would otherwise be unstable).
goldenScript :: String -> String -> PlutusScript -> TestTree
goldenScript testName name plutusScript =
  goldenTest
    testName
    readGolden
    (pure expected)
    compareScripts
    writeGolden
 where
  expected :: Script
  expected = PlutusScript plutusScript

  goldenFile :: FilePath
  goldenFile = "scripts/" <> name <> ".plutus"

  fullScriptName :: String
  fullScriptName = "hydra-" <> name <> maybe "" ("-" <>) gitDescribe

  readGolden :: IO Script
  readGolden =
    readFileTextEnvelope (File goldenFile)
      >>= either (fail . show) pure

  writeGolden :: Script -> IO ()
  writeGolden script =
    writeFileTextEnvelope (File goldenFile) (Just $ fromString fullScriptName) script
      >>= either (fail . show) pure

  compareScripts :: Script -> Script -> IO (Maybe String)
  compareScripts golden actual
    | golden == actual = pure Nothing
    | otherwise =
        pure . Just $
          "script hash mismatch:\n  golden: "
            <> show (hashScript golden)
            <> "\n  actual: "
            <> show (hashScript actual)
