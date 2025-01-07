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
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  AsType (AsPlutusScriptV3, AsScript),
  File (..),
  Script,
  hashScript,
  readFileTextEnvelope,
  writeFileTextEnvelope,
  pattern PlutusScript,
  pattern PlutusScriptSerialised,
 )
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Plutus (commitValidatorScript, depositValidatorScript, initialValidatorScript)
import Hydra.Version (gitDescribe)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as Plutus
import System.Process.Typed (proc, runProcess_)
import Test.Hspec.Golden (Golden (..))

spec :: Spec
spec = do
  it "Plutus blueprint is up-to-date" $ do
    -- Running aiken -t compact should not change plutus.json
    existing <- readFileBS "plutus.json"
    runProcess_ $ proc "aiken" ["build", "-t", "compact"]
    actual <- readFileBS "plutus.json"
    -- Undo any changes made by aiken
    writeFileBS "plutus.json" existing
    when (actual /= existing) $ do
      putTextLn "Plutus blueprint in plutus.json is not up-to-date. Run \"aiken -t compact\" to update it."
    actual `shouldBe` existing

  it "Head validator script" $
    goldenScript "vHead" Head.validatorScript
  it "Head minting policy script" $
    goldenScript "mHead" (serialiseCompiledCode HeadTokens.unappliedMintingPolicy)
  it "Deposit validator script" $
    goldenScript "vDeposit" depositValidatorScript
  it "Initial validator script" $
    goldenScript "vInitial" initialValidatorScript
  it "Commit validator script" $
    goldenScript "vCommit" commitValidatorScript

-- | Write a golden script on first run and ensure it stays the same on
-- subsequent runs.
goldenScript :: String -> Plutus.SerialisedScript -> Golden Script
goldenScript name plutusScript =
  Golden
    { output = PlutusScript $ PlutusScriptSerialised plutusScript
    , encodePretty = show . hashScript
    , writeToFile
    , readFromFile
    , goldenFile = "scripts/" <> name <> ".plutus"
    , actualFile = Nothing
    , failFirstTime = False
    }
 where
  fullScriptName = "hydra-" <> name <> maybe "" ("-" <>) gitDescribe

  writeToFile fp script =
    void $ writeFileTextEnvelope (File fp) (Just $ fromString fullScriptName) script

  readFromFile fp =
    either (die . show) pure
      =<< readFileTextEnvelope (AsScript AsPlutusScriptV3) (File fp)
