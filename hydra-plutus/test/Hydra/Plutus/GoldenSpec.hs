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

import Control.Lens ((^?!))
import Data.Aeson.Lens (key, nth, _String)
import Hydra.Cardano.Api (
  AsType (AsPlutusScriptV3, AsScript),
  File (..),
  Script,
  fromPlutusScript,
  hashScript,
  readFileTextEnvelope,
  writeFileTextEnvelope,
  pattern PlutusScript,
 )
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Version (gitDescribe)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as Plutus
import System.IO.Streams qualified as Stream
import System.Process (readProcess)
import Test.Hspec.Golden (Golden (..))

spec :: Spec
spec = do
  it "Commit and initial validator scripts" $ do
    original <- readFileBS "plutus.json"
    -- This re-generate plutus.json
    let commitHash = encodeUtf8 $ original ^?! key "validators" . nth 0 . key "hash" . _String
    void $ readProcess "aiken" ["build", "-t", "compact"] ""
    (outStream, _, _, _) <- Stream.runInteractiveProcess "aiken" ["blueprint", "apply", "-v", "initial.initial.spend", "-o", "plutus.json"] Nothing Nothing
    Stream.write (Just commitHash) outStream
    (outStream2, _, _, _) <- Stream.runInteractiveProcess "aiken" ["blueprint", "apply", "-v", "initial.initial.else", "-o", "plutus.json"] Nothing Nothing
    Stream.write (Just commitHash) outStream2
    regenerated <- readFileBS "plutus.json"
    regenerated `shouldBe` original
  it "Head validator script" $
    goldenScript "vHead" Head.validatorScript
  it "Head minting policy script" $
    goldenScript "mHead" (serialiseCompiledCode HeadTokens.unappliedMintingPolicy)
  it "Deposit validator script" $
    goldenScript "vDeposit" Deposit.validatorScript

-- | Write a golden script on first run and ensure it stays the same on
-- subsequent runs.
goldenScript :: String -> Plutus.SerialisedScript -> Golden Script
goldenScript name plutusScript =
  Golden
    { output = PlutusScript $ fromPlutusScript plutusScript
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
