{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

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
  AsType (AsPlutusScriptV2, AsScript),
  Script,
  fromPlutusScript,
  hashScript,
  readFileTextEnvelope,
  writeFileTextEnvelope,
  pattern PlutusScript,
 )
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadTokens as HeadTokens
import qualified Hydra.Contract.Initial as Initial
import Plutus.V2.Ledger.Api (fromCompiledCode)
import qualified Plutus.V2.Ledger.Api as Plutus
import Test.Hspec.Golden (Golden (..))

spec :: Spec
spec = do
  it "Initial validator script" $
    goldenScript "vInitial" Initial.validatorScript
  it "Commit validator script" $
    goldenScript "vCommit" Commit.validatorScript
  it "Head validator script" $
    goldenScript "vHead" Head.validatorScript
  it "Head minting policy script" $
    goldenScript "mHead" (fromCompiledCode HeadTokens.unappliedMintingPolicy)

-- | Write a golden script on first run and ensure it stays the same on
-- subsequent runs.
goldenScript :: FilePath -> Plutus.Script -> Golden Script
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
  writeToFile fp script =
    -- TODO: add description
    void $ writeFileTextEnvelope fp Nothing script

  readFromFile fp =
    either (die . show) pure
      =<< readFileTextEnvelope (AsScript AsPlutusScriptV2) fp
