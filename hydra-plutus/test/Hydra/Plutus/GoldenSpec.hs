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

import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth, _String)
import Data.ByteString.Lazy qualified as BSL
import Hydra.Cardano.Api (
  AsType (AsPlutusScriptV2, AsScript),
  File (..),
  Script,
  fromPlutusScript,
  hashScript,
  readFileTextEnvelope,
  serialiseToRawBytesHexText,
  writeFileTextEnvelope,
  pattern PlutusScript,
 )
import Hydra.Contract (ScriptInfo (commitScriptHash), scriptInfo)
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Contract.Initial qualified as Initial
import Hydra.Version (gitDescribe)
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import PlutusLedgerApi.V2 qualified as Plutus
import System.FilePath ((</>))
import System.Process (
  CreateProcess (..),
  StdStream (UseHandle),
  createProcess,
  proc,
  waitForProcess,
 )
import Test.Hspec.Golden (Golden (..))

spec :: Spec
spec = do
  it "checks plutus blueprint remains the same" $ do
    withTempDir "hydra-plutus-golden" $ \tmpDir -> do
      -- FIXME: This requires a git working copy, do it differently.
      -- Run 'aiken build' to re-generate plutus.json file
      let aikenLogFilePath = tmpDir </> "logs" </> "aiken-processes.log"
      _ <- withLogFile aikenLogFilePath $ \out -> do
        hSetBuffering out NoBuffering
        let aikenExec = proc "aiken" ["build", "-k"]
            aikenProcess = aikenExec{std_out = UseHandle out, std_err = UseHandle out}
        (_, _, _, aikenProcessHandle) <- createProcess aikenProcess
        waitForProcess aikenProcessHandle
      -- Run 'git status' to see if plutus.json file has changed
      let gitLogFilePath = tmpDir </> "logs" </> "git-processes.log"
      _ <- withLogFile gitLogFilePath $ \out -> do
        hSetBuffering out NoBuffering
        let gitStatusExec = proc "git" ["status", "--porcelain"]
            gitStatusProcess = gitStatusExec{std_out = UseHandle out, std_err = UseHandle out}
        (_, _, _, gitStatusProcessHandle) <- createProcess gitStatusProcess
        waitForProcess gitStatusProcessHandle
      -- Read git log file and verify plutus.json did not change
      gitLogContents <- decodeUtf8 <$> readFileBS gitLogFilePath
      gitLogContents `shouldNotContain` "plutus.json"
  it "check plutus blueprint hash against code reference" $ do
    withFile "./plutus.json" ReadMode $ \hdl -> do
      plutusJson <- BSL.hGetContents hdl
      let blueprintJSON :: Aeson.Value =
            case Aeson.decode plutusJson of
              Nothing -> error "Invalid blueprint: plutus.json"
              Just value -> value
      let base16Text = blueprintJSON ^. key "validators" . nth 0 . key "hash" . _String
      let plutusScriptHash = commitScriptHash scriptInfo
      base16Text `shouldBe` serialiseToRawBytesHexText plutusScriptHash
  it "Initial validator script" $
    goldenScript "vInitial" Initial.validatorScript
  it "Commit validator script" $
    -- FIXME: Actually test the value of commitValidatorScript
    -- TODO: the script is now double in the repo. Use plutus.json for a golden file
    goldenScript "vCommit" commitValidatorScript
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
      =<< readFileTextEnvelope (AsScript AsPlutusScriptV2) (File fp)
