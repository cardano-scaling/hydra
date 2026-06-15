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

import Data.ByteString qualified as BS
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
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as HeadState
import Hydra.Contract.HeadTokens qualified as HeadTokens
import Hydra.Version (gitDescribe)
import PlutusLedgerApi.V3 (Data (..), TxId (..), TxOutRef (..), serialiseCompiledCode, toData)
import PlutusTx.Builtins (toBuiltin)
import System.Process.Typed (runProcess_, shell)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
    , testCase "Increment redeemer has constructor index 0 (deposit.ak invariant)" $ do
        -- deposit.ak checks the head redeemer constructor index directly:
        --   builtin.un_constr_data(redeemer).1st == 0
        -- If 'Increment' ever shifts away from index 0, deposits become permanently
        -- locked. This test catches that before it reaches the chain.
        let someRef = TxOutRef (TxId . toBuiltin $ BS.replicate 32 0) 0
            incrementInput =
              HeadState.Increment
                HeadState.IncrementRedeemer
                  { HeadState.signature = []
                  , HeadState.snapshotNumber = 0
                  , HeadState.increment = someRef
                  }
        case toData incrementInput of
          Constr n _ -> n @?= 0
          _ -> assertFailure "Increment redeemer did not serialise to a Constr"
    , testCase "Deposit redeemer constructor indices match deposit.ak" $ do
        -- deposit.ak redefines the deposit 'Redeemer' type structurally and
        -- matches on its constructors. If these indices ever drift, deposit
        -- and recover validation break. Keep in sync with Hydra.Contract.Deposit.
        case toData Deposit.Claim of
          Constr n _ -> n @?= 0
          _ -> assertFailure "Claim did not serialise to a Constr"
        case toData (Deposit.Recover 1) of
          Constr n _ -> n @?= 1
          _ -> assertFailure "Recover did not serialise to a Constr"
    , testCase "Commit has constructor index 0 (deposit.ak invariant)" $ do
        -- deposit.ak redefines 'Commit' structurally and hashes its fields, so
        -- its constructor index must stay 0.
        let someRef = TxOutRef (TxId . toBuiltin $ BS.replicate 32 0) 0
            commit = Commit.Commit someRef (toBuiltin BS.empty)
        case toData commit of
          Constr n _ -> n @?= 0
          _ -> assertFailure "Commit did not serialise to a Constr"
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
