{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compile on Linux and export the code consumed by the Windows build.
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (encodeFile)
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (pattern PlutusScriptSerialised)
import Hydra.Contract (hydraScriptCatalogue)
import Hydra.Contract.CRS qualified as CRS
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadTokens qualified as HeadTokens
import PlutusCore.Flat (flat)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V3 (TxId (..), TxOutRef (..), toBuiltin)
import PlutusTx qualified
import PlutusTx.Code (CompiledCode, CompiledCodeIn (SerializedCode), getPir, getPlc)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import UntypedPlutusCore (UnrestrictedProgram (..))

main :: IO ()
main = do
  [directory] <- getArgs
  createDirectoryIfMissing True directory
  writeCode (directory </> "head.flat") Head.unappliedValidator
  writeCode (directory </> "mint.flat") HeadTokens.unappliedMintingPolicy
  writeCode (directory </> "crs.flat") CRS.crsValidatorScript
  checkMintingPolicies directory
  encodeFile (directory </> "linux-script-catalogue.json") hydraScriptCatalogue

-- loadFromFile/SerializedCode expects Flat with named de Bruijn indices,
-- whereas the public script format is CBOR-wrapped Flat without names.
writeCode :: FilePath -> CompiledCode a -> IO ()
writeCode path code = do
  -- Plutus 1.68 requires PIR on both operands when applying code. Keep it for
  -- the Head's CRS parameter and for each runtime minting-policy seed.
  pir <- maybe (fail $ "Missing PIR: " <> path) (pure . flat) (getPir code)
  let bytes = flat (UnrestrictedProgram (getPlc code))
      restored = SerializedCode bytes (Just pir) mempty
  unless (serialiseCompiledCode restored == serialiseCompiledCode code) $
    fail $
      "Script export changes bytes: " <> path
  BS.writeFile path bytes
  BS.writeFile (path <> ".pir") pir

checkMintingPolicies :: FilePath -> IO ()
checkMintingPolicies directory = do
  bytes <- BS.readFile (directory </> "mint.flat")
  pir <- BS.readFile (directory </> "mint.flat.pir")
  let restored = SerializedCode bytes (Just pir) mempty
  forM_ [0, 1, 42, 255] $ \n -> do
    let seed = TxOutRef (TxId (toBuiltin (BS.replicate 32 n))) (fromIntegral n)
        actual =
          PlutusScriptSerialised $
            serialiseCompiledCode $
              restored `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 seed
    unless (actual == HeadTokens.mintingPolicyScript seed) $
      fail $
        "Export changes minting policy for seed " <> show n
