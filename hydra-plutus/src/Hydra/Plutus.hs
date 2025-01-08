{-# LANGUAGE TemplateHaskell #-}

-- | Module to load and provide the Hydra scripts.
--
-- The plutus blueprint in 'plutus.json' is embedded in the binary and serves as
-- the ground truth for validator scripts and hashes.
--
-- NOTE: All scripts are PlutusV3 scripts (defined by the 'PlutusScript' synonym pattern).
--
-- XXX: We are using a hardcoded indices to access validators in plutus.json.
-- This is fragile and depends on the validator names not changing.
module Hydra.Plutus where

import Hydra.Prelude

import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth, _String)
import Data.ByteString.Base16 qualified as Base16
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Hydra.Cardano.Api (PlutusScript, pattern PlutusScriptSerialised)
import PlutusCore.Core (plcVersion110)
import PlutusCore.MkPlc qualified as UPLC
import PlutusLedgerApi.Common (serialiseUPLC, toBuiltin, toData, uncheckedDeserialiseUPLC)
import PlutusLedgerApi.V3 (ScriptHash (..))
import UntypedPlutusCore qualified as UPLC

-- | Loads the embedded "plutus.json" blueprint and provides the decoded JSON.
blueprintJSON :: Aeson.Value
blueprintJSON =
  case Aeson.decodeStrict $(makeRelativeToProject "./plutus.json" >>= embedFile) of
    Nothing -> error "Invalid blueprint: plutus.json"
    Just value -> value

-- | Get the commit validator by decoding it from 'blueprintJSON'.
commitValidatorScript :: PlutusScript
commitValidatorScript =
  PlutusScriptSerialised . toShort . Base16.decodeLenient . encodeUtf8 $
    blueprintJSON ^. key "validators" . nth 0 . key "compiledCode" . _String

-- | Get the commit validator hash from 'blueprintJSON'.
commitValidatorScriptHash :: ScriptHash
commitValidatorScriptHash =
  ScriptHash . toBuiltin . Base16.decodeLenient . encodeUtf8 $
    blueprintJSON ^. key "validators" . nth 0 . key "hash" . _String

-- | Get the initial validator by decoding the parameterized initial validator
-- from the 'blueprintJSON' and applying the 'commitValidatorScriptHash' to it.
initialValidatorScript :: PlutusScript
initialValidatorScript =
  PlutusScriptSerialised $ serialiseUPLC appliedProgram
 where
  appliedProgram = case unappliedProgram `UPLC.applyProgram` argumentProgram of
    Left e -> error $ "Failed to applyProgram: " <> show e
    Right x -> x

  unappliedProgram = uncheckedDeserialiseUPLC unappliedScript

  argumentProgram =
    UPLC.Program () plcVersion110 $
      UPLC.mkConstant () $
        toData commitValidatorScriptHash

  unappliedScript =
    toShort . Base16.decodeLenient . encodeUtf8 $
      blueprintJSON ^. key "validators" . nth 4 . key "compiledCode" . _String

-- | Get the deposit validator by decoding it from 'blueprintJSON'.
depositValidatorScript :: PlutusScript
depositValidatorScript =
  PlutusScriptSerialised . toShort . Base16.decodeLenient . encodeUtf8 $
    blueprintJSON ^. key "validators" . nth 2 . key "compiledCode" . _String
