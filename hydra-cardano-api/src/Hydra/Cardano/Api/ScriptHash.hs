{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ScriptHash where

import Hydra.Cardano.Api.Prelude
import PlutusLedgerApi.Data.V1 qualified as Plutus

-- * Extras

-- | Like 'hashScript', but for a 'ScriptInAnyLang'.
hashScriptInAnyLang :: ScriptInAnyLang -> ScriptHash
hashScriptInAnyLang (ScriptInAnyLang _ script) =
  hashScript script

toPlutusScriptHash :: ScriptHash -> Plutus.ScriptHash
toPlutusScriptHash = Plutus.ScriptHash . Plutus.toBuiltin . serialiseToRawBytes
