{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ScriptHash where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

-- * Extras

-- | Like 'hashScript', but for a 'ScriptInAnyLang'.
hashScriptInAnyLang :: ScriptInAnyLang -> ScriptHash
hashScriptInAnyLang (ScriptInAnyLang _ script) =
  hashScript script
