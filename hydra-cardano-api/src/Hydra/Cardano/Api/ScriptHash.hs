{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ScriptHash where

import Hydra.Cardano.Api.Prelude

-- * Extras

-- | Like 'hashScript', but for a 'ScriptInAnyLang'.
hashScriptInAnyLang :: ScriptInAnyLang -> ScriptHash
hashScriptInAnyLang (ScriptInAnyLang _ script) =
  hashScript script
