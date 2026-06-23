module Hydra.ChainObserver.VersionRegistry.Types where

import Hydra.Prelude

import Data.Version (Version)
import Hydra.Cardano.Api (ScriptHash)

data KnownVersion = KnownVersion
  { kvVersion :: Version
  , kvHeadScriptHash :: ScriptHash
  , kvDepositScriptHash :: Maybe ScriptHash
  }
