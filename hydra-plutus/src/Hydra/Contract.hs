{-# LANGUAGE PatternSynonyms #-}

-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import Hydra.Cardano.Api (ScriptHash, fromPlutusScript, hashScript, pattern PlutusScript)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial

-- | Information about relevant Hydra scripts.
data ScriptInfo = ScriptInfo
  { initialScriptHash :: ScriptHash
  , commitScriptHash :: ScriptHash
  , headScriptHash :: ScriptHash
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Gather 'ScriptInfo' from the current Hydra scripts. This is useful to
-- determine changes in between version of 'hydra-plutus'.
scriptInfo :: ScriptInfo
scriptInfo =
  ScriptInfo
    { initialScriptHash = plutusScriptHash Initial.validatorScript
    , commitScriptHash = plutusScriptHash Commit.validatorScript
    , headScriptHash = plutusScriptHash Head.validatorScript
    }
 where
  plutusScriptHash = hashScript . PlutusScript . fromPlutusScript
