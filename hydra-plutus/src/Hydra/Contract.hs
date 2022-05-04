-- | Things related to the Hydra smart contracts / script validators.
module Hydra.Contract where

import Hydra.Prelude

import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Plutus.Orphans ()
import Plutus.V1.Ledger.Api (ValidatorHash)

-- | Information about relevant Hydra scripts.
data ScriptInfo = ScriptInfo
  { initialScriptHash :: ValidatorHash
  , commitScriptHash :: ValidatorHash
  , headScriptHash :: ValidatorHash
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Gather 'ScriptInfo' from the current Hydra scripts. This is useful to
-- determine changes in between version of 'hydra-plutus'.
scriptInfo :: ScriptInfo
scriptInfo =
  ScriptInfo
    { initialScriptHash = Initial.validatorHash
    , commitScriptHash = Commit.validatorHash
    , headScriptHash = Head.validatorHash
    }
