module Hydra.Cardano.Api.PolicyId where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Hashes as Ledger

toLedgerScriptHash :: PolicyId -> Ledger.ScriptHash StandardCrypto
toLedgerScriptHash (PolicyId scriptHash) = toShelleyScriptHash scriptHash
