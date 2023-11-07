module Hydra.Cardano.Api.PolicyId where

import Hydra.Cardano.Api.Prelude

import Cardano.Ledger.Hashes qualified as Ledger

toLedgerScriptHash :: PolicyId -> Ledger.ScriptHash StandardCrypto
toLedgerScriptHash (PolicyId scriptHash) = toShelleyScriptHash scriptHash
