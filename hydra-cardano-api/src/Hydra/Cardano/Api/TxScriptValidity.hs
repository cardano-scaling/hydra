module Hydra.Cardano.Api.TxScriptValidity where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Tx as Ledger

-- | Convert a cardano-api's 'TxScriptValidity' into a cardano-ledger 'IsValid'
-- boolean wrapper.
toLedgerScriptValidity :: TxScriptValidity Era -> Ledger.IsValid
toLedgerScriptValidity =
  Ledger.IsValid . \case
    TxScriptValidityNone -> True
    TxScriptValidity _ ScriptValid -> True
    TxScriptValidity _ ScriptInvalid -> False
