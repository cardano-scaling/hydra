module Hydra.Cardano.Api.TxScriptValidity where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.Tx as Ledger

toLedgerScriptValidity :: TxScriptValidity Era -> Ledger.IsValid
toLedgerScriptValidity =
  Ledger.IsValid . \case
    TxScriptValidityNone -> True
    TxScriptValidity _ ScriptValid -> True
    TxScriptValidity _ ScriptInvalid -> False
