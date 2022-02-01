module Hydra.Cardano.Api.TxScriptValidity where

import Cardano.Api
import Hydra.Prelude

import qualified Cardano.Ledger.Alonzo.Tx as Ledger

toLedgerScriptValidity :: TxScriptValidity AlonzoEra -> Ledger.IsValid
toLedgerScriptValidity =
  Ledger.IsValid . \case
    TxScriptValidityNone -> True
    TxScriptValidity _ ScriptValid -> True
    TxScriptValidity _ ScriptInvalid -> False
