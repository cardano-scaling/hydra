module Hydra.Ledger where

import Cardano.Prelude

import Cardano.Ledger.Mary (MaryEra)
import Ouroboros.Consensus.Shelley.Protocol (
  StandardCrypto,
 )
import qualified Shelley.Spec.Ledger.API as Ledger
import qualified Shelley.Spec.Ledger.STS.Ledgers as Ledgers

type Era = MaryEra StandardCrypto

-- applyTxs ::
--   LedgerEnv ->
--   Ledger.LedgerState ->
--   Seq Tx ->
--   Either ValidationError (LedgerState l)
-- applyTxs = panice "not implemented"

globals :: Ledger.Globals
globals = panic "undefined"

ledgerEnv :: Ledgers.LedgersEnv Era
ledgerEnv = panic "undefined"

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError

data ValidationError = ValidationError

validateTx :: Ledger.LedgerState Era -> Ledger.Tx Era -> ValidationResult
validateTx ls tx =
  either (Invalid . toValidationError) (const Valid) $
    Ledger.applyTxsTransition globals ledgerEnv (pure tx) ls
 where
  -- toValidationError :: ApplyTxError -> ValidationError
  toValidationError = const ValidationError
