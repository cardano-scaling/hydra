module Hydra.Ledger where

import Cardano.Prelude

import Cardano.Ledger.Mary (MaryEra)
import Ouroboros.Consensus.Shelley.Protocol (
  StandardCrypto,
 )
import qualified Shelley.Spec.Ledger.API as Ledger
import qualified Shelley.Spec.Ledger.STS.Ledgers as Ledgers

data Ledger tx = Ledger
  { canApply :: tx -> ValidationResult
  }

newConcreteLedger :: Ledger.ApplyTx era => Ledger.LedgerState era -> Ledger (Ledger.Tx era)
newConcreteLedger st =
  Ledger
    { canApply = validateTx st
    }

type Tx l = Ledger.Tx l -- In fact this is an era only

type Era = MaryEra StandardCrypto

globals :: Ledger.Globals
globals = panic "undefined globals"

ledgerEnv :: Ledgers.LedgersEnv era
ledgerEnv = panic "undefined ledgerEnv"

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving (Eq, Show)

data ValidationError = ValidationError deriving (Eq, Show)

applyTxs ::
  Ledger.LedgerEnv era ->
  Ledger.LedgerState era ->
  Seq (Ledger.Tx era) ->
  Either ValidationError (Ledger.LedgerState l)
applyTxs = panic "should use 'applyTxsTransition' as well"

validateTx :: Ledger.ApplyTx era => Ledger.LedgerState era -> Ledger.Tx era -> ValidationResult
validateTx ls tx =
  either (Invalid . toValidationError) (const Valid) $
    Ledger.applyTxsTransition globals ledgerEnv (pure tx) ls
 where
  -- toValidationError :: ApplyTxError -> ValidationError
  toValidationError = const ValidationError
