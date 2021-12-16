-- | A concise interface for building transactions, built on top of the
-- cardano-api.
module Hydra.Ledger.Cardano.Builder where

import Cardano.Api
import Hydra.Prelude

import Data.Default (def)

-- * Types

type TxBuilder = TxBodyContent BuildTx AlonzoEra

-- TODO: This is copied straight from 'cardano-api', could be exposed upstream.
type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

-- * Executing

-- | Construct a transction from a builder. It is said 'unsafe' because the
-- underlying implementation will perform some sanity check on a transaction;
-- for example, check that it has at least one input, that no outputs are
-- negatives and whatnot.
--
-- We use the builder only internally for on-chain transaction crafted in the
-- context of Hydra.
unsafeBuildTransaction :: HasCallStack => TxBuilder -> Tx AlonzoEra
unsafeBuildTransaction builder =
  either
    (\txBodyError -> bug $ InvalidTransactionException{txBodyError, builder})
    (`Tx` mempty)
    . makeTransactionBody
    $ builder

-- | A runtime exception to capture (programmer) failures when building
-- transactions. This should never happened in practice (famous last words...)!
data InvalidTransactionException = InvalidTransactionException
  { txBodyError :: TxBodyError
  , builder :: TxBuilder
  }
  deriving (Show)

instance Exception InvalidTransactionException

-- * Constructing

-- | An empty 'TxBodyContent' with all empty/zero values to be extended using
-- record updates.
--
-- FIXME: 'makeTransactionBody' throws when one tries to build a transaction
-- with scripts but no collaterals. This is unfortunate because collaterals are
-- currently added after by out integrated wallet... We may want to revisit our
-- flow to avoid this exception and have the wallet work from a TxBuilder instead
-- of fiddling with a sealed 'CardanoTx'.
--
-- Similarly, 'makeTransactionBody' throws when building a transaction
-- with scripts and no protocol parameters (needed to compute the script
-- integrity hash). This is also added by our wallet at the moment so
-- hopefully, this ugly work-around will be removed eventually.
--
-- So we currently bypass this by having default but seemingly innofensive
-- values for collaterals and protocol params in the 'empty' value
emptyTxBody :: TxBuilder
emptyTxBody =
  TxBodyContent
    mempty
    (TxInsCollateral CollateralInAlonzoEra mempty)
    mempty
    (TxFeeExplicit TxFeesExplicitInAlonzoEra 0)
    (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    TxMetadataNone
    TxAuxScriptsNone
    TxExtraKeyWitnessesNone
    (BuildTxWith $ Just $ fromLedgerPParams ShelleyBasedEraAlonzo def) -- FIXME
    TxWithdrawalsNone
    TxCertificatesNone
    TxUpdateProposalNone
    TxMintNone
    TxScriptValidityNone

-- | Add new inputs to an ongoing builder.
addInputs :: TxIns BuildTx AlonzoEra -> TxBuilder -> TxBuilder
addInputs ins tx =
  tx{txIns = txIns tx <> ins}

-- | Like 'addInputs' but only for vk inputs which requires no additional data.
addVkInputs :: [TxIn] -> TxBuilder -> TxBuilder
addVkInputs ins =
  addInputs ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> ins)

-- | Append new outputs to an ongoing builder.
addOutputs :: [TxOut CtxTx AlonzoEra] -> TxBuilder -> TxBuilder
addOutputs outputs tx =
  tx{txOuts = txOuts tx <> outputs}
