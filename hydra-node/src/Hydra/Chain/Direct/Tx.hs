-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Api (AlonzoEra, TxBody, TxBodyError)
import Hydra.Chain (HeadParameters)

type Era = AlonzoEra

initTx :: HeadParameters -> Either TxBodyError (TxBody Era)
initTx = undefined
