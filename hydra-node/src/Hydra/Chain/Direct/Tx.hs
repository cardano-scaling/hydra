-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.TxBody (TxBody (TxBody))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (ValidityInterval))
import qualified Data.Set as Set
import Hydra.Chain (HeadParameters)
import Shelley.Spec.Ledger.API (Coin (Coin), StrictMaybe (SNothing), TxIn, Wdrl (Wdrl))

-- * Hydra Head transactions

initTx :: HeadParameters -> TxIn StandardCrypto -> TxBody (AlonzoEra StandardCrypto)
initTx _ feeInput =
  TxBody
    (Set.singleton feeInput) -- inputs
    mempty -- collateral
    mempty -- outputs
    mempty -- txcerts
    (Wdrl mempty) -- txwdrls
    (Coin 0) -- txfee
    (ValidityInterval SNothing SNothing) -- txvldt
    SNothing -- txUpdates
    mempty -- reqSignerHashes
    mempty -- mint
    SNothing -- scriptIntegrityHash
    SNothing -- adHash
    SNothing -- txnetworkid
