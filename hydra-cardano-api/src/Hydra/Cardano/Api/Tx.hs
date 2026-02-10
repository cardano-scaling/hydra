module Hydra.Cardano.Api.Tx (
  -- * Extras
  module Hydra.Cardano.Api.Tx,

  -- * Re-export normal Tx (any era)
  Tx,
)
where

import "base" Data.Bifunctor (bimap)
import "base" Data.Functor ((<&>))
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-api" Cardano.Ledger.Api (

import Hydra.Cardano.Api.Prelude
  EraTx (mkBasicTx),
  inputsTxBodyL,
  mkBasicTxBody,
 )
import "cardano-ledger-api" Cardano.Ledger.Api qualified as Ledger
import "containers" Data.Set qualified as Set
import "lens" Control.Lens ((&), (.~))

import Hydra.Cardano.Api.TxIn (mkTxIn, toLedgerTxIn)

-- * Extras

-- | Sign transaction using the provided secret key
-- It only works for tx not containing scripts.
-- You can't sign a script utxo with this.
signTx ::
  IsShelleyBasedEra era =>
  SigningKey PaymentKey ->
  Tx era ->
  Tx era
signTx signingKey (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentKey signingKey)

-- | Create a transaction spending all given `UTxO`.
txSpendingUTxO :: UTxO Era -> Tx Era
txSpendingUTxO utxo =
  fromLedgerTx $
    mkBasicTx
      ( mkBasicTxBody
          & inputsTxBodyL .~ (toLedgerTxIn `Set.map` inputs)
      )
 where
  inputs = UTxO.inputSet utxo

-- | Get the UTxO that are produced by some transaction.
-- XXX: Defined here to avoid cyclic module dependency
utxoProducedByTx :: Tx Era -> UTxO Era
utxoProducedByTx tx =
  UTxO.fromList $
    zip [0 ..] (txOuts body)
      <&> bimap (mkTxIn tx) toCtxUTxOTxOut
 where
  body = getTxBodyContent $ getTxBody tx

-- * Type Conversions

-- | Convert a cardano-api 'Tx' into a matching cardano-ledger 'Tx'.
toLedgerTx ::
  Tx era ->
  Ledger.Tx (ShelleyLedgerEra era)
toLedgerTx (ShelleyTx _era tx) = tx

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx ::
  IsShelleyBasedEra era =>
  Ledger.Tx (ShelleyLedgerEra era) ->
  Tx era
fromLedgerTx =
  ShelleyTx shelleyBasedEra
