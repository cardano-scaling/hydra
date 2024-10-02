module Hydra.Cardano.Api.Tx (
  -- * Extras
  module Hydra.Cardano.Api.Tx,

  -- * Re-export normal Tx (any era)
  Tx,
)
where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api (
  EraTx (mkBasicTx),
  bodyTxL,
  datsTxWitsL,
  getLanguageView,
  inputsTxBodyL,
  mkBasicTxBody,
  rdmrsTxWitsL,
  scriptIntegrityHashTxBodyL,
  witsTxL,
 )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Babbage.Tx (hashScriptIntegrity)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Control.Lens ((&), (.~), (^.))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Set qualified as Set
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
txSpendingUTxO :: UTxO -> Tx Era
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
utxoProducedByTx :: Tx Era -> UTxO
utxoProducedByTx tx =
  UTxO.fromPairs $
    zip [0 ..] (txOuts body)
      <&> bimap (mkTxIn tx) toCtxUTxOTxOut
 where
  TxBody body = getTxBody tx

-- | Get explicit fees allocated to a transaction.
txFee' :: Tx era -> Coin
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit _ y -> y

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

-- | Compute the integrity hash of a transaction using a list of plutus languages.
recomputeIntegrityHash ::
  (Ledger.AlonzoEraPParams ppera, Ledger.AlonzoEraTxWits txera, Ledger.AlonzoEraTxBody txera, EraTx txera) =>
  Ledger.PParams ppera ->
  [Ledger.Language] ->
  Ledger.Tx txera ->
  Ledger.Tx txera
recomputeIntegrityHash pp languages tx = do
  tx & bodyTxL . scriptIntegrityHashTxBodyL .~ integrityHash
 where
  integrityHash =
    hashScriptIntegrity
      (Set.fromList $ getLanguageView pp <$> languages)
      (tx ^. witsTxL . rdmrsTxWitsL)
      (tx ^. witsTxL . datsTxWitsL)
