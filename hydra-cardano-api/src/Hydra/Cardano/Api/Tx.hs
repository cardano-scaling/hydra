module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.KeyWitness (
  fromLedgerTxWitness,
  toLedgerBootstrapWitness,
  toLedgerKeyWitness,
 )
import Hydra.Cardano.Api.TxScriptValidity (toLedgerScriptValidity)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Api.Tx (
  EraTx (mkBasicTx),
  addrTxWitsL,
  auxDataTxL,
  bootAddrTxWitsL,
  datsTxWitsL,
  hashScriptTxWitsL,
  isValidTxL,
  mkBasicTxWits,
  rdmrsTxWitsL,
  witsTxL,
 )
import Cardano.Ledger.Babbage.Tx qualified as Ledger
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Control.Lens ((&), (.~))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Hydra.Cardano.Api.TxIn (mkTxIn)

-- * Extras

-- | Convert a transaction of between cardano eras by re-encoding transactions
-- and relying on the fact that ledger transactions are usually backward
-- compatible. Expect this to succeed, for example, for a Babbage transaction
-- that was included Conway block, which is then to be converted back to Babbage
-- by this function.
convertTx ::
  forall eraFrom eraTo.
  (IsShelleyBasedEra eraFrom, IsShelleyBasedEra eraTo) =>
  Tx eraFrom ->
  Maybe (Tx eraTo)
convertTx tx =
  case deserialiseFromCBOR (proxyToAsType (Proxy @(Tx eraTo))) bytes of
    Left _err -> Nothing
    Right tx' -> Just tx'
 where
  bytes = serialiseToCBOR tx

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
  forall era.
  ( Ledger.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  , Ledger.AlonzoEraTx (ShelleyLedgerEra era)
  ) =>
  Tx era ->
  Ledger.Tx (ShelleyLedgerEra era)
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Redeemers mempty)
        wits =
          mkBasicTxWits
            & addrTxWitsL .~ toLedgerKeyWitness vkWits
            & bootAddrTxWitsL .~ toLedgerBootstrapWitness vkWits
            & hashScriptTxWitsL .~ scripts
            & datsTxWitsL .~ datums
            & rdmrsTxWitsL .~ redeemers
     in mkBasicTx body
          & isValidTxL .~ toLedgerScriptValidity validity
          & auxDataTxL .~ maybeToStrictMaybe auxData
          & witsTxL .~ wits

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx :: Ledger.Tx (ShelleyLedgerEra Era) -> Tx Era
fromLedgerTx ledgerTx =
  Tx
    (ShelleyTxBody shelleyBasedEra body scripts scriptsData (strictMaybeToMaybe auxData) validity)
    (fromLedgerTxWitness wits)
 where
  -- XXX: The suggested way (by the ledger team) forward is to use lenses to
  -- introspect ledger transactions.
  Ledger.AlonzoTx body wits isValid auxData = ledgerTx

  scripts =
    Map.elems $ Ledger.txscripts' wits

  scriptsData :: TxBodyScriptData Era
  scriptsData =
    TxBodyScriptData
      alonzoEraOnwards
      (Ledger.txdats' wits)
      (Ledger.txrdmrs' wits)

  validity = case isValid of
    Ledger.IsValid True ->
      TxScriptValidity alonzoEraOnwards ScriptValid
    Ledger.IsValid False ->
      TxScriptValidity alonzoEraOnwards ScriptInvalid
