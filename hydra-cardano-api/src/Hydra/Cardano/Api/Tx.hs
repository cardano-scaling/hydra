{-# LANGUAGE TypeApplications #-}

module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.KeyWitness (
  fromLedgerTxWitness,
  toLedgerBootstrapWitness,
  toLedgerKeyWitness,
 )
import Hydra.Cardano.Api.TxScriptValidity (toLedgerScriptValidity)

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Alonzo.TxWits as Ledger
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Ledger (Tx, hashScript)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Hydra.Cardano.Api.TxIn (mkTxIn)

-- * Extras

-- | Sign transaction using the provided secret key
-- It only works for tx not containing scripts.
-- You can't sign a script utxo with this.
signTx ::
  IsShelleyBasedEra era =>
  Either (SigningKey PaymentKey) (SigningKey PaymentExtendedKey) ->
  Tx era ->
  Tx era
signTx eSigningKey (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness =
    case eSigningKey of
      Left signingKey -> makeShelleyKeyWitness body (WitnessPaymentKey signingKey)
      Right signingKeyExtended -> makeShelleyKeyWitness body (WitnessPaymentExtendedKey signingKeyExtended)

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
--
-- NOTE: this function is partial and throws if given a Byron transaction for
-- which fees are necessarily implicit.
txFee' :: HasCallStack => Tx era -> Lovelace
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit TxFeesExplicitInShelleyEra fee -> fee
    TxFeeExplicit TxFeesExplicitInAllegraEra fee -> fee
    TxFeeExplicit TxFeesExplicitInMaryEra fee -> fee
    TxFeeExplicit TxFeesExplicitInAlonzoEra fee -> fee
    TxFeeExplicit TxFeesExplicitInBabbageEra fee -> fee
    TxFeeExplicit TxFeesExplicitInConwayEra fee -> fee
    TxFeeImplicit _ -> error "impossible: TxFeeImplicit on non-Byron transaction."

-- * Type Conversions

-- | Convert a cardano-api 'Tx' into a cardano-ledger 'Tx' in the Babbage era
toLedgerTx :: Tx Era -> Ledger.Tx (ShelleyLedgerEra Era)
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Redeemers mempty)
     in -- XXX: The suggested way (by the ledger team) forward is to use
        -- mkBasicTxBody and lenses to construct ledger transactions.
        Ledger.AlonzoTx
          { Ledger.body =
              body
          , Ledger.isValid =
              toLedgerScriptValidity validity
          , Ledger.auxiliaryData =
              maybeToStrictMaybe auxData
          , Ledger.wits =
              Ledger.AlonzoTxWits
                { Ledger.txwitsVKey =
                    toLedgerKeyWitness vkWits
                , Ledger.txwitsBoot =
                    toLedgerBootstrapWitness vkWits
                , Ledger.txscripts =
                    Map.fromList
                      [ ( Ledger.hashScript @(ShelleyLedgerEra Era) s
                        , s
                        )
                      | s <- scripts
                      ]
                , Ledger.txdats =
                    datums
                , Ledger.txrdmrs =
                    redeemers
                }
          }

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx :: Ledger.Tx (ShelleyLedgerEra Era) -> Tx Era
fromLedgerTx ledgerTx =
  Tx
    (ShelleyTxBody era body scripts scriptsData (strictMaybeToMaybe auxData) validity)
    (fromLedgerTxWitness wits)
 where
  -- XXX: The suggested way (by the ledger team) forward is to use lenses to
  -- introspect ledger transactions.
  Ledger.AlonzoTx body wits isValid auxData = ledgerTx

  era =
    ShelleyBasedEraBabbage

  scripts =
    Map.elems $ Ledger.txscripts' wits

  scriptsData =
    TxBodyScriptData
      ScriptDataInBabbageEra
      (Ledger.txdats' wits)
      (Ledger.txrdmrs' wits)

  validity = case isValid of
    Ledger.IsValid True ->
      TxScriptValidity TxScriptValiditySupportedInBabbageEra ScriptValid
    Ledger.IsValid False ->
      TxScriptValidity TxScriptValiditySupportedInBabbageEra ScriptInvalid
