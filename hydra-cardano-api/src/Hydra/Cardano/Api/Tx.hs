module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.KeyWitness (
  fromLedgerTxWitness,
  toLedgerBootstrapWitness,
  toLedgerKeyWitness,
 )
import Hydra.Cardano.Api.Lovelace (fromLedgerCoin)
import Hydra.Cardano.Api.TxScriptValidity (toLedgerScriptValidity)

import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo as Ledger
import qualified Cardano.Ledger.Alonzo.PParams as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)

-- * Extras

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
    TxFeeImplicit _ -> error "impossible: TxFeeImplicit on non-Byron transaction."

-- | Calculate the total execution cost of a transaction, according to the
-- budget assigned to each redeemer.
totalExecutionCost ::
  Ledger.PParams (ShelleyLedgerEra era) ->
  Tx era ->
  Lovelace
totalExecutionCost pparams tx =
  fromLedgerCoin (Ledger.txscriptfee (Ledger._prices pparams) executionUnits)
 where
  executionUnits =
    case tx of
      Tx (ShelleyTxBody _ _ _ (TxBodyScriptData _ _ redeemers) _ _) _ ->
        foldMap snd (Ledger.unRedeemers redeemers)
      _ ->
        mempty

-- | Find and deserialise from 'ScriptData', a redeemer from the transaction
-- associated to the given input.
findRedeemerSpending ::
  forall a era.
  ( FromScriptData a
  , Ledger.Era (ShelleyLedgerEra era)
  , HasField
      "inputs"
      (Ledger.Core.TxBody (ShelleyLedgerEra era))
      (Set (Ledger.TxIn StandardCrypto))
  ) =>
  Tx era ->
  TxIn ->
  Maybe a
findRedeemerSpending (getTxBody -> ByronTxBody{}) _ = do
  Nothing
findRedeemerSpending (getTxBody -> ShelleyTxBody _ body _ scriptData _ _) txIn = do
  idx <- Set.lookupIndex (toLedgerTxIn txIn) (getField @"inputs" body)
  let ptr = RdmrPtr Ledger.Spend $ fromIntegral idx
  (d, _exUnits) <- Map.lookup ptr redeemers
  Plutus.fromData $ Ledger.getPlutusData d
 where
  redeemers = case scriptData of
    TxBodyNoScriptData ->
      mempty
    TxBodyScriptData _ _ (Ledger.Redeemers rs) ->
      rs

-- | Obtain a human-readable pretty text representation of a transaction.
renderTx :: Tx Era -> Text
renderTx (Tx body _wits) =
  unlines $
    [show (getTxId body)]
      <> inputLines
      <> outputLines
      <> scriptLines
      <> datumLines
      <> redeemerLines
 where
  ShelleyTxBody _era lbody scripts scriptsData _auxData _validity = body
  outs = Ledger.outputs' lbody
  TxBody TxBodyContent{txIns, txOuts} = body

  inputLines =
    "  Input set (" <> show (length txIns) <> ")" :
    (("    - " <>) . renderTxIn . fst <$> txIns)

  outputLines =
    [ "  Outputs (" <> show (length txOuts) <> ")"
    , "    total number of assets: " <> show totalNumberOfAssets
    ]
      <> (("    - " <>) . renderValue . txOutValue <$> txOuts)

  txOutValue (TxOut _ value _) =
    txOutValueToValue value

  totalNumberOfAssets =
    sum $
      [ foldl' (\n inner -> n + Map.size inner) 0 outer
      | Ledger.TxOut _ (Ledger.Value _ outer) _ <- toList outs
      ]

  scriptLines =
    [ "  Scripts (" <> show (length scripts) <> ")"
    , "    total size (bytes):  " <> show totalScriptSize
    ]
      <> (("    - " <>) . prettyScript <$> scripts)

  prettyScript = show . (Ledger.hashScript @(ShelleyLedgerEra Era))

  totalScriptSize = sum $ BL.length . serialize <$> scripts

  datumLines = case scriptsData of
    TxBodyNoScriptData -> []
    (TxBodyScriptData _ (Ledger.TxDats dats) _) ->
      "  Datums (" <> show (length dats) <> ")" :
      (("    - " <>) . showDatumAndHash <$> Map.toList dats)

  showDatumAndHash (k, v) = show k <> " -> " <> show v

  redeemerLines = case scriptsData of
    TxBodyNoScriptData -> []
    (TxBodyScriptData _ _ re) ->
      let rdmrs = Map.elems $ Ledger.unRedeemers re
       in "  Redeemers (" <> show (length rdmrs) <> ")" :
          (("    - " <>) . show . fst <$> rdmrs)

-- * Type Conversions

-- | Convert a cardano-api's 'Tx' into a cardano-ledger's 'Tx' in the Alonzo era
-- (a.k.a. 'ValidatedTx').
toLedgerTx :: Tx Era -> Ledger.ValidatedTx (ShelleyLedgerEra Era)
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Redeemers mempty)
     in Ledger.ValidatedTx
          { Ledger.body =
              body
          , Ledger.isValid =
              toLedgerScriptValidity validity
          , Ledger.auxiliaryData =
              maybeToStrictMaybe auxData
          , Ledger.wits =
              Ledger.TxWitness
                { Ledger.txwitsVKey =
                    toLedgerKeyWitness vkWits
                , Ledger.txwitsBoot =
                    toLedgerBootstrapWitness vkWits
                , Ledger.txscripts =
                    fromList
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

-- | Convert a cardano-ledger's 'Tx' in the Alonzo era (a.k.a. 'ValidatedTx')
-- into a cardano-api's 'Tx'.
fromLedgerTx :: Ledger.ValidatedTx (ShelleyLedgerEra Era) -> Tx Era
fromLedgerTx (Ledger.ValidatedTx body wits isValid auxData) =
  Tx
    (ShelleyTxBody era body scripts scriptsData (strictMaybeToMaybe auxData) validity)
    (fromLedgerTxWitness wits)
 where
  era =
    ShelleyBasedEraAlonzo
  scripts =
    Map.elems $ Ledger.txscripts' wits
  scriptsData =
    TxBodyScriptData
      ScriptDataInAlonzoEra
      (Ledger.txdats' wits)
      (Ledger.txrdmrs' wits)
  validity = case isValid of
    Ledger.IsValid True ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptValid
    Ledger.IsValid False ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptInvalid
