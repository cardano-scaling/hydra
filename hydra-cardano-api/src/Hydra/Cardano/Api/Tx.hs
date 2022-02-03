module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.KeyWitness (
  fromLedgerTxWitness,
  toLedgerBootstrapWitness,
  toLedgerKeyWitness,
 )
import Hydra.Cardano.Api.Lovelace (fromLedgerCoin)
import Hydra.Cardano.Api.TxScriptValidity (toLedgerScriptValidity)
import Hydra.Cardano.Api.Value (txOutValue)

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

-- * Extra

-- | Get explicit fees allocated to a transaction.
--
-- NOTE: this function is partial and throws if given a Byron transaction for
-- which fees are necessarily implicit.
txFee' :: Tx Era -> Lovelace
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit TxFeesExplicitInAlonzoEra fee -> fee
    TxFeeImplicit _ -> error "impossible: TxFeeImplicit on non-Byron transaction."

{-# DEPRECATED getFee "use txFee' instead." #-}
getFee :: Tx Era -> Lovelace
getFee = txFee'

-- | Calculate the total execution cost of a transaction, according to the
-- budget assigned to each redeemer.
totalExecutionCost ::
  Ledger.PParams LedgerEra ->
  Tx Era ->
  Lovelace
totalExecutionCost pparams tx =
  fromLedgerCoin (Ledger.txscriptfee (Ledger._prices pparams) executionUnits)
 where
  executionUnits = foldMap snd $ Ledger.unRedeemers $ Ledger.txrdmrs wits
  Ledger.ValidatedTx{Ledger.wits = wits} = toLedgerTx tx

executionCost ::
  Ledger.PParams LedgerEra ->
  Tx Era ->
  Lovelace
executionCost = totalExecutionCost
{-# DEPRECATED executionCost "use totalExecutionCost instead." #-}

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

  prettyScript = show . (Ledger.hashScript @LedgerEra)

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

{-# DEPRECATED describeCardanoTx "use 'renderTx' instead." #-}
describeCardanoTx :: Tx Era -> Text
describeCardanoTx = renderTx

-- * Type Conversions

-- | Convert a cardano-api's 'Tx' into a cardano-ledger's 'Tx' in the Alonzo era
-- (a.k.a. 'ValidatedTx').
toLedgerTx :: Tx Era -> Ledger.ValidatedTx LedgerEra
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
                      [ ( Ledger.hashScript @LedgerEra s
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
fromLedgerTx :: Ledger.ValidatedTx LedgerEra -> Tx Era
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
