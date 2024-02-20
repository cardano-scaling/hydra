-- | Pretty printing transactions and utxo's
module Hydra.Cardano.Api.Pretty where

import Hydra.Cardano.Api qualified as Api
import Hydra.Cardano.Api.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.SafeHash qualified as Ledger
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Function (on)
import Data.List (intercalate, sort, sortBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hydra.Cardano.Api.ScriptData (fromLedgerData)

-- | Obtain a human-readable pretty text representation of a transaction.
renderTx :: Api.Tx -> String
renderTx = renderTxWithUTxO mempty

renderTxs :: [Api.Tx] -> String
renderTxs xs = intercalate "\n\n" (renderTx <$> xs)

-- | Like 'renderTx', but uses the given UTxO to resolve inputs.
renderTxWithUTxO :: UTxO -> Api.Tx -> String
renderTxWithUTxO utxo (Tx body _wits) =
  unlines $
    [show (getTxId body)]
      <> [""]
      <> inputLines
      <> [""]
      <> referenceInputLines
      <> [""]
      <> outputLines
      <> [""]
      <> validityLines
      <> [""]
      <> mintLines
      <> [""]
      <> scriptLines
      <> [""]
      <> datumLines
      <> [""]
      <> redeemerLines
      <> [""]
      <> requiredSignersLines
      <> [""]
      <> metadataLines
 where
  Api.ShelleyTxBody _lbody scripts scriptsData _auxData _validity = body
  outs = txOuts content
  TxBody content = body

  inputLines =
    "== INPUTS (" <> show (length (txIns content)) <> ")"
      : (("- " <>) . prettyTxIn . fst <$> sortBy (compare `on` fst) (txIns content))

  referenceInputLines =
    "== REFERENCE INPUTS (" <> show (length referenceInputs) <> ")"
      : (("- " <>) . prettyTxIn <$> sort referenceInputs)

  referenceInputs =
    case txInsReference content of
      Api.TxInsReferenceNone -> []
      Api.TxInsReference refInputs -> refInputs

  prettyTxIn i =
    case UTxO.resolve i utxo of
      Nothing -> T.unpack $ renderTxIn i
      Just o ->
        T.unpack (renderTxIn i)
          <> ("\n      " <> prettyAddr (Api.txOutAddress o))
          <> ("\n      " <> prettyValue 1 (Api.txOutValue o))
          <> ("\n      " <> prettyDatumUtxo (Api.txOutDatum o))

  outputLines =
    [ "== OUTPUTS (" <> show (length outs) <> ")"
    , "Total number of assets: " <> show totalNumberOfAssets
    ]
      <> (("- " <>) . prettyOut <$> outs)

  prettyOut o =
    mconcat
      [ prettyAddr (Api.txOutAddress o)
      , "\n      " <> prettyValue 1 (Api.txOutValue o)
      , "\n      " <> prettyDatumCtx (Api.txOutDatum o)
      ]

  prettyAddr = \case
    Api.ShelleyAddressInEra addr -> show addr
    Api.ByronAddressInEra addr -> show addr

  totalNumberOfAssets =
    let totalValue = foldMap Api.txOutValue outs
     in length $ valueToList totalValue

  validityLines =
    [ "== VALIDITY"
    , show (txValidityLowerBound content)
    , show (txValidityUpperBound content)
    ]

  mintLines =
    [ "== MINT/BURN\n" <> case txMintValue content of
        Api.TxMintValueNone -> "[]"
        Api.TxMintValue val _ -> prettyValue 0 val
    ]

  prettyValue n =
    T.unpack . T.replace " + " indent . renderValue
   where
    indent = "\n  " <> T.replicate n "    "

  prettyDatumUtxo = \case
    TxOutDatumNone ->
      "TxOutDatumNone"
    Api.TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    Api.TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData
    _ -> error "absurd"

  prettyDatumCtx = \case
    Api.TxOutDatumNone ->
      "TxOutDatumNone"
    Api.TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    Api.TxOutDatumInTx scriptData ->
      "TxOutDatumInTx " <> prettyScriptData scriptData
    Api.TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData

  scriptLines =
    [ "== SCRIPTS (" <> show (length scripts) <> ")"
    , "Total size (bytes):  " <> show totalScriptSize
    ]
      <> (("- " <>) . prettyScript <$> scripts)

  totalScriptSize = sum $ BL.length . serialize <$> scripts

  prettyScript script =
    "Script (" <> show (Ledger.hashScript script) <> ")"

  datumLines = case scriptsData of
    Api.TxBodyNoScriptData -> []
    (Api.TxBodyScriptData (Ledger.TxDats dats) _) ->
      "== DATUMS (" <> show (length dats) <> ")"
        : (("- " <>) . showDatumAndHash <$> Map.toList dats)

  showDatumAndHash (k, v) =
    mconcat
      [ show (Ledger.extractHash k)
      , "\n  "
      , prettyScriptData (fromLedgerData v)
      ]

  prettyScriptData =
    T.unpack . decodeUtf8 . BL.toStrict . Aeson.encode . scriptDataToJson ScriptDataJsonNoSchema

  redeemerLines = case scriptsData of
    Api.TxBodyNoScriptData -> []
    (Api.TxBodyScriptData _ re) ->
      let rdmrs = Map.toList $ Ledger.unRedeemers re
       in "== REDEEMERS (" <> show (length rdmrs) <> ")"
            : (("- " <>) . prettyRedeemer <$> rdmrs)

  prettyRedeemer (purpose, (redeemerData, redeemerBudget)) =
    unwords
      [ show purpose
      , mconcat
          [ "( cpu = " <> show (Ledger.exUnitsSteps redeemerBudget)
          , ", mem = " <> show (Ledger.exUnitsMem redeemerBudget) <> " )"
          ]
      , "\n  " <> prettyScriptData (fromLedgerData redeemerData)
      ]

  requiredSignersLines =
    "== REQUIRED SIGNERS" : case txExtraKeyWits content of
      Api.TxExtraKeyWitnessesNone -> ["[]"]
      Api.TxExtraKeyWitnesses xs -> ("- " <>) . show <$> xs

  metadataLines =
    [ "== METADATA"
    , show (txMetadata content)
    ]
