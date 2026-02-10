-- | Pretty printing transactions and utxo's
module Hydra.Cardano.Api.Pretty where

import "aeson" Data.Aeson qualified as Aeson
import "base" Data.Function (on)
import "base" Data.List (intercalate, sort, sortBy)
import "base" GHC.IsList (IsList (..))
import "bytestring" Data.ByteString.Lazy qualified as BL
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-binary" Cardano.Binary (serialize)
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Core qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Plutus.Data qualified as Ledger
import "containers" Data.Map.Strict qualified as Map
import "text" Data.Text qualified as T

import Hydra.Cardano.Api qualified as Api
import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.ScriptData (fromLedgerData)

-- | Obtain a human-readable pretty text representation of a transaction.
renderTx :: Api.Tx -> String
renderTx = renderTxWithUTxO mempty

renderUTxO :: (TxIn, TxOut ctx era) -> Text
renderUTxO (k, TxOut _ (txOutValueToValue -> v) _ _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> renderValue v

-- | Like 'renderTx', but uses the given UTxO to resolve inputs.
renderTxWithUTxO :: UTxO Era -> Api.Tx -> String
renderTxWithUTxO utxo (Tx body _wits) =
  unlines $
    intercalate
      [""]
      [ pure $ show (getTxId body)
      , inputLines
      , collateralInputLines
      , referenceInputLines
      , outputLines
      , totalCollateralLines
      , returnCollateralLines
      , feeLines
      , validityLines
      , mintLines
      , scriptLines
      , datumLines
      , redeemerLines
      , requiredSignersLines
      , metadataLines
      ]
 where
  Api.ShelleyTxBody _lbody scripts scriptsData _auxData _validity = body
  outs = txOuts content
  content = getTxBodyContent body

  inputLines =
    "== INPUTS (" <> show (length (txIns content)) <> ")"
      : (("- " <>) . prettyTxIn . fst <$> sortBy (compare `on` fst) (txIns content))

  referenceInputLines =
    "== REFERENCE INPUTS (" <> show (length referenceInputs) <> ")"
      : (("- " <>) . prettyTxIn <$> sort referenceInputs)

  referenceInputs =
    case txInsReference content of
      Api.TxInsReferenceNone -> []
      Api.TxInsReference refInputs _ -> refInputs

  collateralInputLines =
    "== COLLATERAL INPUTS (" <> show (length collateralInputs) <> ")"
      : (("- " <>) . prettyTxIn <$> sort collateralInputs)

  collateralInputs =
    case txInsCollateral content of
      Api.TxInsCollateralNone -> []
      Api.TxInsCollateral refInputs -> refInputs

  prettyTxIn i =
    case UTxO.resolveTxIn i utxo of
      Nothing -> T.unpack $ renderTxIn i
      Just o ->
        T.unpack (renderTxIn i)
          <> ("\n      " <> prettyAddr (Api.txOutAddress o))
          <> ("\n      " <> prettyValue 1 (Api.txOutValue o))
          <> ("\n      " <> prettyDatumUtxo (Api.txOutDatum o))
          <> ("\n      " <> prettyReferenceScript (Api.txOutReferenceScript o))

  outputLines :: [String]
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
     in length $ toList totalValue

  totalCollateralLines :: [String]
  totalCollateralLines =
    [ "== TOTAL COLLATERAL"
    , show $ txTotalCollateral content
    ]

  returnCollateralLines :: [String]
  returnCollateralLines =
    [ "== RETURN COLLATERAL"
    , show $ txReturnCollateral content
    ]

  feeLines :: [String]
  feeLines =
    [ "== FEE"
    , show $ txFee content
    ]

  validityLines :: [String]
  validityLines =
    [ "== VALIDITY"
    , show (txValidityLowerBound content)
    , show (txValidityUpperBound content)
    ]

  mintLines =
    [ "== MINT/BURN\n" <> prettyValue 0 (txMintValueToValue $ txMintValue content)
    ]

  prettyValue n =
    T.unpack . T.replace " + " indent . renderValue
   where
    indent = "\n  " <> T.replicate n "    "

  prettyDatumUtxo :: Api.TxOutDatum ctx -> String
  prettyDatumUtxo = \case
    TxOutDatumNone ->
      "TxOutDatumNone"
    Api.TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    Api.TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData
    _ -> error "absurd"

  prettyReferenceScript = \case
    Api.ReferenceScriptNone ->
      "ReferenceScriptNone"
    (Api.ReferenceScript (Api.ScriptInAnyLang l s)) ->
      "ReferenceScript " <> show l <> " " <> show (serialiseToRawBytesHexText (hashScript s))

  prettyDatumCtx = \case
    Api.TxOutDatumNone ->
      "TxOutDatumNone"
    Api.TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    Api.TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData
    Api.TxOutSupplementalDatum scriptData ->
      "TxOutSupplementalDatum " <> prettyScriptData scriptData

  scriptLines =
    [ "== SCRIPTS (" <> show (length scripts) <> ")"
    , "Total size (bytes):  " <> show totalScriptSize
    ]
      <> (("- " <>) . prettyScript <$> scripts)

  totalScriptSize = sum $ BL.length . serialize <$> scripts

  prettyScript :: Ledger.Script LedgerEra -> String
  prettyScript script =
    "Script (" <> show (Ledger.hashScript script) <> ")"

  datumLines = case scriptsData of
    Api.TxBodyNoScriptData -> []
    (Api.TxBodyScriptData (Ledger.TxDats dats) _) ->
      "== DATUMS (" <> show (length dats) <> ")"
        : (("- " <>) . showDatumAndHash <$> Map.toList dats)

  showDatumAndHash :: (Ledger.SafeHash i, Ledger.Data era) -> String
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

  prettyRedeemer :: forall p era. Show p => (p, (Ledger.Data era, Ledger.ExUnits)) -> String
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
