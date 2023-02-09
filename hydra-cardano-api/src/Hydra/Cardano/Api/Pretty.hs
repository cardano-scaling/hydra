module Hydra.Cardano.Api.Pretty where

import Cardano.Binary (decodeAnnotator, serialize, serialize', unsafeDeserialize')
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Babbage.TxBody as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Hydra.Cardano.Api.Prelude

-- | Obtain a human-readable pretty text representation of a transaction.
renderTx :: IsString str => Tx -> str
renderTx = renderTxWithUTxO mempty

renderTxs :: IsString str => [Tx] -> str
renderTxs xs = fromString $ toString $ intercalate "\n\n" (renderTx <$> xs)

-- | Like 'renderTx', but uses the given UTxO to resolve inputs.
renderTxWithUTxO :: IsString str => UTxO -> Tx -> str
renderTxWithUTxO utxo (Tx body _wits) =
  fromString $
    toString $
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
 where
  ShelleyTxBody lbody scripts scriptsData _auxData _validity = body
  outs = Ledger.outputs' lbody
  TxBody content = body

  inputLines =
    "== INPUTS (" <> show (length (txIns content)) <> ")" :
    (("- " <>) . prettyTxIn . fst <$> sortBy (compare `on` fst) (txIns content))

  referenceInputLines =
    "== REFERENCE INPUTS (" <> show (length referenceInputs) <> ")" :
    (("- " <>) . prettyTxIn <$> sort referenceInputs)

  referenceInputs =
    case txInsReference content of
      TxInsReferenceNone -> []
      TxInsReference refInputs -> refInputs

  prettyTxIn i =
    case UTxO.resolve i utxo of
      Nothing -> renderTxIn i
      Just o ->
        renderTxIn i
          <> ("\n      " <> prettyAddr (txOutAddress o))
          <> ("\n      " <> prettyValue 1 (txOutValue o))
          <> ("\n      " <> prettyDatumUtxo (txOutDatum o))

  outputLines =
    [ "== OUTPUTS (" <> show (length (txOuts content)) <> ")"
    , "Total number of assets: " <> show totalNumberOfAssets
    ]
      <> (("- " <>) . prettyOut <$> txOuts content)

  prettyOut o =
    mconcat
      [ prettyAddr (txOutAddress o)
      , "\n      " <> prettyValue 1 (txOutValue o)
      , "\n      " <> prettyDatumCtx (txOutDatum o)
      ]

  prettyAddr = \case
    ShelleyAddressInEra addr -> show addr
    ByronAddressInEra addr -> show addr

  totalNumberOfAssets =
    sum $
      [ foldl' (\n inner -> n + Map.size inner) 0 outer
      | Ledger.TxOut _ (Ledger.Value _ outer) _ _ <- toList outs
      ]

  validityLines =
    [ "== VALIDITY"
    , show (txValidityRange content)
    ]

  mintLines =
    [ "== MINT/BURN\n" <> case txMintValue content of
        TxMintValueNone -> "[]"
        TxMintValue val _ -> prettyValue 0 val
    ]

  prettyValue n =
    T.replace " + " indent . renderValue
   where
    indent = "\n  " <> T.replicate n "    "

  prettyDatumUtxo :: TxOutDatum CtxUTxO -> Text
  prettyDatumUtxo = \case
    TxOutDatumNone ->
      "TxOutDatumNone"
    TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData
    _ -> error "absurd"

  prettyDatumCtx = \case
    TxOutDatumNone ->
      "TxOutDatumNone"
    TxOutDatumHash h ->
      "TxOutDatumHash " <> show h
    TxOutDatumInTx scriptData ->
      "TxOutDatumInTx " <> prettyScriptData scriptData
    TxOutDatumInline scriptData ->
      "TxOutDatumInline " <> prettyScriptData scriptData

  scriptLines =
    [ "== SCRIPTS (" <> show (length scripts) <> ")"
    , "Total size (bytes):  " <> show totalScriptSize
    ]
      <> (("- " <>) . prettyScript <$> scripts)

  totalScriptSize = sum $ BL.length . serialize <$> scripts

  prettyScript (fromLedgerScript -> script) =
    "Script (" <> scriptHash <> ")"
   where
    scriptHash =
      show (Ledger.hashScript @(ShelleyLedgerEra Era) (toLedgerScript script))

  datumLines = case scriptsData of
    TxBodyNoScriptData -> []
    (TxBodyScriptData (Ledger.TxDats dats) _) ->
      "== DATUMS (" <> show (length dats) <> ")" :
      (("- " <>) . showDatumAndHash <$> Map.toList dats)

  showDatumAndHash (k, v) =
    mconcat
      [ show (Ledger.extractHash k)
      , "\n  "
      , prettyScriptData (fromLedgerData v)
      ]

  prettyScriptData =
    decodeUtf8 . Aeson.encode . scriptDataToJson ScriptDataJsonNoSchema

  redeemerLines = case scriptsData of
    TxBodyNoScriptData -> []
    (TxBodyScriptData _ re) ->
      let rdmrs = Map.toList $ Ledger.unRedeemers re
       in "== REDEEMERS (" <> show (length rdmrs) <> ")" :
          (("- " <>) . prettyRedeemer <$> rdmrs)

  prettyRedeemer (Ledger.RdmrPtr tag ix, (redeemerData, redeemerBudget)) =
    unwords
      [ show tag <> "#" <> show ix
      , mconcat
          [ "( cpu = " <> show (Ledger.exUnitsSteps redeemerBudget)
          , ", mem = " <> show (Ledger.exUnitsMem redeemerBudget) <> " )"
          ]
      , "\n  " <> prettyScriptData (fromLedgerData redeemerData)
      ]

  requiredSignersLines =
    "== REQUIRED SIGNERS" : case txExtraKeyWits content of
      TxExtraKeyWitnessesNone -> ["[]"]
      TxExtraKeyWitnesses xs -> ("- " <>) . show <$> xs

deriving newtype instance ToJSON UTxO

deriving newtype instance FromJSON UTxO

instance ToCBOR UTxO where
  toCBOR = toCBOR . toLedgerUTxO
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR UTxO where
  fromCBOR = fromLedgerUTxO <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

instance Arbitrary UTxO where
  shrink = shrinkUTxO
  arbitrary = genUTxOAlonzo
