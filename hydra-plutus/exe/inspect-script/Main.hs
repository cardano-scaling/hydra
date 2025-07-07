module Main where

import Hydra.Cardano.Api
    ( fromPlutusData,
      hashScriptDataBytes,
      scriptDataToJson,
      unsafeHashableScriptData,
      toPlutusCurrencySymbol,
      toPlutusTxOutRef,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema),
      TxId(TxId),
      TxIn(TxIn),
      TxIx(TxIx) )
import Hydra.Prelude
    ( ($),
      Integral(div),
      Semigroup((<>)),
      String,
      IO,
      encodePretty,
      forM_,
      (.),
      putLBSLn,
      putTextLn,
      show,
      ToText(toText) )

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (pack)
import Hydra.Cardano.Api.Prelude (unsafeHashFromBytes)
import Hydra.Contract (scriptInfo)
import Hydra.Contract.HeadState as Head
    ( Input(Abort), State(Final, Initial) )
import Hydra.Contract.HeadTokens qualified as HeadTokens
import PlutusLedgerApi.V3 (Data, toData)

-- | Serialise Hydra scripts to files for submission through cardano-cli.
main :: IO ()
main = do
  putTextLn "Script info:"
  putLBSLn $ encodePretty scriptInfo

  putTextLn "Serialise datums:"
  writeData datums

  putTextLn "Serialise redeemers:"
  writeData redeemers

  putTextLn "Datum hashes:"
  forM_ datums $ \(aDatum, datumName) ->
    putTextLn $
      toText $
        datumName
          <> ": "
          <> show (hashScriptDataBytes . unsafeHashableScriptData $ fromPlutusData aDatum)
 where
  writeData :: [(Data, String)] -> IO ()
  writeData plutus =
    forM_ plutus $ \(item, itemName) -> do
      let itemFile = itemName <> ".data"
          serialised =
            Aeson.encode
              . scriptDataToJson ScriptDataJsonDetailedSchema
              . unsafeHashableScriptData
              $ fromPlutusData item
      BL.writeFile itemFile serialised
      putTextLn $ "  " <> pack itemFile <> ":     " <> sizeInKb serialised

  sizeInKb = (<> " KB") . show . (`div` 1024) . BL.length

  datums =
    [ (headDatum, "headDatum")
    , (abortDatum, "abortDatum")
    ]

  headDatum = toData $ Head.Initial 1_000_000_000_000 [] (toPlutusCurrencySymbol $ HeadTokens.headPolicyId someTxIn) (toPlutusTxOutRef someTxIn)

  someTxIn = TxIn (TxId $ unsafeHashFromBytes "01234567890123456789012345678901") (TxIx 1)

  abortDatum = toData Head.Final

  redeemers = [(headRedeemer, "headRedeemer")]

  headRedeemer = toData Head.Abort
