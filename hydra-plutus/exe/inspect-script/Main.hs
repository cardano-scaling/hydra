{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Cardano.Api
import Hydra.Prelude

import Codec.Serialise (serialise)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import Hydra.Cardano.Api.Prelude (unsafeHashFromBytes)
import Hydra.Contract (scriptInfo)
import Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Hash as Hash
import Hydra.Contract.Head as Head
import Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.HeadTokens as HeadTokens
import Hydra.Contract.Initial as Initial
import Plutus.V2.Ledger.Api (Data, Script, toData)
import PlutusTx (getPlc)
import PlutusTx.Code (CompiledCode)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

-- | Serialise Hydra scripts to files for submission through cardano-cli.
-- This small utility is useful to manually construct transactions payload for Hydra on-chain
-- protocol.
main :: IO ()
main = do
  putTextLn "Script info:"
  putLBSLn $ encodePretty scriptInfo

  putTextLn "Serialise scripts:"
  writeScripts scripts

  putTextLn "Compile scripts:"
  writePlc compiledScripts

  putTextLn "Serialise datums:"
  writeData datums

  putTextLn "Serialise redeemers:"
  writeData redeemers

  putTextLn "Datum hashes:"
  forM_ datums $ \(aDatum, datumName) ->
    putTextLn $ toText $ datumName <> ": " <> show (hashScriptData $ fromPlutusData aDatum)
 where
  writeScripts :: [(Plutus.V2.Ledger.Api.Script, String)] -> IO ()
  writeScripts plutus =
    forM_ plutus $ \(item, itemName) -> do
      let itemFile = itemName <> ".plutus"
          serialised =
            Aeson.encode $
              serialiseToTextEnvelope (Just $ fromString itemName) $
                fromPlutusScript @PlutusScriptV2 item
      BL.writeFile itemFile serialised
      putTextLn $ "  " <> pack itemFile <> ":     " <> sizeInKb (serialise item)

  writePlc :: [(Compiled, String)] -> IO ()
  writePlc plutus =
    forM_ plutus $ \(Compiled item, itemName) -> do
      let itemFile = itemName <> ".plc"
          serialised = encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ pretty $ getPlc item
      BL.writeFile itemFile serialised
      putTextLn $ "  " <> pack itemFile <> ":     " <> sizeInKb serialised

  writeData :: [(Data, String)] -> IO ()
  writeData plutus =
    forM_ plutus $ \(item, itemName) -> do
      let itemFile = itemName <> ".data"
          serialised = Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData item
      BL.writeFile itemFile serialised
      putTextLn $ "  " <> pack itemFile <> ":     " <> sizeInKb serialised

  sizeInKb = (<> " KB") . show . (`div` 1024) . BL.length

  scripts =
    [ (headScript, "headScript")
    , (initialScript, "initialScript")
    , (commitScript, "commitScript")
    , (hashPlutusScript, "hashScript")
    ]

  headScript = Head.validatorScript

  commitScript = Commit.validatorScript

  initialScript = Initial.validatorScript

  hashPlutusScript = Hash.validatorScript

  compiledScripts =
    [ (Compiled Head.compiledValidator, "headScript")
    , (Compiled Initial.compiledValidator, "initialScript")
    , (Compiled Commit.compiledValidator, "commitScript")
    ]

  datums =
    [ (headDatum, "headDatum")
    , (abortDatum, "abortDatum")
    ]

  headDatum = toData $ Head.Initial 1_000_000_000_000 [] (toPlutusCurrencySymbol $ HeadTokens.headPolicyId $ someTxIn) (toPlutusTxOutRef someTxIn)

  someTxIn = TxIn (TxId $ unsafeHashFromBytes "01234567890123456789012345678901") (TxIx 1)

  abortDatum = toData Head.Final

  redeemers = [(headRedeemer, "headRedeemer")]

  headRedeemer = toData Head.Abort

data Compiled = forall a. Compiled {compiledCode :: CompiledCode a}
