module Main where

import Hydra.Prelude

import Cardano.Api (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), scriptDataToJson, serialiseToTextEnvelope)
import Cardano.Api.Shelley (fromPlutusData)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import Data.Text.Prettyprint.Doc.Extras (pretty)
import Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Hash as Hash
import Hydra.Contract.Head as Head
import Hydra.Contract.Initial as Initial
import Ledger (Datum (..), datumHash)
import Ledger.Scripts (Script, toCardanoApiScript)
import Ledger.Value
import Plutus.V1.Ledger.Api (Data, dataToBuiltinData, toData)
import PlutusTx (getPlc)
import PlutusTx.Code (CompiledCode)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

-- | Serialise Hydra scripts to files for submission through cardano-cli.
-- This small utility is useful to manually construct transactions payload for Hydra on-chain
-- protocol. It takes as arguments the currency and token name to be used as unique Head
-- identifier, both of which can simply be a UTXO in the network that's consumed by the
-- init transaction.
main :: IO ()
main = do
  [currency, token] <- getArgs
  let (policyId, _) =
        first
          currencyMPSHash
          ( unAssetClass $
              assetClass (currencySymbol $ encodeUtf8 currency) (tokenName $ encodeUtf8 token)
          )
  putTextLn "Serialise scripts:"
  writeScripts (scripts policyId)

  putTextLn "Compile scripts:"
  writePlc (compiledScripts policyId)

  putTextLn "Serialise datums:"
  writeData datums

  putTextLn "Serialise redeemers:"
  writeData redeemers

  putTextLn "Datum hashes:"
  forM_ datums $ \(aDatum, datumName) ->
    putTextLn $ toText $ datumName <> ": " <> show (datumHash $ Datum $ dataToBuiltinData $ aDatum)
 where
  writeScripts :: [(Script, String)] -> IO ()
  writeScripts plutus =
    forM_ plutus $ \(item, itemName) -> do
      let itemFile = itemName <> ".plutus"
          serialised = Aeson.encode $ serialiseToTextEnvelope (Just $ fromString itemName) $ toCardanoApiScript item
      BL.writeFile itemFile serialised
      putTextLn $ "  " <> pack itemFile <> ":     " <> sizeInKb serialised

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

  scripts policyId =
    [ (headScript policyId, "headScript")
    , (initialScript, "initialScript")
    , (commitScript, "commitScript")
    , (hashScript, "hashScript")
    ]

  headScript policyId = Head.validatorScript policyId

  commitScript = Commit.validatorScript

  initialScript = Initial.validatorScript

  hashScript = Hash.validatorScript

  compiledScripts policyId =
    [ (Compiled $ Head.compiledValidator policyId, "headScript")
    , (Compiled Initial.compiledValidator, "initialScript")
    , (Compiled Commit.compiledValidator, "commitScript")
    ]

  datums =
    [ (headDatum, "headDatum")
    , (abortDatum, "abortDatum")
    ]

  headDatum = toData $ Head.Initial 1_000_000_000_000 []

  abortDatum = toData Head.Final

  redeemers = [(headRedeemer, "headRedeemer")]

  headRedeemer = toData Head.Abort

data Compiled = forall a. Compiled {compiledCode :: CompiledCode a}
