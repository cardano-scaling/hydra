module Main where

import Hydra.Prelude

import Cardano.Api (serialiseToTextEnvelope)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import Hydra.Contract.Commit as Commit
import Hydra.Contract.Head as Head
import Hydra.Contract.Initial as Initial
import Ledger (Datum (..))
import Ledger.Value
import Plutus.V1.Ledger.Api (Redeemer (Redeemer), dataToBuiltinData, toData)

-- | Serialise Hydra scripts to files for submission through cardano-cli
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
  forM_ (scripts policyId) $ \(script, scriptName) -> do
    let scriptFile = scriptName <> ".plutus"
        serialised = Aeson.encode $ serialiseToTextEnvelope (Just $ fromString scriptName) script
    BL.writeFile scriptFile serialised
    putTextLn $ "  " <> pack scriptFile <> ":     " <> sizeInKb serialised

  putTextLn "Serialise datums:"
  forM_ datums $ \(aDatum, datumName) -> do
    let datumFile = datumName <> ".plutus"
        serialised = Aeson.encode $ serialiseToTextEnvelope (Just $ fromString datumName) aDatum
    BL.writeFile datumFile serialised
    putTextLn $ "  " <> pack datumFile <> ":     " <> sizeInKb serialised

  putTextLn "Serialise redeemers:"
  forM_ redeemers $ \(aRedeemer, redeemerName) -> do
    let redeemerFile = redeemerName <> ".plutus"
        serialised = Aeson.encode $ serialiseToTextEnvelope (Just $ fromString redeemerName) aRedeemer
    BL.writeFile redeemerFile serialised
    putTextLn $ "  " <> pack redeemerFile <> ":     " <> sizeInKb serialised
 where
  sizeInKb = (<> " KB") . show . (`div` 1024) . BL.length

  scripts policyId =
    [ (headScript policyId, "headScript")
    , (initialScript, "initialScript")
    , (commitScript, "commitScript")
    ]

  headScript policyId = Head.validatorScript policyId

  commitScript = Commit.validatorScript

  initialScript = Initial.validatorScript

  datums =
    [ (headDatum, "headDatum")
    , (abortDatum, "abortDatum")
    ]

  headDatum = Datum $ dataToBuiltinData $ toData $ Head.Initial 1_000_000_000_000 []

  abortDatum = Datum $ dataToBuiltinData $ toData $ Head.Final

  redeemers = [(headRedeemer, "headRedeemer")]

  headRedeemer = Redeemer $ dataToBuiltinData $ toData Head.Abort
