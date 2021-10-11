module Main where

import Hydra.Prelude

import Cardano.Api (HasTextEnvelope, serialiseToTextEnvelope)
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
  writeEnvelopes (scripts policyId)

  putTextLn "Serialise datums:"
  writeEnvelopes datums

  putTextLn "Serialise redeemers:"
  writeEnvelopes redeemers
 where
  writeEnvelopes :: HasTextEnvelope item => [(item, String)] -> IO ()
  writeEnvelopes plutus =
    forM_ plutus $ \(item, itemName) -> do
      let itemFile = itemName <> ".plutus"
          serialised = Aeson.encode $ serialiseToTextEnvelope (Just $ fromString itemName) item
      BL.writeFile itemFile serialised
      putTextLn $ "  " <> pack itemFile <> ":     " <> sizeInKb serialised

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
