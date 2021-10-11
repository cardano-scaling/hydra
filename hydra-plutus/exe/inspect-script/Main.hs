module Main where

import Hydra.Prelude

import Cardano.Api (serialiseToTextEnvelope)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import Hydra.Contract.Commit as Commit
import Hydra.Contract.Head as Head
import Hydra.Contract.Initial as Initial
import Ledger.Value

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
  putTextLn "Serialise scripts"
  forM_ (scripts policyId) $ \(script, scriptName) -> do
    let scriptFile = scriptName <> ".plutus"
    BL.writeFile scriptFile script
    putTextLn $ "  " <> pack scriptFile <> ":     " <> sizeInKb (headScript policyId)
 where
  sizeInKb = (<> " KB") . show . (`div` 1024) . BL.length

  scripts policyId =
    [ (headScript policyId, "headScript")
    , (initialScript, "initialScript")
    , (commitScript, "commitScript")
    ]

  headScript policyId =
    Aeson.encode $
      serialiseToTextEnvelope Nothing $ Head.validatorScript policyId

  commitScript =
    Aeson.encode $
      serialiseToTextEnvelope Nothing Commit.validatorScript

  initialScript =
    Aeson.encode $
      serialiseToTextEnvelope Nothing Initial.validatorScript
