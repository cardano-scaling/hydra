module Main where

import Prelude

import Codec.Serialise
import Hydra.Contract.Initial as Initial
import Hydra.Contract.OnChain
import Ledger.Typed.Scripts
import Ledger.Value

import qualified Data.ByteString.Lazy as BL
import qualified Hydra.ContractSM as HydraSM

main :: IO ()
main = do
  putStrLn $ "Hydra Script:       " <> sizeInKb hydraScript
  putStrLn $ "Initial Script:     " <> sizeInKb initialScript
  putStrLn $ "Commit Script:      " <> sizeInKb commitScript
  putStrLn $ ""
  putStrLn $ "Hydra Script (SM):  " <> sizeInKb hydraScriptSM
  putStrLn $ "Initial Script (2): " <> sizeInKb initialScript'
 where
  sizeInKb = (<> " KB") . show . (`div` 1024) . BL.length

  hydraScript =
    serialise $ validatorScript hydraTypedValidator
  initialScript =
    serialise $ validatorScript initialTypedValidator
  commitScript =
    serialise $ validatorScript commitTypedValidator

  initialScript' =
    serialise $ validatorScript Initial.typedValidator
  hydraScriptSM =
    serialise $ validatorScript $ HydraSM.typedValidator asset
   where
    asset = assetClass (CurrencySymbol mempty) (TokenName mempty)
