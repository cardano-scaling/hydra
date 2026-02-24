{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell helpers for loading genesis files at compile time.
module Test.Hydra.Ledger.Cardano.Fixtures.TH (
  loadCostModelTH,
) where

import Hydra.Prelude hiding (lookup)

import Data.Aeson (Object, Value (..), eitherDecodeFileStrict)
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax qualified as TH
import System.FilePath ((</>))

import Cardano.Ledger.Alonzo.Scripts (CostModel, mkCostModel)
import Cardano.Ledger.Plutus (Language (..))

-- | Template Haskell function to load a cost model from a genesis JSON file at compile time.
--
-- The genesis files are located in the genesis/ directory within hydra-tx.
-- Source: Official Cardano mainnet genesis files from book.world.dev.cardano.org
loadCostModelTH :: FilePath -> String -> Language -> Q Exp
loadCostModelTH genesisFile key lang = do
  -- Construct path relative to hydra-tx package root
  let genesisFp = "genesis" </> genesisFile

  -- Read and parse the JSON file at compile time
  result <- runIO $ eitherDecodeFileStrict genesisFp
  case result of
    Left err -> fail $ "Failed to parse " <> genesisFile <> ": " <> err
    Right (Object obj) -> do
      case KeyMap.lookup (Aeson.fromString key) obj of
        Just (Array arr) -> do
          -- Convert the JSON array to a list of integers
          let costModelValues = [round n :: Integer | Number n <- toList arr]
          -- Build Language expression manually (Language doesn't have Lift instance)
          langExp <- case lang of
            PlutusV1 -> [| PlutusV1 |]
            PlutusV2 -> [| PlutusV2 |]
            PlutusV3 -> [| PlutusV3 |]
          -- Lift and splice the values directly into the generated code
          [| either (error . show) id $ mkCostModel $(pure langExp) $(TH.lift costModelValues) |]
        Just _ -> fail $ key <> " in " <> genesisFile <> " is not an array"
        Nothing -> fail $ key <> " not found in " <> genesisFile
    Right _ -> fail $ genesisFile <> " does not contain a JSON object"
