{-# LANGUAGE TemplateHaskell #-}

-- | Module to load and provide the Hydra scripts.
module Hydra.Plutus where

import Hydra.Prelude

import Control.Lens ((^.))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, nth, _String)
import qualified Data.ByteString.Base16 as Base16
import Data.FileEmbed (embedFile, makeRelativeToProject)
import PlutusLedgerApi.Common (SerialisedScript)

-- | Loads the "plutus.json" blueprint and provides the decoded JSON.
blueprintJSON :: Aeson.Value
blueprintJSON =
  case Aeson.decodeStrict $(makeRelativeToProject "./plutus.json" >>= embedFile) of
    Nothing -> error "Invalid blueprint: plutus.json"
    Just value -> value

-- | Access the commit validator script from the 'blueprintJSON'.
-- REVIEW: make validator access less fragile?
commitValidatorScript :: SerialisedScript
commitValidatorScript =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode commit validator: " <> show e
    Right bytes -> toShort bytes
 where
  base16Bytes = encodeUtf8 base16Text

  base16Text = blueprintJSON ^. key "validators" . nth 0 . key "compiledCode" . _String
