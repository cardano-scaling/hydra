{-# LANGUAGE TemplateHaskell #-}

-- | Module to load and provide the Hydra scripts.
module Hydra.Plutus where

import Hydra.Prelude

import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, nth, _String)
import Data.ByteString.Base16 qualified as Base16
import Data.FileEmbed (embedFile, makeRelativeToProject)
import PlutusLedgerApi.Common (SerialisedScript)

-- | Loads the "plutus.json" blueprint and provides the decoded JSON.
blueprintJSON :: Aeson.Value
blueprintJSON =
  case Aeson.decodeStrict $(makeRelativeToProject "./plutus.json" >>= embedFile) of
    Nothing -> error "Invalid blueprint: plutus.json"
    Just value -> value

-- | Access the commit validator script from the 'blueprintJSON'.
commitValidatorScript :: SerialisedScript
commitValidatorScript =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode commit validator: " <> show e
    Right bytes -> toShort bytes
 where
  base16Bytes = encodeUtf8 base16Text
  -- NOTE: we are using a hardcoded index to access the commit validator.
  -- This is fragile and will raise problems when we move another plutus validator
  -- to Aiken.
  -- Reference: https://github.com/cardano-foundation/CIPs/tree/master/CIP-0057
  base16Text = blueprintJSON ^. key "validators" . nth 0 . key "compiledCode" . _String

-- | Access the initial validator script from the 'blueprintJSON'.
initialValidatorScript :: SerialisedScript
initialValidatorScript =
  case Base16.decode base16Bytes of
    Left e -> error $ "Failed to decode initial validator: " <> show e
    Right bytes -> toShort bytes
 where
  base16Bytes = encodeUtf8 base16Text

  -- NOTE: we are using a hardcoded index to access the commit validator.
  -- This is fragile and will raise problems when we move another plutus validator
  -- to Aiken.
  -- Reference: https://github.com/cardano-foundation/CIPs/tree/master/CIP-0057
  base16Text = blueprintJSON ^. key "validators" . nth 2 . key "compiledCode" . _String
