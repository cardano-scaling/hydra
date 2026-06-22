{-# LANGUAGE TemplateHaskell #-}

module Hydra.ChainObserver.VersionRegistry where

import Hydra.Prelude

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map.Strict qualified as Map
import Data.Version (Version, parseVersion)
import Hydra.Cardano.Api (ScriptHash, deserialiseFromRawBytesHex)
import Text.ParserCombinators.ReadP (readP_to_S)

data KnownVersion = KnownVersion
  { kvVersion :: Version
  , kvHeadScriptHash :: ScriptHash
  , kvDepositScriptHash :: Maybe ScriptHash
  }

networksJsonBytes :: ByteString
networksJsonBytes = $(makeRelativeToProject "../hydra-node/networks.json" >>= embedFile)

-- | All known head (and deposit) validator script hashes, parsed from the
-- embedded networks.json at compile time.
loadKnownVersions :: [KnownVersion]
loadKnownVersions =
  either (error . toText . ("VersionRegistry: " <>)) id $ do
    val <-
      maybe (Left "Invalid JSON in networks.json") Right $
        Aeson.decodeStrict networksJsonBytes
    parseEither parseVersions val
 where
  parseVersions :: Aeson.Value -> Parser [KnownVersion]
  parseVersions = Aeson.withObject "networks.json" $ \o -> do
    headHashes :: Map Text Text <- o .: "scriptHashes"
    depositHashes :: Map Text Text <- fromMaybe mempty <$> o .:? "depositScriptHashes"
    mapM (parseEntry depositHashes) (Map.toList headHashes)

  parseEntry :: Map Text Text -> (Text, Text) -> Parser KnownVersion
  parseEntry depositHashes (versionStr, headHashStr) = do
    kvVersion <-
      case [v | (v, "") <- readP_to_S parseVersion (toString versionStr)] of
        [v] -> pure v
        _ -> fail $ "Invalid version string: " <> toString versionStr
    kvHeadScriptHash <- parseScriptHash versionStr headHashStr
    kvDepositScriptHash <- case Map.lookup versionStr depositHashes of
      Nothing -> pure Nothing
      Just s -> Just <$> parseScriptHash versionStr s
    pure KnownVersion{kvVersion, kvHeadScriptHash, kvDepositScriptHash}

  parseScriptHash :: Text -> Text -> Parser ScriptHash
  parseScriptHash versionStr hashStr =
    case deserialiseFromRawBytesHex (encodeUtf8 hashStr) of
      Left err -> fail $ "Invalid script hash for " <> toString versionStr <> ": " <> show err
      Right sh -> pure sh
