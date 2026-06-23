{-# LANGUAGE TemplateHaskell #-}

module Hydra.ChainObserver.VersionRegistry.Internal where

import Hydra.Prelude

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Map.Strict qualified as Map
import Data.Version (Version (..), parseVersion)
import Hydra.Cardano.Api (ScriptHash, deserialiseFromRawBytesHex, serialiseToRawBytesHex)
import Hydra.ChainObserver.VersionRegistry.Types (KnownVersion (..))
import Language.Haskell.TH qualified as TH
import Text.ParserCombinators.ReadP (readP_to_S)

parseNetworksJson :: ByteString -> Either String [KnownVersion]
parseNetworksJson bytes = do
  val <- Aeson.eitherDecodeStrict' bytes
  parseEither parseVersions val

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

-- Generates a Q Exp for a ScriptHash by embedding its hex representation as a
-- string literal and reconstructing the hash at runtime via OverloadedStrings.
-- The deserialise call cannot fail: we are re-encoding a hash that was already
-- validated at compile time by parseScriptHash above.
scriptHashExp :: ScriptHash -> TH.Q TH.Exp
scriptHashExp sh = do
  let hexStr = toString (decodeUtf8 (serialiseToRawBytesHex sh) :: Text) :: String
      errMsg = "VersionRegistry: script hash " <> hexStr <> " from networks.json could not be deserialized: "
  [|
    case deserialiseFromRawBytesHex $(TH.litE (TH.stringL hexStr)) of
      Right sh' -> sh'
      Left err -> error ($(TH.litE (TH.stringL errMsg)) <> show err)
    |]

versionExp :: Version -> TH.Q TH.Exp
versionExp (Version branch tags) = do
  let branchE = TH.listE (map (TH.litE . TH.integerL . fromIntegral) branch)
      tagsE = TH.listE (map (TH.litE . TH.stringL) tags)
  TH.appsE [TH.conE 'Version, branchE, tagsE]

-- Uses positional constructor application to avoid quoting field names, which
-- runs into GHC 9.6 staging restrictions with FieldSelectors.
knownVersionExp :: KnownVersion -> TH.Q TH.Exp
knownVersionExp KnownVersion{kvVersion, kvHeadScriptHash, kvDepositScriptHash} = do
  versionE <- versionExp kvVersion
  headHashE <- scriptHashExp kvHeadScriptHash
  depositE <- case kvDepositScriptHash of
    Nothing -> TH.conE 'Nothing
    Just sh -> TH.appE (TH.conE 'Just) (scriptHashExp sh)
  TH.appsE
    [ TH.conE 'KnownVersion
    , pure versionE
    , pure headHashE
    , pure depositE
    ]

knownVersionListExp :: [KnownVersion] -> TH.Q TH.Exp
knownVersionListExp = TH.listE . map knownVersionExp
