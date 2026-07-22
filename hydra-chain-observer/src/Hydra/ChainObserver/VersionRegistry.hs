{-# LANGUAGE TemplateHaskell #-}

module Hydra.ChainObserver.VersionRegistry (
  InvalidScriptHashesFile (..),
  KnownVersion (..),
  loadKnownVersions,
  loadKnownVersionsFromFile,
) where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Data.FileEmbed (makeRelativeToProject)
import Hydra.ChainObserver.VersionRegistry.Internal (knownVersionListExp, parseScriptHashesJson)
import Hydra.ChainObserver.VersionRegistry.Types (KnownVersion (..))
import Language.Haskell.TH (runIO)

-- | All known head (and deposit) validator script hashes, parsed from
-- script-hashes.json at compile time. A malformed script-hashes.json is a build error.
loadKnownVersions :: [KnownVersion]
loadKnownVersions =
  $( do
      path <- makeRelativeToProject "script-hashes.json"
      bytes <- runIO $ BS.readFile path
      case parseScriptHashesJson bytes of
        Left err -> fail ("VersionRegistry: " <> err)
        Right vs -> knownVersionListExp vs
   )

-- | Thrown when a user-provided script hashes file cannot be parsed.
newtype InvalidScriptHashesFile = InvalidScriptHashesFile String
  deriving stock (Show)

instance Exception InvalidScriptHashesFile

-- | Load known script hashes at runtime from a file in the same format as
-- script-hashes.json. Complementary to the built-in 'loadKnownVersions': it
-- allows observing heads that use scripts not (yet) in the released registry.
loadKnownVersionsFromFile :: FilePath -> IO [KnownVersion]
loadKnownVersionsFromFile path =
  BS.readFile path
    >>= either (throwIO . InvalidScriptHashesFile) pure
    . parseScriptHashesJson
