{-# LANGUAGE TemplateHaskell #-}

module Hydra.ChainObserver.VersionRegistry (
  KnownVersion (..),
  loadKnownVersions,
) where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Data.FileEmbed (makeRelativeToProject)
import Hydra.ChainObserver.VersionRegistry.Internal (knownVersionListExp, parseNetworksJson)
import Hydra.ChainObserver.VersionRegistry.Types (KnownVersion (..))
import Language.Haskell.TH (runIO)

-- | All known head (and deposit) validator script hashes, parsed from
-- networks.json at compile time. A malformed networks.json is a build error.
loadKnownVersions :: [KnownVersion]
loadKnownVersions =
  $( do
      path <- makeRelativeToProject "../hydra-node/networks.json"
      bytes <- runIO $ BS.readFile path
      case parseNetworksJson bytes of
        Left err -> fail ("VersionRegistry: " <> err)
        Right vs -> knownVersionListExp vs
   )
