{-# LANGUAGE TemplateHaskell #-}

module Hydra.NodeVersion where

import Hydra.Prelude hiding (encodeUtf8)

import Data.Version (Version (..), showVersion)
import Hydra.Version (embeddedRevision, gitRevision, unknownVersion)
import Paths_hydra_node (version)

hydraNodeVersion :: Version
hydraNodeVersion =
  version & \(Version semver _) -> Version semver revision
 where
  revision =
    maybeToList $
      embeddedRevision
        <|> gitRevision
        <|> Just unknownVersion
