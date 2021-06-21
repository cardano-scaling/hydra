{-# LANGUAGE TemplateHaskell #-}

-- | Provides version number with optional Git SHA1.
module Hydra.Node.Version (
  -- * Values computed at compile-time
  version,
  gitRevision,
  GitRevision,
  Version,

  -- * Displaying Versions
  showVersion,
  showFullVersion,
) where

import Hydra.Prelude

import Data.Version (
  Version,
  showVersion,
 )
import Paths_hydra_node (
  version,
 )

import qualified Data.Text as T
import Development.GitRev (gitHash)

newtype GitRevision = GitRevision Text deriving (Show, Eq)

-- | Like 'showVersion', but also show the git revision.
showFullVersion :: Version -> GitRevision -> String
showFullVersion v (GitRevision r) =
  showVersion v <> toString r

-- | The Git revision ID (40 character hex string) of this build.
--
-- This requires @git@ do be available when building.
gitRevision :: GitRevision
gitRevision
  | T.null fromGit = GitRevision zeroRev
  | otherwise = GitRevision fromGit
 where
  -- Git revision found during compilation by running git. If
  -- git could not be run, then this will be empty.
  fromGit = T.strip (fromString $(gitHash))

  zeroRev :: Text
  zeroRev = "0000000000000000000000000000000000000000"
