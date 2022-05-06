{-# LANGUAGE TemplateHaskell #-}

-- | Provides version numbers from git identifiers. Based on 'gitrev' package
-- with a 'Maybe' interface around it.
module Hydra.Node.Version where

import Hydra.Prelude

import qualified Development.GitRev as GitRev

-- | Determine the version during build time using `git describe`.
gitDescribe :: Maybe String
gitDescribe
  | fromGit == unknownFromGit = Nothing
  | otherwise = Just fromGit
 where
  -- Git describe version found during compilation by running git. If git could
  -- not be run, then this will be "UNKNOWN".
  fromGit = $(GitRev.gitDescribe)

-- | Determine the version during build time using `git rev-parse`.
gitRevision :: Maybe String
gitRevision
  | fromGit == unknownFromGit = Nothing
  | otherwise = Just fromGit
 where
  -- Git revision found during compilation by running git. If
  -- git could not be run, then this will be "UNKNOWN".
  fromGit = $(GitRev.gitHash)

-- According to 'gitrev' docs, this is the default value returned on errors.
unknownFromGit :: String
unknownFromGit = "UNKNOWN"
