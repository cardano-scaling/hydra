{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Provides version numbers from git identifiers. Based on 'gitrev' package
-- with a 'Maybe' interface around it.
module Hydra.Version where

import Hydra.Prelude

import Data.FileEmbed (embedFileIfExists)
import qualified Data.Text as Text
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

-- | Determine the version during build time.  Uses either the content of a
-- `VERSION` file if it exists, the output of `git rev-parse` if available, or
-- `Nothing`.
gitRevision :: Maybe String
gitRevision
  | isJust fromFile = fromFile
  | fromGit == unknownFromGit = Nothing
  | otherwise = Just fromGit
 where
  -- Git revision found during compilation by running git. If
  -- git could not be run, then this will be "UNKNOWN".
  fromGit = $(GitRev.gitHash)

  -- Git revision found in a file named `VERSION` at top-level of
  -- project's directory.
  fromFile = fmap (Text.unpack . decodeUtf8 @_ @ByteString) $(embedFileIfExists "VERSION")

-- According to 'gitrev' docs, this is the default value returned on errors.
unknownFromGit :: String
unknownFromGit = "UNKNOWN"
