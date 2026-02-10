{-# LANGUAGE TemplateHaskell #-}

-- | Provides version numbers from calling git on build time or from an embedded
-- string.
--
-- The former is based on the 'gitrev' package with a 'Maybe' interface around
-- it, while the embedding is done using a special c-array placeholder in
-- cbits/revision.c
module Hydra.Version where

import "base" Foreign.C (CString)
import "base" GHC.Foreign (peekCStringLen)
import "base" GHC.IO (unsafeDupablePerformIO)
import "base" GHC.IO.Encoding (utf8)
import "gitrev" Development.GitRev qualified as GitRev

import Hydra.Prelude

-- | Identifier to be used when no revision can be found.
--
-- This is also the default used in 'gitrev'.
unknownVersion :: String
unknownVersion = "UNKNOWN"

-- | Determine the version on build time using `git describe`.
-- FIXME: This does not change when hydra-prelude is not re-compiled
gitDescribe :: Maybe String
gitDescribe
  | fromGit == unknownVersion = Nothing
  | otherwise = Just fromGit
 where
  -- Git describe version found during compilation by running git. If git could
  -- not be run, then this will be "UNKNOWN".
  fromGit = $(GitRev.gitDescribe)

-- | Determine the version on build time using `git rev-parse`.
-- FIXME: This does not change when hydra-prelude is not re-compiled
gitRevision :: Maybe String
gitRevision
  | fromGit == unknownVersion = Nothing
  | otherwise = Just fromGit
 where
  -- Git revision found during compilation by running git. If
  -- git could not be run, then this will be "UNKNOWN".
  fromGit = $(GitRev.gitHash)

-- Placeholder for the git revision. Must match name in 'cbits/revision.c'.
foreign import ccall "&_hydra_gitrev" c_gitrev :: CString

-- | The git revision embedded at a special place holder in the object/binary.
-- NOTE: Keep this consistent with what is hard-coded in 'cbits/revision.c'
embeddedRevision :: Maybe String
embeddedRevision
  | embedded == placeholder = Nothing
  | otherwise = Just embedded
 where
  embedded = unsafeDupablePerformIO (peekCStringLen utf8 (c_gitrev, 40))

  placeholder = replicate 40 '0'
