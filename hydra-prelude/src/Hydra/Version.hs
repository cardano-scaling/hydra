{-# LANGUAGE TemplateHaskell #-}

-- | Provides version numbers from calling git on build time or from an embedded
-- string.
--
-- The former is based on the 'githash' package with a 'Maybe' interface around
-- it, while the embedding is done using a special c-array placeholder in
-- cbits/revision.c
module Hydra.Version where

import Hydra.Prelude

import Foreign.C (CString)
import GHC.Foreign (peekCStringLen)
import GHC.IO (unsafeDupablePerformIO)
import GHC.IO.Encoding (utf8)
import GitHash (GitInfo, giDescribe, giHash, tGitInfoCwdTry)

-- | Identifier to be used when no revision can be found.
unknownVersion :: String
unknownVersion = "UNKNOWN"

-- | Git information determined at compile time, or 'Left' when git could not be
-- run (e.g. building from a source tarball or a nix store path with no @.git@).
--
-- NOTE: 'githash' registers @.git/HEAD@, @.git/index@ and @.git/packed-refs@ as
-- compile-time dependencies, so this module (and hence everything downstream of
-- hydra-prelude) is recompiled when the git state changes. That is what keeps
-- the values below from going stale; the cost is a rebuild after operations
-- that touch the index.
gitInfo :: Either String GitInfo
gitInfo = $$tGitInfoCwdTry

-- | Determine the version on build time using `git describe --always --long`.
gitDescribe :: Maybe String
gitDescribe = either (const Nothing) (Just . giDescribe) gitInfo

-- | Determine the version on build time using `git rev-parse HEAD`.
gitRevision :: Maybe String
gitRevision = either (const Nothing) (Just . giHash) gitInfo

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
