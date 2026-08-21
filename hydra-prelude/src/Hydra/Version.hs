{-# LANGUAGE TemplateHaskell #-}

-- | Provides version numbers from calling git on build time or from an embedded
-- string.
--
-- The former is based on the 'gitrev' package with a 'Maybe' interface around
-- it, while the embedding is done using a special c-array placeholder in
-- cbits/revision.c
--
-- NOTE: 'gitrev' does not register the git state as a compile-time dependency,
-- so the two values below go stale until hydra-prelude happens to be recompiled.
-- That is the trade we want. hydra-prelude is the root of the dependency tree,
-- and a package that does register it (githash registers @.git/HEAD@,
-- @.git/index@ and @.git/packed-refs@) rebuilds every package downstream of it
-- after a plain @git add@. Only dev builds are affected: released binaries carry
-- their revision in the 'cbits/revision.c' placeholder, patched by nix (see
-- nix/hydra/packages.nix), and 'embeddedRevision' is preferred over
-- 'gitRevision' at every call site.
module Hydra.Version where

import Hydra.Prelude

import Development.GitRev qualified as GitRev
import Foreign.C (CString)
import GHC.Foreign (peekCStringLen)
import GHC.IO (unsafeDupablePerformIO)
import GHC.IO.Encoding (utf8)

-- | Identifier to be used when no revision can be found.
--
-- This is also the default used in 'gitrev'.
unknownVersion :: String
unknownVersion = "UNKNOWN"

-- | Determine the version on build time using `git describe`.
gitDescribe :: Maybe String
gitDescribe
  | fromGit == unknownVersion = Nothing
  | otherwise = Just fromGit
 where
  -- Git describe version found during compilation by running git. If git could
  -- not be run, then this will be "UNKNOWN".
  fromGit = $(GitRev.gitDescribe)

-- | Determine the version on build time using `git rev-parse`.
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
--
-- NOTE: 'placeholder' has to agree with the array in 'cbits/revision.c' and
-- with the string nix substitutes for it (see nix/hydra/packages.nix). It
-- drifted from both in ab2362a29, which made every unpatched build report the
-- placeholder itself as its revision and left 'gitRevision' unreachable, since
-- both call sites prefer this one. 'Hydra.NetworkVersionsSpec' guards against
-- that happening again.
embeddedRevision :: Maybe String
embeddedRevision
  | embedded == placeholder = Nothing
  | otherwise = Just embedded
 where
  embedded = unsafeDupablePerformIO (peekCStringLen utf8 (c_gitrev, 40))

  -- Four ten-character groups of nine '0' and a '1', matching how
  -- 'cbits/revision.c' writes it. Assembled rather than spelled out as a
  -- string literal on purpose: nix greps the linked binary for exactly one
  -- occurrence of this byte sequence and silently skips embedding the revision
  -- if it finds more, and a literal here would be that second occurrence.
  placeholder = concat (replicate 4 (replicate 9 '0' <> "1"))
