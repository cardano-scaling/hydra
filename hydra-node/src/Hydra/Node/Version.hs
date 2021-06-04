{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides version number optionally with Git SHA1
--
-- Shamelessly stolen from <cardano-wallet https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core/src/Cardano/Wallet/Version.hs>
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

import Cardano.Prelude
import Data.FileEmbed (
  dummySpaceWith,
 )
import Data.String (
  String,
  fromString,
 )
import Data.Text.Encoding (
  decodeLatin1,
 )
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
  showVersion v <> "-" <> T.unpack r

-- | The Git revision ID (40 character hex string) of this build.
--
-- This requires @git@ do be available when building. Alternatively, the git
-- revision of the @cardano-wallet@ binary can be updated post-build using
-- "Data.FileEmbed.injectWith".
gitRevision :: GitRevision
gitRevision
  | gitRevEmbed /= zeroRev = GitRevision gitRevEmbed
  | T.null fromGit = GitRevision zeroRev
  | otherwise = GitRevision fromGit
 where
  -- Git revision embedded after compilation using
  -- Data.FileEmbed.injectWith. If nothing has been injected,
  -- this will be filled with 0 characters.
  gitRevEmbed :: Text
  gitRevEmbed = decodeLatin1 $(dummySpaceWith "gitrev" 40)

  -- Git revision found during compilation by running git. If
  -- git could not be run, then this will be empty.
  fromGit = T.strip (fromString $(gitHash))

  zeroRev :: Text
  zeroRev = "0000000000000000000000000000000000000000"
