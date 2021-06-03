{-# LANGUAGE TemplateHaskell #-}

-- |Template Haskell function for getting the git revision from the local
-- repo.
-- This is a separate module due to the GHC stage restriction.
-- Shamelessly stolen from <cardano-wallet https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core/src/Cardano/Wallet/Version/TH.hs>
module Hydra.Node.Version.TH (
  gitRevFromGit,
) where

import Cardano.Prelude
import Language.Haskell.TH (
  Exp (..),
  Lit (..),
  Q,
  runIO,
 )
import System.IO.Error (
  ioeGetErrorType,
  isDoesNotExistErrorType,
 )
import System.Process (readProcessWithExitCode)

-- | Git revision found by running @git rev-parse@. If @git@ could not be
-- executed, then this will be an empty string.
gitRevFromGit :: Q Exp
gitRevFromGit = LitE . StringL <$> runIO runGitRevParse
 where
  runGitRevParse = handleJust missingGit (const $ pure "") $ do
    (exitCode, output, _) <-
      readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
    pure $ case exitCode of
      ExitSuccess -> output
      _ -> ""
  missingGit e =
    if isDoesNotExistErrorType (ioeGetErrorType e)
      then Just ()
      else Nothing
