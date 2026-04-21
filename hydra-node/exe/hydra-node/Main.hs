{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Hydra.Prelude hiding (fromList, intercalate)

import Control.Concurrent (mkWeakThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Data.ByteString (intercalate)
import Data.List (stripPrefix)
import GHC.Weak (deRefWeak)
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain.Blockfrost (runBlockfrostBackend)
import Hydra.Chain.Direct (runDirectBackend)
import Hydra.Chain.ScriptRegistry (publishHydraScripts)
import Hydra.Config (loadConfig)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (run)
import Hydra.Node.Util (readKeyPair)
import Hydra.Options (ChainBackendOptions (..), Command (GenHydraKey, Publish, Run), PublishOptions (..), RunOptions (..), parseHydraCommandFromArgsWith)
import Hydra.Utils (genHydraKeys)
import System.IO (hPutStrLn)
import System.Posix.Signals qualified as Signals

main :: IO ()
main = do
  installSigTermHandler
  args <- getArgs
  -- Strip --config FILE before optparse sees the args so that it is always
  -- accepted (even when used alongside subcommands) and appears correctly in
  -- --help via the configFileParser added to runOptionsParser.
  let strippedArgs = stripConfigFlag args
  command <- case findConfigFlag args of
    Nothing -> do
      -- Warn if the user passed --config with a subcommand, where it is ignored.
      when (any isConfigArg args && any (`elem` knownSubcommands) args) $
        hPutStrLn stderr "Warning: --config is not used with subcommands and will be ignored."
      parseHydraCommandFromArgsWith strippedArgs
    Just configFile -> do
      hPutStrLn stderr $ "Loading configuration from " <> configFile
      baseOpts <- loadConfig configFile
      cliOpts <- parseHydraCommandFromArgsWith strippedArgs
      pure $ case cliOpts of
        Run cliRunOpts ->
          -- Re-apply self-filtering after the merge: if CLI overrode --listen
          -- or --advertise, the YAML-parsed peer list may still contain the
          -- node's own address.
          let merged = baseOpts <> cliRunOpts
              selfAddr = fromMaybe merged.listen merged.advertise
           in Run (merged{peers = filter (/= selfAddr) merged.peers})
        other -> other
  case command of
    Run options ->
      run (identifyNode options)
        `catch` \(SomeException e) -> die $ displayException e
    Publish options ->
      publish options
        `catch` \(SomeException e) -> die $ displayException e
    GenHydraKey outputFile ->
      either (die . show) pure =<< genHydraKeys outputFile
 where
  publish PublishOptions{chainBackendOptions, publishSigningKey} = do
    (_, sk) <- readKeyPair publishSigningKey
    txIds <- case chainBackendOptions of
      Direct directOptions ->
        runDirectBackend directOptions $ publishHydraScripts sk
      Blockfrost blockfrostOptions ->
        runBlockfrostBackend blockfrostOptions $ publishHydraScripts sk
    putBSLn $ intercalate "," (serialiseToRawBytesHex <$> txIds)

-- | Handle SIGTERM like SIGINT
--
-- Taken from: <https://github.com/IntersectMBO/cardano-node/commit/ce26b8e6e5b2ccda123f04c224036be44529b97e>
installSigTermHandler :: IO ()
installSigTermHandler = do
  -- Similar implementation to the RTS's handling of SIGINT (see GHC's
  -- https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/GHC/TopHandler.hs).
  runThreadIdWk <- mkWeakThreadId =<< myThreadId
  _ <-
    Signals.installHandler
      Signals.sigTERM
      ( Signals.CatchOnce $ do
          runThreadIdMay <- deRefWeak runThreadIdWk
          forM_ runThreadIdMay (`throwTo` UserInterrupt)
      )
      Nothing
  pure ()

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt

-- | Subcommands that do not use @--config@.
-- NOTE: Must be kept in sync with the subcommands defined in 'commandParser'
-- in Options.hs. If a new subcommand is added there, add it here too —
-- otherwise --config loading may be attempted for that subcommand.
knownSubcommands :: [String]
knownSubcommands = ["publish-scripts", "gen-hydra-key"]

-- | True if this argument is a @--config@ flag in either @--config FILE@ or
-- @--config=FILE@ form.
isConfigArg :: String -> Bool
isConfigArg a = a == "--config" || isJust (stripPrefix "--config=" a)

-- | Scan CLI args for @--config FILE@ or @--config=FILE@ outside of any
-- subcommand. Returns the config file path if found.
findConfigFlag :: [String] -> Maybe FilePath
findConfigFlag args
  | any (`elem` knownSubcommands) args = Nothing
  | otherwise = go args
 where
  go :: [String] -> Maybe FilePath
  go [] = Nothing
  go ("--config" : fp : _) = Just fp
  go (arg : rest)
    | Just fp <- stripPrefix "--config=" arg = Just fp
    | otherwise = go rest

-- | Remove @--config FILE@ or @--config=FILE@ from a list of arguments.
stripConfigFlag :: [String] -> [String]
stripConfigFlag [] = []
stripConfigFlag ("--config" : _ : rest) = stripConfigFlag rest
stripConfigFlag (arg : rest)
  | Just _ <- stripPrefix "--config=" arg = stripConfigFlag rest
  | otherwise = arg : stripConfigFlag rest
