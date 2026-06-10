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
import Hydra.Network (showHost)
import Hydra.Node.Run (run)
import Hydra.Node.Util (readKeyPair)
import Hydra.Options (
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  Command (GenHydraKey, Publish, Run),
  PublishOptions (..),
  RunOptions (..),
  defaultRunOptions,
  parseHydraCommandFromArgsWith,
  subcommandNames,
  validateRunOptions,
 )
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
  strippedArgs <- stripConfigFlag args
  command <- case findConfigFlag args of
    Nothing -> do
      -- Warn if the user passed --config with a subcommand, where it is ignored.
      when (any isConfigArg args && any (`elem` subcommandNames) args) $
        hPutStrLn stderr "Warning: --config is not used with subcommands and will be ignored."
      parseHydraCommandFromArgsWith strippedArgs
    Just configFile -> do
      hPutStrLn stderr $ "Loading configuration from " <> configFile
      baseOpts <- loadConfig configFile
      cliOpts <- parseHydraCommandFromArgsWith strippedArgs
      case cliOpts of
        Run cliRunOpts -> do
          let merged = baseOpts <> cliRunOpts
          mapM_ (hPutStrLn stderr) (cliDefaultWarnings strippedArgs baseOpts cliRunOpts)
          case validateRunOptions merged of
            Left err ->
              die $
                "Invalid configuration after merging "
                  <> configFile
                  <> " with CLI flags:\n  "
                  <> show err
                  <> "\n\n"
                  <> validationHint err merged
            Right () -> pure ()
          pure (Run merged)
        other -> pure other
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

-- | True if this argument is a @--config@ flag in either @--config FILE@ or
-- @--config=FILE@ form.
isConfigArg :: String -> Bool
isConfigArg a = a == "--config" || isJust (stripPrefix "--config=" a)

-- | Scan CLI args for @--config FILE@ or @--config=FILE@ outside of any
-- subcommand. Returns the config file path if found.
findConfigFlag :: [String] -> Maybe FilePath
findConfigFlag args
  | any (`elem` subcommandNames) args = Nothing
  | otherwise = go args
 where
  go :: [String] -> Maybe FilePath
  go [] = Nothing
  go ("--config" : fp : _)
    | looksLikeFlag fp = Nothing
    | otherwise = Just fp
  go (arg : rest)
    | Just fp <- stripPrefix "--config=" arg = Just fp
    | otherwise = go rest

-- | Remove @--config FILE@ or @--config=FILE@ from a list of arguments.
-- Dies with a helpful error if @--config@ is given without a value (or with
-- another flag as its value).
stripConfigFlag :: [String] -> IO [String]
stripConfigFlag [] = pure []
stripConfigFlag ["--config"] =
  die "--config requires a FILE argument."
stripConfigFlag ("--config" : v : rest)
  | looksLikeFlag v =
      die $ "--config requires a FILE argument; got '" <> v <> "' which looks like a flag."
  | otherwise = stripConfigFlag rest
stripConfigFlag (arg : rest)
  | Just _ <- stripPrefix "--config=" arg = stripConfigFlag rest
  | otherwise = (arg :) <$> stripConfigFlag rest

-- | Does this argument look like a flag (starts with @--@)?
looksLikeFlag :: String -> Bool
looksLikeFlag s = "--" `isPrefixOf` s

-- | Warn when a CLI flag was explicitly passed with a value equal to the
-- default, while the YAML config file sets a non-default value. Because the
-- merge logic can't distinguish "flag not passed" from "flag passed but equal
-- to the default" at the 'RunOptions' level, the YAML value silently wins.
--
-- This does not change behavior — it only surfaces the surprise as a warning.
-- The full fix would require tracking per-flag presence (e.g. Maybe-wrapped
-- parser results) throughout 'RunOptions', which is a larger refactor.
cliDefaultWarnings :: [String] -> RunOptions -> RunOptions -> [String]
cliDefaultWarnings args yaml cli =
  catMaybes $
    [ check "--api-port" cli.apiPort yaml.apiPort defaultRunOptions.apiPort
    , check "--api-host" (show cli.apiHost :: String) (show yaml.apiHost) (show defaultRunOptions.apiHost)
    , check "--node-id" cli.nodeId yaml.nodeId defaultRunOptions.nodeId
    , check "--listen" (showHost cli.listen) (showHost yaml.listen) (showHost defaultRunOptions.listen)
    , check "--persistence-dir" cli.persistenceDir yaml.persistenceDir defaultRunOptions.persistenceDir
    , check "--hydra-signing-key" cli.hydraSigningKey yaml.hydraSigningKey defaultRunOptions.hydraSigningKey
    ]
      <> cardanoChainChecks
 where
  cardanoChainChecks = case (cli.chainConfig, yaml.chainConfig, defaultRunOptions.chainConfig) of
    (Cardano c, Cardano y, Cardano d) ->
      [ check "--cardano-signing-key" c.cardanoSigningKey y.cardanoSigningKey d.cardanoSigningKey
      , check "--contestation-period" (show c.contestationPeriod :: String) (show y.contestationPeriod) (show d.contestationPeriod)
      , check "--deposit-period" (show c.depositPeriod :: String) (show y.depositPeriod) (show d.depositPeriod)
      ]
    _ -> []

  check :: Eq a => String -> a -> a -> a -> Maybe String
  check flag cliV yamlV defV
    | flagPresent flag && cliV == defV && yamlV /= defV =
        Just $
          "Warning: CLI flag "
            <> flag
            <> " was passed with a value equal to the compiled-in default, "
            <> "but the config file sets it to a different value. "
            <> "The config-file value will be used (known limitation of the merge logic). "
            <> "Pass a non-default value on the CLI or remove it from the YAML to resolve."
    | otherwise = Nothing

  flagPresent flag =
    any (\a -> a == flag || (flag <> "=") `isPrefixOf` a) args

-- | Render a user-friendly hint for a validation failure.
validationHint :: Show err => err -> RunOptions -> String
validationHint err opts =
  "Hint: " <> hint
 where
  tag :: String
  tag = show err
  hint :: String
  hint = case tag of
    "CardanoAndHydraKeysMismatch" ->
      let hvks = length opts.hydraVerificationKeys
          cvks = case opts.chainConfig of
            Cardano c -> length c.cardanoVerificationKeys
            _ -> 0
       in "got "
            <> (show hvks :: String)
            <> " hydra-verification-key(s) and "
            <> (show cvks :: String)
            <> " cardano-verification-key(s). Each signing peer needs both; "
            <> "a peer entry with only one is almost certainly a mistake, and "
            <> "an observer/mirror peer needs neither."
    "MaximumNumberOfPartiesExceeded" ->
      "too many parties are configured. Reduce the peer list or split the head."
    _ -> "inspect the merged configuration via GET /config once the node starts."
