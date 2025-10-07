{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude hiding (fromList, intercalate)

import Control.Concurrent (mkWeakThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Data.ByteString (intercalate)
import GHC.Weak (deRefWeak)
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain.Blockfrost (BlockfrostBackend (..))
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Chain.ScriptRegistry (publishHydraScripts)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (run)
import Hydra.Node.Util (readKeyPair)
import Hydra.Options (ChainBackendOptions (..), Command (GenHydraKey, Publish, Run), PublishOptions (..), RunOptions (..), parseHydraCommand)
import Hydra.Utils (genHydraKeys)
import System.Posix.Signals qualified as Signals

main :: IO ()
main = do
  installSigTermHandler
  command <- parseHydraCommand
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
        publishHydraScripts (DirectBackend directOptions) sk
      Blockfrost blockfrostOptions ->
        publishHydraScripts (BlockfrostBackend blockfrostOptions) sk
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
