{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude hiding (fromList, intercalate)

import Control.Concurrent (mkWeakThreadId, myThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Data.ByteString (intercalate)
import GHC.Weak (deRefWeak)
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Chain.ScriptRegistry (publishHydraScripts)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (run)
import Hydra.Options (ChainConfig (..), Command (GenHydraKey, Publish, Run), DirectChainConfig (..), PublishOptions (..), RunOptions (..), parseHydraCommand)
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
    GenHydraKey outputFile ->
      either (die . show) pure =<< genHydraKeys outputFile
 where
  publish PublishOptions{publishChainConfig} = case publishChainConfig of
    Offline _ -> error "not supported"
    Direct DirectChainConfig{networkId, nodeSocket, cardanoSigningKey} ->
      do
        (_, sk) <- readKeyPair cardanoSigningKey
        txIds <- publishHydraScripts networkId nodeSocket sk
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
