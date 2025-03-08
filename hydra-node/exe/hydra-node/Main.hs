{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude hiding (fromList, intercalate)

import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException (UserInterrupt), throwTo)
import Data.ByteString (intercalate)
import Hydra.Cardano.Api (serialiseToRawBytesHex)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Chain.ScriptRegistry (publishHydraScripts)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (run)
import Hydra.Options (ChainConfig (..), Command (GenHydraKey, Publish, Run), DirectChainConfig (..), PublishOptions (..), RunOptions (..), parseHydraCommand)
import Hydra.Utils (genHydraKeys)
import System.Posix (Handler (..), installHandler, sigTERM)

main :: IO ()
main = do
  command <- parseHydraCommand
  case command of
    Run options -> do
      -- Handle SIGTERM like SIGINT
      tid <- myThreadId
      _ <- installHandler sigTERM (Catch $ throwTo tid UserInterrupt) Nothing
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

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
