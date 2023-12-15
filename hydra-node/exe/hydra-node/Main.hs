{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude hiding (fromList)

import Hydra.Cardano.Api (
  serialiseToRawBytesHex,
 )
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (explain, run, runOffline)
import Hydra.Options (
  Command (GenHydraKey, Publish, Run, RunOffline),
  PublishOptions (..),
  RunOptions (..),
  RunOfflineOptions (..),
  parseHydraCommand,
 )
import Hydra.Options.Online qualified as OnlineOptions
import Hydra.Utils (genHydraKeys)

main :: IO ()
main = do
  command <- parseHydraCommand
  case command of
    Run options ->
      run (identifyNode options) `catch` (die . explain)
    RunOffline options ->
      runOffline options `catch` (die . explain)
    Publish options ->
      publish options
    GenHydraKey outputFile ->
      either (die . show) pure =<< genHydraKeys outputFile
 where
  publish opts = do
    (_, sk) <- readKeyPair (publishSigningKey opts)
    let PublishOptions{publishNetworkId = networkId, publishNodeSocket} = opts
    txId <- publishHydraScripts networkId publishNodeSocket sk
    putStr (decodeUtf8 (serialiseToRawBytesHex txId))

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{OnlineOptions.verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
