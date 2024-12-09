{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude hiding (fromList)

import Hydra.Cardano.Api (
  serialiseToRawBytesHex,
 )
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Chain.ScriptRegistry (publishHydraScripts)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (run)
import Hydra.Options (
  Command (GenHydraKey, Publish, Run),
  PublishOptions (..),
  RunOptions (..),
  parseHydraCommand,
 )
import Hydra.Utils (genHydraKeys)

main :: IO ()
main = do
  command <- parseHydraCommand
  case command of
    Run options ->
      run (identifyNode options) `catch` \(SomeException e) -> die $ displayException e
    Publish options ->
      publish options
    GenHydraKey outputFile ->
      either (die . show) pure =<< genHydraKeys outputFile
 where
  publish opts = do
    (_, sk) <- readKeyPair (publishSigningKey opts)
    let PublishOptions{publishNetworkId = networkId, publishNodeSocket} = opts
    txIds <- publishHydraScripts networkId publishNodeSocket sk
    mapM_ putBSLn (serialiseToRawBytesHex <$> txIds)

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
