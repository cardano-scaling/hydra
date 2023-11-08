{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude hiding (fromList)

import Hydra.Cardano.Api (
  serialiseToRawBytesHex,
  toLedgerPParams
 )

import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (explain, run)
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
      run (identifyNode options) `catch` (die . explain)
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
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
