{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude hiding (fromList)

import Hydra.Cardano.Api (
  serialiseToRawBytesHex,
 )
import Hydra.Chain.Direct.ScriptRegistry (publishHydraScripts)
import Hydra.Chain.Direct.Util (readKeyPair)
import Hydra.Chain.Inception qualified as Inception
import Hydra.Logging (Verbosity (..))
import Hydra.Node.Run (run)
import Hydra.Options (
  ChainConfig (..),
  Command (GenHydraKey, Publish, Run),
  DirectChainConfig (..),
  InceptionChainConfig (..),
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
  publish PublishOptions{publishChainConfig} = do
    -- HACK: Using a full ChainConfig is smelly as we require defaults on things that are actually not needed
    txId <- case publishChainConfig of
      Offline{} -> die "Cannot publish scripts in offline mode"
      Direct DirectChainConfig{networkId, nodeSocket, cardanoSigningKey} -> do
        (_, sk) <- readKeyPair cardanoSigningKey
        publishHydraScripts networkId nodeSocket sk
      Inception InceptionChainConfig{underlyingHydraApi, cardanoSigningKey} -> do
        (_, sk) <- readKeyPair cardanoSigningKey
        Inception.publishHydraScripts underlyingHydraApi sk
    putStr (decodeUtf8 (serialiseToRawBytesHex txId))

identifyNode :: RunOptions -> RunOptions
identifyNode opt@RunOptions{verbosity = Verbose "HydraNode", nodeId} = opt{verbosity = Verbose $ "HydraNode-" <> show nodeId}
identifyNode opt = opt
