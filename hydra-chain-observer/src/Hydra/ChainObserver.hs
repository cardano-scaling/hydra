{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Hydra.Blockfrost.ChainObserver (blockfrostClient)
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.ChainObserver.NodeClient (ChainObservation, ChainObserverLog (..), NodeClient (..))
import Hydra.ChainObserver.Options (Backend (..), Options (..), hydraChainObserverOptions)
import Hydra.Contract qualified as Contract
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.NetworkVersions (hydraNodeVersion)
import Hydra.Ouroborus.ChainObserver (ouroborusClient)
import "base" Data.Version (showVersion)
import "http-conduit" Network.HTTP.Simple (getResponseBody, httpNoBody, parseRequestThrow, setRequestBodyJSON)
import "network-uri" Network.URI (URI)
import "optparse-applicative" Options.Applicative (execParser)

main :: IO ()
main = do
  Options{backend, startChainFrom, explorerBaseURI} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer KnownScripts{hydraScriptCatalogue = Contract.hydraScriptCatalogue}
    NodeClient{follow, networkId} <-
      case backend of
        Direct{networkId, nodeSocket} -> do
          pure $ ouroborusClient tracer nodeSocket networkId
        Blockfrost{projectPath} -> do
          -- FIXME: should be configurable
          let blockConfirmations = 1
          blockfrostClient tracer projectPath blockConfirmations
    follow startChainFrom $ \observations ->
      case explorerBaseURI of
        Nothing -> pure ()
        Just uri -> forM_ observations $ reportObservation networkId uri

-- | Submit observation to a 'hydra-explorer' at given base 'URI'.
reportObservation :: NetworkId -> URI -> ChainObservation -> IO ()
reportObservation networkId baseURI observation = do
  req <- parseRequestThrow url <&> setRequestBodyJSON observation
  httpNoBody req <&> getResponseBody
 where
  networkParam = case networkId of
    Mainnet -> "mainnet"
    (Testnet (NetworkMagic magic)) -> show magic

  version = showVersion hydraNodeVersion

  url = "POST " <> show baseURI <> "/observations/" <> networkParam <> "/" <> version
