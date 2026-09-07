{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Control.Tracer.JSON (Verbosity (..), traceWith, withTracer)
import Data.Version (Version, showVersion)
import Hydra.Blockfrost.ChainObserver (blockfrostClient)
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.ChainObserver.NodeClient (ChainObservation, ChainObserverLog (..), NodeClient (..))
import Hydra.ChainObserver.Options (Backend (..), Options (..), hydraChainObserverOptions)
import Hydra.ChainObserver.VersionRegistry (kvVersion, loadKnownVersions, loadKnownVersionsFromFile)
import Hydra.NetworkVersions (hydraNodeVersion)
import Hydra.Ouroborus.ChainObserver (ouroborusClient)
import Network.HTTP.Simple (getResponseBody, httpNoBody, parseRequestThrow, setRequestBodyJSON)
import Network.URI (URI)
import Options.Applicative (execParser)

main :: IO ()
main = do
  Options{backend, startChainFrom, explorerBaseURI, scriptHashesFile} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    knownScriptVersions <- maybe (pure loadKnownVersions) loadKnownVersionsFromFile scriptHashesFile
    traceWith tracer KnownVersions{knownVersions = kvVersion <$> knownScriptVersions}
    NodeClient{follow, networkId} <-
      case backend of
        Direct{networkId, nodeSocket} -> do
          pure $ ouroborusClient tracer knownScriptVersions nodeSocket networkId
        Blockfrost{projectPath} -> do
          -- FIXME: should be configurable
          let blockConfirmations = 1
          blockfrostClient tracer knownScriptVersions projectPath blockConfirmations
    follow startChainFrom $ \observations ->
      case explorerBaseURI of
        Nothing -> pure ()
        Just uri -> forM_ observations $ \(mVer, obs) ->
          reportObservation networkId uri (fromMaybe hydraNodeVersion mVer) obs

-- | Submit observation to a 'hydra-explorer' at given base 'URI'.
-- The version is used in the URL path; ticks fall back to the binary version.
reportObservation :: NetworkId -> URI -> Version -> ChainObservation -> IO ()
reportObservation networkId baseURI version observation = do
  req <- parseRequestThrow url <&> setRequestBodyJSON observation
  httpNoBody req <&> getResponseBody
 where
  networkParam = case networkId of
    Mainnet -> "mainnet"
    (Testnet (NetworkMagic magic)) -> show magic

  url = "POST " <> show baseURI <> "/observations/" <> networkParam <> "/" <> showVersion version
