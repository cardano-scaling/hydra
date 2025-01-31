{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Hydra.Blockfrost.ChainObserver (blockfrostClient)
import Hydra.ChainObserver.NodeClient (ChainObservation, ChainObserverLog (..), NodeClient (..))
import Hydra.ChainObserver.Options (Backend (..), Options (..), hydraChainObserverOptions)
import Hydra.Contract qualified as Contract
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Ouroborus.ChainObserver (ouroborusClient)
import Network.HTTP.Simple (getResponseBody, httpNoBody, parseRequestThrow, setRequestBodyJSON)
import Network.URI (URI)
import Options.Applicative (execParser)

main :: IO ()
main = do
  -- TODO: add a --version command line option
  Options{backend, startChainFrom, explorerBaseURI} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer KnownScripts{scriptInfo = Contract.scriptInfo}
    let NodeClient{follow} =
          case backend of
            Direct{networkId, nodeSocket} -> do
              ouroborusClient tracer nodeSocket networkId
            Blockfrost{projectPath} -> do
              -- FIXME: should be configurable
              let blockConfirmations = 1
              blockfrostClient tracer projectPath blockConfirmations
    follow startChainFrom $ \observations ->
      case explorerBaseURI of
        Nothing -> pure ()
        Just uri -> forM_ observations $ reportObservation uri

-- | Submit observation to a 'hydra-explorer' at given base 'URI'.
reportObservation :: URI -> ChainObservation -> IO ()
reportObservation baseURI observation = do
  req <- parseRequestThrow url <&> setRequestBodyJSON observation
  httpNoBody req <&> getResponseBody
 where
  -- FIXME: determine and send network and version information
  -- TODO: maybe change schema to have network and version as part of the request body
  url = "POST " <> show baseURI <> "/observations/2/0.19.0"
