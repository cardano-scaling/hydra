{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Hydra.Blockfrost.ChainObserver (blockfrostClient)
import Hydra.ChainObserver.NodeClient (ChainObservation, ChainObserverLog (..), NodeClient (..))
import Hydra.ChainObserver.Options (Backend (..), Options (..), hydraChainObserverOptions)
import Hydra.Contract qualified as Contract
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Ouroborus.ChainObserver (ouroborusClient)
import Network.HTTP.Simple (getResponseBody, httpLbs, httpNoBody, parseRequestThrow, setRequestBodyJSON)
import Network.URI (URI)
import Options.Applicative (execParser)

main :: IO ()
main = do
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
-- TODO: how to handle errors?
reportObservation :: URI -> ChainObservation -> IO ()
reportObservation baseURI observation =
  parseRequestThrow url
    >>= httpNoBody
    <&> getResponseBody
 where
  -- TODO: determine network and version
  url = "POST " <> show baseURI <> "/observations/mainnet/0.19.0"
