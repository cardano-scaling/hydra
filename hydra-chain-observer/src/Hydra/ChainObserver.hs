{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Hydra.Blockfrost.ChainObserver (blockfrostClient)
import Hydra.ChainObserver.NodeClient (ChainObserverLog (..), NodeClient (..), ObserverHandler)
import Hydra.ChainObserver.Options (BlockfrostOptions (..), DirectOptions (..), Options (..), hydraChainObserverOptions)
import Hydra.Contract qualified as Contract
import Hydra.Logging (Verbosity (..), traceWith, withTracer)
import Hydra.Ouroborus.ChainObserver (ouroborusClient)
import Hydra.Tx.ScriptRegistry (SerialisedScriptRegistry)
import Options.Applicative (execParser)

main :: SerialisedScriptRegistry -> ObserverHandler IO -> IO ()
main serialisedScriptRegistry observerHandler = do
  opts <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer KnownScripts{scriptInfo = Contract.scriptInfo}
    case opts of
      DirectOpts DirectOptions{networkId, nodeSocket, startChainFrom} -> do
        let NodeClient{follow} = ouroborusClient tracer nodeSocket networkId serialisedScriptRegistry
        follow startChainFrom observerHandler
      BlockfrostOpts BlockfrostOptions{projectPath, startChainFrom} -> do
        -- FIXME: should be configurable
        let blockConfirmations = 1
            NodeClient{follow} = blockfrostClient tracer projectPath blockConfirmations serialisedScriptRegistry
        follow startChainFrom observerHandler
