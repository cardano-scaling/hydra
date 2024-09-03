-- | A Hydra chain module that interacts with yet another hydra-node upstream.
--
-- This allows to open heads in other heads leveraging the isomorphic nature of
-- the Hydra Head protocol.
--
-- Basd on a websocket client to the API of one of the underlying head's nodes.
module Hydra.Chain.Inception where

import Hydra.Prelude

import Cardano.Api.Genesis (shelleyGenesisDefaults)
import Cardano.Api.GenesisParameters (fromShelleyGenesis)
import Hydra.Cardano.Api (GenesisParameters, ShelleyEra, Tx, sgSystemStart)
import Hydra.Chain (Chain (..), ChainComponent, ChainStateHistory, PostTxError (..))
import Hydra.Options (InceptionChainConfig)

-- | Determine the ledger genesis parameters for the head in a head.
-- TODO: Make this configurable like --offline?
getGenesisParameters :: IO (GenesisParameters ShelleyEra)
getGenesisParameters = do
  now <- getCurrentTime
  -- TODO: uses internal cardano-api lib
  pure $ fromShelleyGenesis shelleyGenesisDefaults{sgSystemStart = now}

-- | Start an inception chain component by connecting to the given hydra API
-- endpoint.
withInceptionChain ::
  InceptionChainConfig ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withInceptionChain config chainStateHistory callback action = do
  action chainHandle
 where
  chainHandle =
    Chain
      { submitTx = const $ pure ()
      , draftCommitTx = \_ _ -> pure $ Left FailedToDraftTxNotInitializing
      , postTx = \tx -> do
          print (spy tx)
          pure ()
      }
