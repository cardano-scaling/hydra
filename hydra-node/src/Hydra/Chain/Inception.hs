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
import Hydra.Cardano.Api (GenesisParameters, NetworkId (..), NetworkMagic (..), ShelleyEra, Tx, sgSystemStart)
import Hydra.Chain (Chain (..), ChainComponent, ChainStateHistory, PostChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.Network (Host)
import Hydra.Options (InceptionChainConfig)
import System.IO (hPutStrLn)
import Text.Pretty.Simple (pHPrint)

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
  -- TODO: add wallet
  let wallet = undefined
  -- TODO: open websocket and callback on SnapshotConfirmed
  action
    Chain
      { postTx = postTx wallet
      , submitTx
      , draftCommitTx = \_ _ -> pure $ Left FailedToDraftTxNotInitializing
      }
 where
  -- TODO: configure / fetch from underlying?
  networkId = Testnet (NetworkMagic 42)

  postTx wallet toPost = do
    hPutStrLn stderr "Should postTx in Head:"
    pHPrint stderr toPost

    -- TODO: query spendable utxo on demand
    -- TODO: define a timehandle to use (current slot?)
    -- TODO: prepareTxToPost with timehandle, wallet and spendable utxo
    tx <- atomically $ do
      case toPost of
        InitTx{participants, headParameters} ->
          getSeedInput wallet >>= \case
            Just seedInput ->
              pure $ initTx networkId seedInput participants headParameters
            Nothing ->
              error "no seed input"
        _ -> undefined

    -- TODO: finalizeTx with wallet
    submitTx tx

  submitTx tx = do
    -- TODO: send via "NewTx" on websocket
    hPutStrLn stderr "submitTx"
    pHPrint stderr tx

-- | Create a 'TinyWallet' interface using a connection to the Hydra node.
newHydraWallet :: Host -> m (TinyWallet m)
newHydraWallet = undefined
