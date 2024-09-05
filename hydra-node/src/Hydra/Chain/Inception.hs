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
import Cardano.Api.Keys.Class (Key (getVerificationKey))
import Control.Tracer (debugTracer, showTracing)
import Hydra.Cardano.Api (AsType (AsPaymentKey, AsSigningKey), GenesisParameters, NetworkId (..), NetworkMagic (..), PaymentKey, ShelleyEra, SigningKey, Tx, sgSystemStart)
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain (Chain (..), ChainComponent, ChainStateHistory, PostChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Chain.Direct.Wallet (TinyWallet (..), newTinyWallet)
import Hydra.Logging (withTracer)
import Hydra.Network (Host)
import Hydra.Options (InceptionChainConfig (..))
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
-- HACK: Should add tracing
withInceptionChain ::
  InceptionChainConfig ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withInceptionChain config chainStateHistory callback action = do
  -- TODO: instantiate a wallet to balance the txs?
  -- sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) cardanoSigningKey
  -- wallet <- newHydraWallet networkId sk underlyingHydraApi
  let wallet = undefined
  -- TODO: open websocket and callback on SnapshotConfirmed
  action
    Chain
      { postTx = postTx wallet
      , submitTx
      , draftCommitTx = \_ _ -> pure $ Left FailedToDraftTxNotInitializing
      }
 where
  InceptionChainConfig{underlyingHydraApi, cardanoSigningKey} = config

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

    hPutStrLn stderr $ renderTx tx

    -- TODO: finalizeTx with wallet
    submitTx tx

  submitTx tx = do
    -- TODO: send via "NewTx" on websocket
    hPutStrLn stderr "submitTx"
    pHPrint stderr tx

-- | Create a 'TinyWallet' interface from a connection to the Hydra node.
-- HACK: Should find a better "common" denominator for a wallet.
newHydraWallet :: NetworkId -> SigningKey PaymentKey -> Host -> IO (TinyWallet IO)
newHydraWallet networkId sk host =
  newTinyWallet
    (showTracing debugTracer)
    networkId
    (getVerificationKey sk, sk)
    queryWalletInfo
    queryEpochInfo
    querySomePParams
 where
  queryWalletInfo = undefined

  queryEpochInfo = undefined

  querySomePParams = undefined
