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
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.UTxO qualified as Ledger
import Control.Tracer (debugTracer, showTracing)
import Hydra.Cardano.Api (AsType (AsPaymentKey, AsSigningKey), ChainPoint (..), GenesisParameters, LedgerEra, NetworkId (..), NetworkMagic (..), PParams, PaymentKey, ShelleyEra, SigningKey, Tx, UTxO, isVkTxOut, sgSystemStart, toLedgerUTxO)
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain (Chain (..), ChainComponent, ChainStateHistory, PostChainTx (..), PostTxError (..))
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Chain.Direct.Wallet (SomePParams (..), TinyWallet (..), WalletInfoOnChain (..), newTinyWallet)
import Hydra.Network (Host)
import Hydra.Options (InceptionChainConfig (..))
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Simple (getResponseBody, httpJSON)
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
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) cardanoSigningKey
  pHPrint stderr sk
  wallet <- newHydraWallet networkId sk underlyingHydraApi
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
--
-- HACK: Should find a better "common" denominator for a wallet. Currently the
-- TinyWallet is quite strictly tied to direct chain following semantics.
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
  queryWalletInfo _queryTip _address = do
    utxo <- getSnapshotUTxO host
    let ownUTxO = UTxO.filter (isVkTxOut $ getVerificationKey sk) utxo
    pure
      WalletInfoOnChain
        { walletUTxO = Ledger.unUTxO $ toLedgerUTxO ownUTxO
        , -- HACK: hard-coded system start
          systemStart = Fixture.systemStart
        , -- HACK: always at genesis
          tip = ChainPointAtGenesis
        }

  -- HACK: hard-coded epoch info
  queryEpochInfo = pure Fixture.epochInfo

  -- HACK: hard-coded pparams
  querySomePParams = pure $ BabbagePParams Fixture.defaultPParams

-- * Hydra client

-- TODO: This could become a hydra-api or hydra-client package.

-- | Fetch protocol parameters.
getProtocolParameters :: Host -> IO (PParams LedgerEra)
getProtocolParameters host = do
  parseUrlThrow ("GET http://" <> show host <> "/protocol-parameters")
    >>= httpJSON
    <&> getResponseBody

-- | Fetch last confirmed UTxO.
getSnapshotUTxO :: Host -> IO UTxO
getSnapshotUTxO host = do
  parseUrlThrow ("GET http://" <> show host <> "/snapshot/utxo")
    >>= httpJSON
    <&> getResponseBody
