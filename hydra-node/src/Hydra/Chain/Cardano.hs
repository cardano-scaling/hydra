{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Cardano where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Monad.Trans.Except (runExcept)
import Hydra.Cardano.Api (
  EraHistory (EraHistory),
  Tx,
  shelleyBasedEra,
 )
import Hydra.Chain (ChainComponent, ChainStateHistory)
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Blockfrost (BlockfrostBackend (..), withBlockfrostChain)
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
  localNodeConnectInfo,
 )
import Hydra.Chain.Direct (DirectBackend (..), withDirectChain)
import Hydra.Chain.Direct.Handlers (CardanoChainLog (..))
import Hydra.Chain.Direct.State (
  ChainContext (..),
 )
import Hydra.Chain.Direct.Wallet (
  TinyWallet (..),
  WalletInfoOnChain (..),
  newTinyWallet,
 )
import Hydra.Logging (Tracer)
import Hydra.Node.Util (readKeyPair)
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..), DirectOptions (..))
import Hydra.Tx (Party)
import Ouroboros.Consensus.HardFork.History qualified as Consensus

withCardanoChain ::
  forall a.
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  Party ->
  -- | Chain state loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withCardanoChain tracer cfg party chainStateHistory callback action =
  case chainBackendOptions of
    Direct DirectOptions{networkId, nodeSocket} -> do
      let connectInfo = localNodeConnectInfo networkId nodeSocket
      let runDirect :: forall b. DirectBackend b -> IO b
          runDirect = flip runReaderT connectInfo . runDirectBackend
      wallet <- mkTinyWallet runDirect tracer cfg
      ctx <- loadChainContext runDirect cfg party
      withDirectChain connectInfo tracer cfg ctx wallet chainStateHistory callback action
    Blockfrost blockfrostOptions -> do
      let runBlockfrost :: forall b. BlockfrostBackend b -> IO b
          runBlockfrost = flip runReaderT blockfrostOptions . runBlockfrostBackend
      wallet <- mkTinyWallet runBlockfrost tracer cfg
      ctx <- loadChainContext runBlockfrost cfg party
      withBlockfrostChain blockfrostOptions tracer cfg ctx wallet chainStateHistory callback action
 where
  CardanoChainConfig{chainBackendOptions} = cfg

-- | Build the 'ChainContext' from a 'ChainConfig' and additional information.
loadChainContext ::
  forall backend r.
  (ChainBackend backend, MonadIO backend, MonadReader r backend) =>
  (forall a. backend a -> IO a) ->
  CardanoChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | The current running era we can use to query the node
  IO ChainContext
loadChainContext runBackend config party = do
  (vk, _) <- readKeyPair cardanoSigningKey
  scriptRegistry <- runBackend $ queryScriptRegistry hydraScriptsTxId
  networkId <- runBackend queryNetworkId
  pure $
    ChainContext
      { networkId
      , ownVerificationKey = vk
      , ownParty = party
      , scriptRegistry
      }
 where
  CardanoChainConfig
    { hydraScriptsTxId
    , cardanoSigningKey
    } = config

mkTinyWallet ::
  forall backend r.
  (ChainBackend backend, MonadIO backend, MonadReader r backend) =>
  (forall a. backend a -> IO a) ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet runBackend tracer config = do
  keyPair <- readKeyPair cardanoSigningKey
  networkId <- runBackend queryNetworkId
  newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  CardanoChainConfig{cardanoSigningKey} = config

  queryEpochInfo = toEpochInfo <$> runBackend (queryEraHistory QueryTip)

  querySomePParams = runBackend (queryProtocolParameters QueryTip)
  queryWalletInfo queryPoint address = runBackend $ do
    point <- case queryPoint of
      QueryAt point -> pure point
      QueryTip -> queryTip
    walletUTxO <- Ledger.unUTxO . UTxO.toShelleyUTxO shelleyBasedEra <$> queryUTxO [address]
    systemStart <- querySystemStart QueryTip
    pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter
