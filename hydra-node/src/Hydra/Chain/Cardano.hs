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
import Hydra.Chain.Blockfrost (BlockfrostBackend, runBlockfrostBackend, withBlockfrostChain)
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
 )
import Hydra.Chain.Direct (DirectBackend, runDirectBackend, withDirectChain)
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
import Hydra.Options (CardanoChainConfig (..), ChainBackendOptions (..))
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
    Direct directOptions -> do
      wallet <- mkTinyWallet @DirectBackend (runDirectBackend directOptions) tracer cfg
      ctx <- runDirectBackend directOptions $ loadChainContext cfg party
      withDirectChain directOptions tracer cfg ctx wallet chainStateHistory callback action
    Blockfrost blockfrostOptions -> do
      wallet <- mkTinyWallet @BlockfrostBackend (runBlockfrostBackend blockfrostOptions) tracer cfg
      ctx <- runBlockfrostBackend blockfrostOptions $ loadChainContext cfg party
      withBlockfrostChain blockfrostOptions tracer cfg ctx wallet chainStateHistory callback action
 where
  CardanoChainConfig{chainBackendOptions} = cfg

-- | Build the 'ChainContext' from a 'ChainConfig' and additional information.
loadChainContext ::
  forall m.
  (ChainBackend m, MonadIO m) =>
  CardanoChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  m ChainContext
loadChainContext config party = do
  (vk, _) <- liftIO $ readKeyPair cardanoSigningKey
  scriptRegistry <- queryScriptRegistry hydraScriptsTxId
  networkId <- queryNetworkId
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
  forall m.
  (ChainBackend m, Monad m) =>
  (forall b. m b -> IO b) ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet runM tracer config = do
  keyPair <- readKeyPair cardanoSigningKey
  networkId <- runM queryNetworkId
  newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  CardanoChainConfig{cardanoSigningKey} = config

  queryEpochInfo = runM $ toEpochInfo <$> queryEraHistory QueryTip

  querySomePParams = runM $ queryProtocolParameters QueryTip

  queryWalletInfo queryPoint address = runM $ do
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
