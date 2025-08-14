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
      let directBackend = DirectBackend directOptions
      wallet <- mkTinyWallet directBackend tracer cfg
      ctx <- loadChainContext directBackend cfg party
      withDirectChain directBackend tracer cfg ctx wallet chainStateHistory callback action
    Blockfrost blockfrostOptions -> do
      let blockfrostBackend = BlockfrostBackend blockfrostOptions
      wallet <- mkTinyWallet blockfrostBackend tracer cfg
      ctx <- loadChainContext blockfrostBackend cfg party
      withBlockfrostChain blockfrostBackend tracer cfg ctx wallet chainStateHistory callback action
 where
  CardanoChainConfig{chainBackendOptions} = cfg

-- | Build the 'ChainContext' from a 'ChainConfig' and additional information.
loadChainContext ::
  forall backend.
  ChainBackend backend =>
  backend ->
  CardanoChainConfig ->
  -- | Hydra party of our hydra node.
  Party ->
  -- | The current running era we can use to query the node
  IO ChainContext
loadChainContext backend config party = do
  (vk, _) <- readKeyPair cardanoSigningKey
  scriptRegistry <- queryScriptRegistry backend hydraScriptsTxId
  networkId <- queryNetworkId backend
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
  forall backend.
  ChainBackend backend =>
  backend ->
  Tracer IO CardanoChainLog ->
  CardanoChainConfig ->
  IO (TinyWallet IO)
mkTinyWallet backend tracer config = do
  keyPair <- readKeyPair cardanoSigningKey
  networkId <- queryNetworkId backend
  newTinyWallet (contramap Wallet tracer) networkId keyPair queryWalletInfo queryEpochInfo querySomePParams
 where
  CardanoChainConfig{cardanoSigningKey} = config

  queryEpochInfo = toEpochInfo <$> queryEraHistory backend QueryTip

  querySomePParams = queryProtocolParameters backend QueryTip
  queryWalletInfo queryPoint address = do
    point <- case queryPoint of
      QueryAt point -> pure point
      QueryTip -> queryTip backend
    walletUTxO <- Ledger.unUTxO . UTxO.toShelleyUTxO shelleyBasedEra <$> queryUTxO backend [address]
    systemStart <- querySystemStart backend QueryTip
    pure $ WalletInfoOnChain{walletUTxO, systemStart, tip = point}

  toEpochInfo :: EraHistory -> EpochInfo (Either Text)
  toEpochInfo (EraHistory interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter
