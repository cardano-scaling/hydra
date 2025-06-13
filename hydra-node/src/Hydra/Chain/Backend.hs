module Hydra.Chain.Backend where

import Hydra.Prelude

import Hydra.Cardano.Api (
  Address,
  ChainPoint,
  EraHistory,
  GenesisParameters,
  LedgerEra,
  NetworkId,
  PParams,
  PaymentKey,
  PoolId,
  ShelleyAddr,
  ShelleyEra,
  SystemStart (..),
  Tx,
  TxId,
  UTxO,
  VerificationKey,
 )
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Tx (ScriptRegistry)

class ChainBackend a where
  queryGenesisParameters :: (MonadIO m, MonadThrow m) => a -> m (GenesisParameters ShelleyEra)
  queryScriptRegistry :: (MonadIO m, MonadThrow m) => a -> [TxId] -> m ScriptRegistry
  queryNetworkId :: (MonadIO m, MonadThrow m) => a -> m NetworkId
  queryTip :: (MonadIO m, MonadThrow m) => a -> m ChainPoint
  queryUTxO :: (MonadIO m, MonadThrow m) => a -> [Address ShelleyAddr] -> m UTxO
  queryEraHistory :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m EraHistory
  querySystemStart :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m SystemStart
  queryProtocolParameters :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m (PParams LedgerEra)
  queryStakePools :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m (Set PoolId)
  queryUTxOFor :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> VerificationKey PaymentKey -> m UTxO
  submitTransaction :: (MonadIO m, MonadThrow m) => a -> Tx -> m ()
  awaitTransaction :: (MonadIO m, MonadThrow m) => a -> Tx -> m UTxO
