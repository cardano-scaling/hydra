module Hydra.Chain.Backend where

import Hydra.Prelude

import Cardano.Api.Consensus (EraMismatch (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  Address,
  AnyCardanoEra (AnyCardanoEra),
  CardanoEra (..),
  ChainPoint,
  EraHistory,
  GenesisParameters,
  LedgerEra,
  NetworkId,
  PParams,
  QueryInShelleyBasedEra (QueryProtocolParameters),
  ShelleyAddr,
  ShelleyEra,
  SystemStart (..),
  TxId,
  UTxO,
  shelleyBasedEra,
 )
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Chain.ScriptRegistry qualified as ScriptRegistry
import Hydra.Options (BlockfrostBackend (..), ChainBackend (..), DirectBackend (..))
import Hydra.Tx (ScriptRegistry)

class BackendOps a where
  queryGenesisParameters :: (MonadIO m, MonadThrow m) => a -> m (GenesisParameters ShelleyEra)
  queryScriptRegistry :: (MonadIO m, MonadThrow m) => a -> [TxId] -> m ScriptRegistry
  queryNetworkId :: (MonadIO m, MonadThrow m) => a -> m NetworkId
  queryTip :: (MonadIO m, MonadThrow m) => a -> m ChainPoint
  queryUTxO :: (MonadIO m, MonadThrow m) => a -> [Address ShelleyAddr] -> m UTxO
  queryEraHistory :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m EraHistory
  querySystemStart :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m SystemStart
  queryProtocolParameters :: (MonadIO m, MonadThrow m) => a -> CardanoClient.QueryPoint -> m (PParams LedgerEra)

instance BackendOps DirectBackend where
  queryGenesisParameters DirectBackend{networkId, nodeSocket} =
    liftIO $ CardanoClient.queryGenesisParameters networkId nodeSocket CardanoClient.QueryTip

  queryScriptRegistry DirectBackend{networkId, nodeSocket} =
    ScriptRegistry.queryScriptRegistry networkId nodeSocket

  queryNetworkId DirectBackend{networkId} = pure networkId

  queryTip DirectBackend{networkId, nodeSocket} =
    liftIO $ CardanoClient.queryTip networkId nodeSocket

  queryUTxO DirectBackend{networkId, nodeSocket} addresses =
    liftIO $ CardanoClient.queryUTxO networkId nodeSocket CardanoClient.QueryTip addresses

  queryEraHistory DirectBackend{networkId, nodeSocket} queryPoint =
    liftIO $ CardanoClient.queryEraHistory networkId nodeSocket queryPoint

  querySystemStart DirectBackend{networkId, nodeSocket} queryPoint =
    liftIO $ CardanoClient.querySystemStart networkId nodeSocket queryPoint

  queryProtocolParameters DirectBackend{networkId, nodeSocket} queryPoint =
    liftIO $ CardanoClient.runQueryExpr networkId nodeSocket queryPoint $ do
      AnyCardanoEra era <- CardanoClient.queryCurrentEraExpr
      case era of
        ConwayEra{} -> CardanoClient.queryInShelleyBasedEraExpr shelleyBasedEra QueryProtocolParameters
        _ -> liftIO . throwIO $ CardanoClient.QueryEraMismatchException EraMismatch{ledgerEraName = show era, otherEraName = "Conway"}

instance BackendOps BlockfrostBackend where
  queryGenesisParameters BlockfrostBackend{projectPath} = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.toCardanoGenesisParameters <$> Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters

  queryScriptRegistry BlockfrostBackend{projectPath} txIds = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj $ Blockfrost.queryScriptRegistry txIds

  queryNetworkId BlockfrostBackend{projectPath} = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    -- TODO: This calls to queryGenesisParameters again, but we only need the network magic
    Blockfrost.Genesis{_genesisNetworkMagic} <- Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
    pure $ Blockfrost.toCardanoNetworkId _genesisNetworkMagic

  queryTip BlockfrostBackend{projectPath} = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryTip

  queryUTxO BlockfrostBackend{projectPath} addresses = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.Genesis
      { _genesisNetworkMagic
      , _genesisSystemStart
      } <-
      Blockfrost.runBlockfrostM prj Blockfrost.queryGenesisParameters
    let networkId = Blockfrost.toCardanoNetworkId _genesisNetworkMagic
    Blockfrost.runBlockfrostM prj $ Blockfrost.queryUTxO networkId addresses

  queryEraHistory BlockfrostBackend{projectPath} _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryEraHistory

  querySystemStart BlockfrostBackend{projectPath} _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.querySystemStart

  queryProtocolParameters BlockfrostBackend{projectPath} _ = do
    prj <- liftIO $ Blockfrost.projectFromFile projectPath
    Blockfrost.runBlockfrostM prj Blockfrost.queryProtocolParameters

instance BackendOps ChainBackend where
  queryGenesisParameters (Direct backend) = queryGenesisParameters backend
  queryGenesisParameters (Blockfrost backend) = queryGenesisParameters backend

  queryScriptRegistry (Direct backend) txIds = queryScriptRegistry backend txIds
  queryScriptRegistry (Blockfrost backend) txIds = queryScriptRegistry backend txIds

  queryNetworkId (Direct backend) = queryNetworkId backend
  queryNetworkId (Blockfrost backend) = queryNetworkId backend

  queryTip (Direct backend) = queryTip backend
  queryTip (Blockfrost backend) = queryTip backend

  queryUTxO (Direct backend) = queryUTxO backend
  queryUTxO (Blockfrost backend) = queryUTxO backend

  queryEraHistory (Direct backend) = queryEraHistory backend
  queryEraHistory (Blockfrost backend) = queryEraHistory backend

  querySystemStart (Direct backend) = querySystemStart backend
  querySystemStart (Blockfrost backend) = querySystemStart backend

  queryProtocolParameters (Direct backend) = queryProtocolParameters backend
  queryProtocolParameters (Blockfrost backend) = queryProtocolParameters backend
