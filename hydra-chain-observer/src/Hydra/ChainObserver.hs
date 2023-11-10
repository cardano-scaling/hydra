{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver (
  main,
  ChainObserverLog (..),
  observeTx,
  observeAll,
) where

import Hydra.Prelude

import Hydra.Cardano.Api (Block (..), BlockHeader (..), BlockInMode (..), CardanoMode, ChainPoint, ChainSyncClient, ConsensusModeParams (..), EpochSlots (..), EraInMode (..), LocalChainSyncClient (..), LocalNodeClientProtocols (..), LocalNodeConnectInfo (..), NetworkId, SocketPath, Tx, UTxO, connectToLocalNode, getTxBody, getTxId)
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Chain (HeadId (..))
import Hydra.Chain.Direct.Tx (AbortObservation (..), CloseObservation (..), CollectComObservation (..), CommitObservation (..), ContestObservation (..), FanoutObservation (..), HeadObservation (..), RawInitObservation (..), mkHeadId, observeHeadTx)
import Hydra.ChainObserver.Options (Options (..), hydraChainObserverOptions)
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Options.Applicative (execParser)
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStNext (..),
 )
import Hydra.Chain.CardanoClient (queryTip)

main :: IO ()
main = do
  Options{networkId, nodeSocket, startChainFrom} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer ConnectingToNode{nodeSocket, networkId}
    chainPoint <- case startChainFrom of
            Nothing -> queryTip networkId nodeSocket
            Just x -> pure x
    traceWith tracer StartObservingFrom{chainPoint}
    connectToLocalNode
      (connectInfo nodeSocket networkId)
      (clientProtocols tracer networkId)

type ChainObserverLog :: Type
data ChainObserverLog
  = ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | StartObservingFrom { chainPoint :: ChainPoint }
  | HeadInitTx {headId :: HeadId}
  | HeadCommitTx {headId :: HeadId}
  | HeadCollectComTx {headId :: HeadId}
  | HeadCloseTx {headId :: HeadId}
  | HeadFanoutTx {headId :: HeadId}
  | HeadAbortTx {headId :: HeadId}
  | HeadContestTx {headId :: HeadId}
  | Rollback {point :: ChainPoint}
  | RollForward {receivedTxIds :: [TxId]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

type BlockType :: Type
type BlockType = BlockInMode CardanoMode

connectInfo :: SocketPath -> NetworkId -> LocalNodeConnectInfo CardanoMode
connectInfo nodeSocket networkId =
  LocalNodeConnectInfo
    { -- REVIEW: This was 432000 before, but all usages in the
      -- cardano-node repository are using this value. This is only
      -- relevant for the Byron era.
      localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
    , localNodeNetworkId = networkId
    , localNodeSocketPath = nodeSocket
    }

clientProtocols ::
  Tracer IO ChainObserverLog ->
  NetworkId ->
  LocalNodeClientProtocols BlockType ChainPoint tip slot tx txid txerr query IO
clientProtocols tracer networkId =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

-- | Fetch all blocks via chain sync and trace their contents.
chainSyncClient ::
  Tracer IO ChainObserverLog ->
  NetworkId ->
  ChainSyncClient BlockType ChainPoint tip IO ()
chainSyncClient tracer networkId =
  ChainSyncClient $ do
    pure $ SendMsgRequestNext (clientStNext mempty) (pure $ clientStNext mempty)
 where
  clientStIdle :: UTxO -> ClientStIdle BlockType ChainPoint tip IO ()
  clientStIdle utxo = SendMsgRequestNext (clientStNext utxo) (pure $ clientStNext utxo)

  clientStNext :: UTxO -> ClientStNext BlockType ChainPoint tip IO ()
  clientStNext utxo =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode (Block (BlockHeader _slotNo _hash _blockNo) txs) BabbageEraInCardanoMode -> do
              traceWith tracer RollForward{receivedTxIds = getTxId . getTxBody <$> txs}
              let (utxo', logs) = observeAll networkId utxo txs
              forM_ logs (traceWith tracer)
              pure $ clientStIdle utxo'
            _ -> pure $ clientStIdle utxo
      , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
          traceWith tracer Rollback{point}
          pure $ clientStIdle utxo
      }

observeTx :: NetworkId -> UTxO -> Tx -> (UTxO, Maybe ChainObserverLog)
observeTx networkId utxo tx =
  let utxo' = adjustUTxO tx utxo
   in case observeHeadTx networkId utxo tx of
        NoHeadTx -> (utxo, Nothing)
        Init RawInitObservation{headId} -> (utxo', pure $ HeadInitTx{headId = mkHeadId headId})
        Commit CommitObservation{headId} -> (utxo', pure $ HeadCommitTx{headId})
        CollectCom CollectComObservation{headId} -> (utxo', pure $ HeadCollectComTx{headId})
        Close CloseObservation{headId} -> (utxo', pure $ HeadCloseTx{headId})
        Fanout FanoutObservation{headId} -> (utxo', pure $ HeadFanoutTx{headId})
        Abort AbortObservation{headId} -> (utxo', pure $ HeadAbortTx{headId})
        Contest ContestObservation{headId} -> (utxo', pure $ HeadContestTx{headId})

observeAll :: NetworkId -> UTxO -> [Tx] -> (UTxO, [ChainObserverLog])
observeAll networkId utxo txs =
  second reverse $ foldr go (utxo, []) txs
 where
  go :: Tx -> (UTxO, [ChainObserverLog]) -> (UTxO, [ChainObserverLog])
  go tx (utxo'', logs) =
    case observeTx networkId utxo'' tx of
      (utxo', Nothing) -> (utxo', logs)
      (utxo', Just logEntry) -> (utxo', logEntry : logs)
