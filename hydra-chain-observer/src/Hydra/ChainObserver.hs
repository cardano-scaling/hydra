{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver where

import Hydra.Prelude

import Hydra.Cardano.Api (
  Block (..),
  BlockInMode (..),
  CardanoEra (..),
  CardanoMode,
  ChainPoint,
  ChainSyncClient,
  ChainTip,
  ConsensusModeParams (..),
  EpochSlots (..),
  EraInMode (..),
  LocalChainSyncClient (..),
  LocalNodeClientProtocols (..),
  LocalNodeConnectInfo (..),
  NetworkId,
  SocketPath,
  Tx,
  UTxO,
  connectToLocalNode,
  getTxBody,
  getTxId,
 )
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Chain.CardanoClient (queryTip)
import Hydra.Chain.Direct.Tx (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  InitObservation (..),
  observeHeadTx,
 )
import Hydra.ChainObserver.Options (Options (..), hydraChainObserverOptions)
import Hydra.Contract (ScriptInfo)
import Hydra.Contract qualified as Contract
import Hydra.HeadId (HeadId (..))
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Logging (Tracer, Verbosity (..), traceWith, withTracer)
import Options.Applicative (execParser)
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStIntersect (..),
  ClientStNext (..),
 )

main :: IO ()
main = do
  Options{networkId, nodeSocket, startChainFrom} <- execParser hydraChainObserverOptions
  withTracer (Verbose "hydra-chain-observer") $ \tracer -> do
    traceWith tracer KnownScripts{scriptInfo = Contract.scriptInfo}
    traceWith tracer ConnectingToNode{nodeSocket, networkId}
    chainPoint <- case startChainFrom of
      Nothing -> queryTip networkId nodeSocket
      Just x -> pure x
    traceWith tracer StartObservingFrom{chainPoint}
    connectToLocalNode
      (connectInfo nodeSocket networkId)
      (clientProtocols tracer networkId chainPoint)

type ChainObserverLog :: Type
data ChainObserverLog
  = KnownScripts {scriptInfo :: ScriptInfo}
  | ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | StartObservingFrom {chainPoint :: ChainPoint}
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
  ChainPoint ->
  LocalNodeClientProtocols BlockType ChainPoint ChainTip slot tx txid txerr query IO
clientProtocols tracer networkId startingPoint =
  LocalNodeClientProtocols
    { localChainSyncClient = LocalChainSyncClient $ chainSyncClient tracer networkId startingPoint
    , localTxSubmissionClient = Nothing
    , localStateQueryClient = Nothing
    , localTxMonitoringClient = Nothing
    }

-- | Thrown when the user-provided custom point of intersection is unknown to
-- the local node. This may happen if users shut down their node quickly after
-- starting them and hold on a not-so-stable point of the chain. When they turn
-- the node back on, that point may no longer exist on the network if a fork
-- with deeper roots has been adopted in the meantime.
type IntersectionNotFoundException :: Type
newtype IntersectionNotFoundException = IntersectionNotFound {requestedPoint :: ChainPoint}
  deriving stock (Show)

instance Exception IntersectionNotFoundException

-- | Fetch all blocks via chain sync and trace their contents.
chainSyncClient ::
  forall m.
  MonadThrow m =>
  Tracer m ChainObserverLog ->
  NetworkId ->
  ChainPoint ->
  ChainSyncClient BlockType ChainPoint ChainTip m ()
chainSyncClient tracer networkId startingPoint =
  ChainSyncClient $
    pure $
      SendMsgFindIntersect [startingPoint] clientStIntersect
 where
  clientStIntersect :: ClientStIntersect BlockType ChainPoint ChainTip m ()
  clientStIntersect =
    ClientStIntersect
      { recvMsgIntersectFound = \_ _ ->
          ChainSyncClient (pure $ clientStIdle mempty)
      , recvMsgIntersectNotFound = \_ ->
          ChainSyncClient $ throwIO (IntersectionNotFound startingPoint)
      }

  clientStIdle :: UTxO -> ClientStIdle BlockType ChainPoint tip m ()
  clientStIdle utxo = SendMsgRequestNext (clientStNext utxo) (pure $ clientStNext utxo)

  clientStNext :: UTxO -> ClientStNext BlockType ChainPoint tip m ()
  clientStNext utxo =
    ClientStNext
      { recvMsgRollForward = \blockInMode _tip -> ChainSyncClient $ do
          case blockInMode of
            BlockInMode _ (Block _header txs) BabbageEraInCardanoMode -> do
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
        Init InitObservation{headId} -> (utxo', pure $ HeadInitTx{headId})
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
