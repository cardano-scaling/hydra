{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver.NodeClient where

import Hydra.Prelude

import Hydra.Cardano.Api (
  BlockNo,
  ChainPoint,
  NetworkId,
  SocketPath,
  Tx,
  UTxO,
 )
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Chain (OnChainTx (..))
import Hydra.Chain.Direct.Tx (
  HeadObservation (..),
  observeHeadTx,
 )
import Hydra.Contract (ScriptInfo)
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.ScriptRegistry (SerialisedScriptRegistry)

type ObserverHandler m = [ChainObservation] -> m ()

data ChainObservation
  = Tick
      { point :: ChainPoint
      , blockNo :: BlockNo
      }
  | HeadObservation
      { point :: ChainPoint
      , blockNo :: BlockNo
      , onChainTx :: OnChainTx Tx
      }
  deriving stock (Eq, Show, Generic)

defaultObserverHandler :: Applicative m => ObserverHandler m
defaultObserverHandler = const $ pure ()

newtype NodeClient m = NodeClient
  { follow :: Maybe ChainPoint -> ObserverHandler m -> m ()
  }

type ChainObserverLog :: Type
data ChainObserverLog
  = KnownScripts {scriptInfo :: ScriptInfo}
  | ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | ConnectingToExternalNode {networkId :: NetworkId}
  | StartObservingFrom {chainPoint :: ChainPoint}
  | HeadInitTx {headId :: HeadId}
  | HeadCommitTx {headId :: HeadId}
  | HeadCollectComTx {headId :: HeadId}
  | HeadDepositTx {headId :: HeadId}
  | HeadRecoverTx {headId :: HeadId}
  | HeadIncrementTx {headId :: HeadId}
  | HeadDecrementTx {headId :: HeadId}
  | HeadCloseTx {headId :: HeadId}
  | HeadFanoutTx {headId :: HeadId}
  | HeadAbortTx {headId :: HeadId}
  | HeadContestTx {headId :: HeadId}
  | Rollback {point :: ChainPoint}
  | RollForward {point :: ChainPoint, receivedTxIds :: [TxId]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

logOnChainTx :: OnChainTx Tx -> ChainObserverLog
logOnChainTx = \case
  OnInitTx{headId} -> HeadInitTx{headId}
  OnCommitTx{headId} -> HeadCommitTx{headId}
  OnCollectComTx{headId} -> HeadCollectComTx{headId}
  OnIncrementTx{headId} -> HeadIncrementTx{headId}
  OnDepositTx{headId} -> HeadDepositTx{headId}
  OnRecoverTx{headId} -> HeadRecoverTx{headId}
  OnDecrementTx{headId} -> HeadDecrementTx{headId}
  OnCloseTx{headId} -> HeadCloseTx{headId}
  OnFanoutTx{headId} -> HeadFanoutTx{headId}
  OnAbortTx{headId} -> HeadAbortTx{headId}
  OnContestTx{headId} -> HeadContestTx{headId}

observeTx :: NetworkId -> SerialisedScriptRegistry -> UTxO -> Tx -> (UTxO, Maybe HeadObservation)
observeTx networkId serialisedScriptRegistry utxo tx =
  let utxo' = adjustUTxO tx utxo
   in case observeHeadTx networkId serialisedScriptRegistry utxo tx of
        NoHeadTx -> (utxo, Nothing)
        observation -> (utxo', pure observation)

observeAll :: NetworkId -> SerialisedScriptRegistry -> UTxO -> [Tx] -> (UTxO, [HeadObservation])
observeAll networkId serialisedScriptRegistry utxo txs =
  second reverse $ foldr go (utxo, []) txs
 where
  go :: Tx -> (UTxO, [HeadObservation]) -> (UTxO, [HeadObservation])
  go tx (utxo'', observations) =
    case observeTx networkId serialisedScriptRegistry utxo'' tx of
      (utxo', Nothing) -> (utxo', observations)
      (utxo', Just observation) -> (utxo', observation : observations)
