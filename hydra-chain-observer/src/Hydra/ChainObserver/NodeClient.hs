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
import Hydra.Contract (ScriptInfo)
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.Observe (HeadObservation (..), observeHeadTx)

type ObserverHandler m = [ChainObservation] -> m ()

data ChainObservation = ChainObservation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , observedTx :: Maybe (OnChainTx Tx)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ChainObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

data NodeClient m = NodeClient
  { follow :: Maybe ChainPoint -> ObserverHandler m -> m ()
  , networkId :: NetworkId
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

observeTx :: NetworkId -> UTxO -> Tx -> (UTxO, Maybe HeadObservation)
observeTx networkId utxo tx =
  let utxo' = adjustUTxO tx utxo
   in case observeHeadTx networkId utxo tx of
        NoHeadTx -> (utxo, Nothing)
        observation -> (utxo', pure observation)

observeAll :: NetworkId -> UTxO -> [Tx] -> (UTxO, [HeadObservation])
observeAll networkId utxo txs =
  second reverse $ foldr go (utxo, []) txs
 where
  go :: Tx -> (UTxO, [HeadObservation]) -> (UTxO, [HeadObservation])
  go tx (utxo'', observations) =
    case observeTx networkId utxo'' tx of
      (utxo', Nothing) -> (utxo', observations)
      (utxo', Just observation) -> (utxo', observation : observations)
