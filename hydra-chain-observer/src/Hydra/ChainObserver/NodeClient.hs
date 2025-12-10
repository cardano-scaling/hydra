{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver.NodeClient where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  BlockNo,
  ChainPoint,
  NetworkId,
  SocketPath,
  Tx,
  UTxO,
 )
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Contract (HydraScriptCatalogue)
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.Observe (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  DepositObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  InitObservation (..),
  RecoverObservation (..),
  observeHeadTx,
 )

type ObserverHandler m = [ChainObservation] -> m ()

data ChainObservation
  = ChainObservation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , observed :: HeadObservation
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
  = KnownScripts {hydraScriptCatalogue :: HydraScriptCatalogue}
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

logObservation :: HeadObservation -> Maybe ChainObserverLog
logObservation = \case
  NoHeadTx -> Nothing
  Init InitObservation{headId} -> Just HeadInitTx{headId}
  Commit CommitObservation{headId} -> Just HeadCommitTx{headId}
  Abort AbortObservation{headId} -> Just HeadAbortTx{headId}
  CollectCom CollectComObservation{headId} -> Just HeadCollectComTx{headId}
  Deposit DepositObservation{headId} -> Just HeadDepositTx{headId}
  Recover RecoverObservation{headId} -> Just HeadRecoverTx{headId}
  Increment IncrementObservation{headId} -> Just HeadIncrementTx{headId}
  Decrement DecrementObservation{headId} -> Just HeadDecrementTx{headId}
  Close CloseObservation{headId} -> Just HeadCloseTx{headId}
  Contest ContestObservation{headId} -> Just HeadContestTx{headId}
  Fanout FanoutObservation{headId} -> Just HeadFanoutTx{headId}

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
