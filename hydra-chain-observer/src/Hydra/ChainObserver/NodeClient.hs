{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver.NodeClient where

import Hydra.Prelude

import Cardano.Api.UTxO (fromPairs, pairs)
import Hydra.Cardano.Api (
  BlockNo,
  ChainPoint,
  NetworkId,
  SocketPath,
  Tx,
  TxIn (..),
  TxIx (..),
  UTxO,
  toCtxUTxOTxOut,
  txIns',
  txOuts',
 )
import Hydra.Cardano.Api.Prelude (TxId)
import Hydra.Chain.Chain (OnChainTx (..))
import Hydra.Contract (ScriptInfo)
import Hydra.Plutus.Extras (posixToUTCTime)
import Hydra.Tx (HeadParameters (..), txId)
import Hydra.Tx.Close (ClosedThreadOutput (..))
import Hydra.Tx.HeadId (HeadId (..), txInToHeadSeed)
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
  , observedTx :: Maybe (OnChainTx Tx)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- FIXME: Check if this is needed
-- instance Arbitrary ChainObservation where
--   arbitrary = genericArbitrary
--   shrink = genericShrink

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

-- | Utility function to "adjust" a `UTxO` set given a `Tx`
--
--  The inputs from the `Tx` are removed from the internal map of the `UTxO` and
--  the outputs added, correctly indexed by the `TxIn`. This function is useful
--  to manually maintain a `UTxO` set without caring too much about the `Ledger`
--  rules.
--  TODO: This is exact duplicate from 'Hydra.Ledger.Cardano'
adjustUTxO :: Tx -> UTxO -> UTxO
adjustUTxO tx utxo =
  let txid = txId tx
      consumed = txIns' tx
      produced =
        toCtxUTxOTxOut
          <$> fromPairs ((\(txout, ix) -> (TxIn txid (TxIx ix), txout)) <$> zip (txOuts' tx) [0 ..])
      utxo' = fromPairs $ filter (\(txin, _) -> txin `notElem` consumed) $ pairs utxo
   in utxo' <> produced

observeAll :: NetworkId -> UTxO -> [Tx] -> (UTxO, [HeadObservation])
observeAll networkId utxo txs =
  second reverse $ foldr go (utxo, []) txs
 where
  go :: Tx -> (UTxO, [HeadObservation]) -> (UTxO, [HeadObservation])
  go tx (utxo'', observations) =
    case observeTx networkId utxo'' tx of
      (utxo', Nothing) -> (utxo', observations)
      (utxo', Just observation) -> (utxo', observation : observations)

convertObservation :: HeadObservation -> Maybe (OnChainTx Tx)
convertObservation = \case
  NoHeadTx -> Nothing
  Init InitObservation{headId, contestationPeriod, parties, seedTxIn, participants} ->
    pure
      OnInitTx
        { headId
        , headSeed = txInToHeadSeed seedTxIn
        , headParameters = HeadParameters{contestationPeriod, parties}
        , participants
        }
  Abort AbortObservation{headId} ->
    pure OnAbortTx{headId}
  Commit CommitObservation{headId, party, committed} ->
    pure OnCommitTx{headId, party, committed}
  CollectCom CollectComObservation{headId} ->
    pure OnCollectComTx{headId}
  Deposit DepositObservation{headId, deposited, depositTxId, deadline} ->
    pure $ OnDepositTx{headId, deposited, depositTxId, deadline = posixToUTCTime deadline}
  Recover RecoverObservation{headId, recoveredTxId, recoveredUTxO} ->
    pure OnRecoverTx{headId, recoveredTxId, recoveredUTxO}
  Increment IncrementObservation{headId, newVersion, depositTxId} ->
    pure OnIncrementTx{headId, newVersion, depositTxId}
  Decrement DecrementObservation{headId, newVersion, distributedUTxO} ->
    pure OnDecrementTx{headId, newVersion, distributedUTxO}
  -- XXX: Needing ClosedThreadOutput feels weird here
  Close CloseObservation{headId, snapshotNumber, threadOutput = ClosedThreadOutput{closedContestationDeadline}} ->
    pure
      OnCloseTx
        { headId
        , snapshotNumber
        , contestationDeadline = posixToUTCTime closedContestationDeadline
        }
  Contest ContestObservation{contestationDeadline, headId, snapshotNumber} ->
    pure OnContestTx{contestationDeadline, headId, snapshotNumber}
  Fanout FanoutObservation{headId, fanoutUTxO} ->
    pure OnFanoutTx{headId, fanoutUTxO}
