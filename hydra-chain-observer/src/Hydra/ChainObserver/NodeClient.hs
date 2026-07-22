{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver.NodeClient where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as LedgerHashes
import Data.Map.Strict qualified as Map
import Data.Version (Version)
import Hydra.Cardano.Api hiding (Block)
import Hydra.ChainObserver.VersionRegistry (KnownVersion (..), loadKnownVersions)
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Tx.HeadId (HeadId (..))
import Hydra.Tx.Observe (
  CloseObservation (..),
  ContestObservation (..),
  DecrementObservation (..),
  DepositObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  IncrementObservation (..),
  InitObservation (..),
  PartialFanoutObservation (..),
  RecoverObservation (..),
  observeHeadTx,
 )

type ObserverHandler m = [(Maybe Version, ChainObservation)] -> m ()

data ChainObservation
  = ChainObservation
  { point :: ChainPoint
  , blockNo :: BlockNo
  , observed :: HeadObservation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data NodeClient m = NodeClient
  { follow :: Maybe ChainPoint -> ObserverHandler m -> m ()
  , networkId :: NetworkId
  }

type ChainObserverLog :: Type
data ChainObserverLog
  = KnownVersions {knownVersions :: [Version]}
  | ConnectingToNode {nodeSocket :: SocketPath, networkId :: NetworkId}
  | ConnectingToExternalNode {networkId :: NetworkId}
  | StartObservingFrom {chainPoint :: ChainPoint}
  | HeadInitTx {headId :: HeadId}
  | HeadDepositTx {headId :: HeadId}
  | HeadRecoverTx {headId :: HeadId}
  | HeadIncrementTx {headId :: HeadId}
  | HeadDecrementTx {headId :: HeadId}
  | HeadCloseTx {headId :: HeadId}
  | HeadFanoutTx {headId :: HeadId}
  | HeadPartialFanoutTx {headId :: HeadId}
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
  Deposit DepositObservation{headId} -> Just HeadDepositTx{headId}
  Recover RecoverObservation{headId} -> Just HeadRecoverTx{headId}
  Increment IncrementObservation{headId} -> Just HeadIncrementTx{headId}
  Decrement DecrementObservation{headId} -> Just HeadDecrementTx{headId}
  Close CloseObservation{headId} -> Just HeadCloseTx{headId}
  Contest ContestObservation{headId} -> Just HeadContestTx{headId}
  Fanout FanoutObservation{headId} -> Just HeadFanoutTx{headId}
  FinalPartialFanout FanoutObservation{headId} -> Just HeadFanoutTx{headId}
  PartialFanout PartialFanoutObservation{headId} -> Just HeadPartialFanoutTx{headId}

-- | Detect which known Hydra version's head validator script a transaction
-- touches, by checking outputs against known script hashes and inputs against
-- the tracked Hydra UTxO. Returns Nothing for non-Hydra transactions, acting
-- as a cheap pre-filter before the full observeHeadTx pipeline.
detectVersion :: UTxO -> Tx -> Maybe KnownVersion
detectVersion utxo tx = find (txTouchesHydraScript utxo tx) loadKnownVersions

txTouchesHydraScript :: UTxO -> Tx -> KnownVersion -> Bool
txTouchesHydraScript utxo tx KnownVersion{kvHeadScriptHash, kvDepositScriptHash} =
  any outputAtAnyHash (txOuts' tx)
    || any inputAtAnyHash (txIns' tx)
 where
  headSH = toShelleyScriptHash kvHeadScriptHash
  depositSH = toShelleyScriptHash <$> kvDepositScriptHash

  outputAtAnyHash :: TxOut CtxTx -> Bool
  outputAtAnyHash out =
    outputAtScriptHash headSH out
      || maybe False (`outputAtScriptHash` out) depositSH

  inputAtAnyHash :: TxIn -> Bool
  inputAtAnyHash txIn =
    inputAtAnyScriptHash headSH txIn
      || maybe False (`inputAtAnyScriptHash` txIn) depositSH

  inputAtAnyScriptHash :: LedgerHashes.ScriptHash -> TxIn -> Bool
  inputAtAnyScriptHash sh txIn =
    case Map.lookup txIn (UTxO.toMap utxo) of
      Nothing -> False
      Just out -> outputAtScriptHash sh out

outputAtScriptHash :: LedgerHashes.ScriptHash -> TxOut ctx -> Bool
outputAtScriptHash sh (TxOut addr _ _ _) =
  case addr of
    ShelleyAddressInEra (ShelleyAddress _ (Ledger.ScriptHashObj addrSH) _) -> sh == addrSH
    _ -> False

observeTx :: NetworkId -> UTxO -> Tx -> (UTxO, Maybe HeadObservation)
observeTx networkId utxo tx =
  let utxo' = adjustUTxO tx utxo
   in case observeHeadTx networkId utxo tx of
        NoHeadTx -> (utxo, Nothing)
        observation -> (utxo', pure observation)

-- | Like observeTx, but pre-filters via detectVersion. When detectVersion
-- returns Nothing, observeHeadTx is never called and the UTxO is unchanged.
observeTxVersioned ::
  NetworkId -> UTxO -> Tx -> (UTxO, Maybe (Version, HeadObservation))
observeTxVersioned networkId utxo tx =
  case detectVersion utxo tx of
    Nothing -> (utxo, Nothing)
    Just kv ->
      let (utxo', mObs) = observeTx networkId utxo tx
       in (utxo', fmap (kvVersion kv,) mObs)

observeAll :: NetworkId -> UTxO -> [Tx] -> (UTxO, [(Version, HeadObservation)])
observeAll networkId utxo txs =
  second reverse $ foldl' go (utxo, []) txs
 where
  go :: (UTxO, [(Version, HeadObservation)]) -> Tx -> (UTxO, [(Version, HeadObservation)])
  go (utxo'', observations) tx =
    case observeTxVersioned networkId utxo'' tx of
      (utxo', Nothing) -> (utxo', observations)
      (utxo', Just obs) -> (utxo', obs : observations)
