{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver.NodeClient where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as LedgerHashes
import Data.Map.Strict qualified as Map
import Data.Version (Version)
import Hydra.Cardano.Api hiding (Block)
import Hydra.ChainObserver.VersionRegistry (KnownVersion (..))
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
  NotAnInitReason,
  PartialFanoutObservation (..),
  RecoverObservation (..),
  observeHeadTxWithReason,
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
  | -- | A transaction minted a head's tokens but carries a datum that cannot be
    -- observed as an init. The head minting policy constrains none of the datum
    -- fields involved.
    HeadInitTxRejected {rejectedTxId :: TxId, notAnInitReason :: NotAnInitReason}
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
detectVersion :: [KnownVersion] -> UTxO -> Tx -> Maybe KnownVersion
detectVersion knownVersions utxo tx = find (txTouchesHydraScript utxo tx) knownVersions

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

-- | Observe a transaction, additionally reporting why one that minted a head's
-- tokens could not be observed as an init (see 'isMalformedInit'). There is
-- nothing to report to an explorer in that case, but it is worth logging.
observeTx :: NetworkId -> UTxO -> Tx -> (UTxO, Maybe HeadObservation, Maybe NotAnInitReason)
observeTx networkId utxo tx =
  let utxo' = adjustUTxO tx utxo
      (observation, mNotAnInitReason) = observeHeadTxWithReason networkId utxo tx
   in case observation of
        NoHeadTx -> (utxo, Nothing, mNotAnInitReason)
        _ -> (utxo', Just observation, mNotAnInitReason)

-- | Like observeTx, but pre-filters via detectVersion. When detectVersion
-- returns Nothing, observeHeadTx is never called and the UTxO is unchanged.
observeTxVersioned ::
  [KnownVersion] ->
  NetworkId ->
  UTxO ->
  Tx ->
  (UTxO, Maybe (Version, HeadObservation), Maybe NotAnInitReason)
observeTxVersioned knownVersions networkId utxo tx =
  case detectVersion knownVersions utxo tx of
    Nothing -> (utxo, Nothing, Nothing)
    Just kv ->
      let (utxo', mObs, mNotAnInitReason) = observeTx networkId utxo tx
       in (utxo', fmap (kvVersion kv,) mObs, mNotAnInitReason)

-- | What 'observeAll' saw in a sequence of transactions, in order.
data BlockObservations = BlockObservations
  { adjustedUTxO :: !UTxO
  , observations :: ![(Version, HeadObservation)]
  , rejectedInits :: ![(TxId, NotAnInitReason)]
  -- ^ See 'observeTx'.
  }

observeAll :: [KnownVersion] -> NetworkId -> UTxO -> [Tx] -> BlockObservations
observeAll knownVersions networkId utxo txs =
  let BlockObservations{adjustedUTxO, observations, rejectedInits} =
        foldl' go (BlockObservations utxo [] []) txs
   in BlockObservations
        { adjustedUTxO
        , observations = reverse observations
        , rejectedInits = reverse rejectedInits
        }
 where
  -- NOTE: The fields are strict because 'foldl'' only forces the accumulator
  -- to WHNF. Lazy ones would leave a thunk per transaction of the block,
  -- retaining its 'Tx' and the preceding UTxO snapshot.
  go :: BlockObservations -> Tx -> BlockObservations
  go BlockObservations{adjustedUTxO = utxo'', observations, rejectedInits} tx =
    let (utxo', mObs, mNotAnInitReason) = observeTxVersioned knownVersions networkId utxo'' tx
     in BlockObservations
          { adjustedUTxO = utxo'
          , observations = maybe observations (: observations) mObs
          , rejectedInits = maybe rejectedInits (\reason -> (getTxId (getTxBody tx), reason) : rejectedInits) mNotAnInitReason
          }
