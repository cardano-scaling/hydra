{-# LANGUAGE DisambiguateRecordFields #-}

module Hydra.Chain.Offline.Persistence (
  initializeStateIfOffline,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Hydra.Cardano.Api (Tx)
import Hydra.Chain (
  ChainEvent (Observation, observedTx),
  ChainStateHistory,
  HeadParameters (..),
  OnChainTx (..),
  committed,
  initHistory,
  newChainState,
  party,
 )
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.HeadId (HeadId, HeadSeed (UnsafeHeadSeed))
import Hydra.HeadLogic (StateChanged (SnapshotConfirmed, snapshot))
import Hydra.Ledger (IsTx (UTxOType))
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (PersistenceIncremental, append, loadAll), createPersistenceIncremental)
import Hydra.Snapshot (Snapshot (Snapshot, utxo))
import UnliftIO.IO.File (writeBinaryFileDurableAtomic)

initializeStateIfOffline ::
  ChainStateHistory Tx ->
  UTxOType Tx ->
  HeadId ->
  Party ->
  ContestationPeriod ->
  (ChainEvent Tx -> IO ()) ->
  IO ()
initializeStateIfOffline chainStateHistory initialUTxO ownHeadId ownParty contestationPeriod callback = do
  let emptyChainStateHistory = initHistory initialChainState

  -- if we don't have a chainStateHistory to restore from disk from, start a new one
  when (chainStateHistory == emptyChainStateHistory) $ do
    callback $
      Observation
        { newChainState = initialChainState
        , observedTx =
            OnInitTx
              { headId = ownHeadId
              , headParameters = HeadParameters{parties = [ownParty], contestationPeriod}
              , headSeed = UnsafeHeadSeed "OfflineHeadSeed_"
              , participants = []
              }
        }

    callback $
      Observation
        { newChainState = initialChainState
        , observedTx =
            OnCommitTx
              { party = ownParty
              , committed = initialUTxO
              , headId = ownHeadId
              }
        }
