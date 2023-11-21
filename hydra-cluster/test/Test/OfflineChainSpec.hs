{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.OfflineChainSpec where

import Control.Concurrent.STM (newEmptyTMVarIO, putTMVar, takeTMVar)
import Hydra.Chain (Chain (Chain, postTx), ChainEvent, HeadParameters (HeadParameters, contestationPeriod, parties), PostChainTx (InitTx, headParameters), initHistory)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (withOfflineChain)
import Hydra.Cluster.Fixture (Actor (Alice), aliceSk)
import Hydra.Cluster.Util (keysFor, seedInitialUTxOFromOffline)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Options (OfflineConfig (OfflineConfig, initialUTxOFile, ledgerGenesisFile), OfflineUTxOWriteBackConfig (), defaultContestationPeriod, defaultOfflineConfig, initialUTxOFile)
import Hydra.Party (deriveParty)
import Hydra.Prelude
import Test.Hydra.Prelude

-- TODO(Elaine): replace with some offlinechainlog?

import Hydra.Chain (OnChainTx (OnCommitTx, committed, party))
import Hydra.Chain.Direct.Handlers (DirectChainLog)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Party (Party)

import Hydra.Chain (ChainEvent (observedTx), OnChainTx (OnAbortTx), PostChainTx (AbortTx))
import Hydra.HeadId (HeadId (HeadId))
import Hydra.Ledger (IsTx (UTxOType))
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)
import Test.DirectChainSpec (hasInitTxWith, observesInTime)

data OfflineChainTest tx m = OfflineChainTest
  { postTx :: PostChainTx tx -> m ()
  , waitCallback :: m (ChainEvent tx)
  }

withOfflineChainTest ::
  Tracer IO DirectChainLog ->
  OfflineConfig ->
  Party ->
  (OfflineChainTest Tx IO -> IO b) ->
  IO b
withOfflineChainTest tracer offlineConfig party action = do
  eventMVar <- newEmptyTMVarIO

  let callback event = atomically $ putTMVar eventMVar event
      globals = undefined
      contestationPeriod = defaultContestationPeriod
  withOfflineChain tracer offlineConfig globals (HeadId "HeadId") party contestationPeriod (initHistory initialChainState) callback $ \Chain{postTx} -> do
    action
      OfflineChainTest
        { postTx
        , waitCallback = atomically $ takeTMVar eventMVar
        }

spec :: Spec
spec = pure ()
