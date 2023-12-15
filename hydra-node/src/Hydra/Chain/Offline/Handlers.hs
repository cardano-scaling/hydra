{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Offline.Handlers (
  mkFakeL1Chain,
) where

import Hydra.Chain (
  Chain (Chain, draftCommitTx, postTx, submitTx),
  ChainEvent (Observation, newChainState, observedTx),
  HeadParameters (HeadParameters),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError (FailedToDraftTxNotInitializing),
  confirmedSnapshot,
  contestationDeadline,
  snapshotNumber,
 )
import Hydra.Chain.Direct.Handlers (DirectChainLog (ToPost, toPost), LocalChainState, getLatest)
import Hydra.Chain.Direct.State (ChainStateAt (ChainStateAt))
import Hydra.ContestationPeriod (ContestationPeriod, toNominalDiffTime)
import Hydra.HeadId (HeadId, HeadSeed (UnsafeHeadSeed))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Prelude
import Hydra.Snapshot (Snapshot (number), getSnapshot)
import Hydra.OnChainId (OnChainId(UnsafeOnChainId))

mkFakeL1Chain ::
  ContestationPeriod ->
  LocalChainState IO Tx ->
  Tracer IO DirectChainLog ->
  HeadId ->
  (ChainEvent Tx -> IO ()) ->
  Chain Tx IO
mkFakeL1Chain contestationPeriod localChainState tracer ownHeadId callback =
  Chain
    { submitTx = const $ pure ()
    , draftCommitTx = \_ _ -> pure $ Left FailedToDraftTxNotInitializing
    , postTx = \tx -> do
        cst@ChainStateAt{} <- atomically (getLatest localChainState)
        traceWith tracer $ ToPost{toPost = tx}

        let headId = ownHeadId
        let offlineHeadSeed = UnsafeHeadSeed "OfflineHeadSeed_"
            headSeed = offlineHeadSeed
            ownParticipant = UnsafeOnChainId "___OfflineHeadParticipant___"
            participants = [ownParticipant]
        _ <- case tx of
          InitTx{headParameters} ->
            callback $ Observation{newChainState = cst, observedTx = OnInitTx{headId, headParameters, headSeed, participants}}
          AbortTx{} ->
            callback $ Observation{newChainState = cst, observedTx = OnAbortTx{headId}}
          CollectComTx{} ->
            callback $ Observation{newChainState = cst, observedTx = OnCollectComTx{headId}}
          CloseTx{confirmedSnapshot} -> do
            contestationDeadline <- addUTCTime (toNominalDiffTime contestationPeriod) <$> getCurrentTime
            callback $
              Observation
                { newChainState = cst
                , observedTx =
                    OnCloseTx{headId, snapshotNumber = number $ getSnapshot confirmedSnapshot, contestationDeadline}
                }
          ContestTx{confirmedSnapshot} ->
            -- TODO: this shouldn't really happen... make it impossible to do contestation in offline mode?
            callback $
              Observation
                { newChainState = cst
                , observedTx =
                    OnContestTx{snapshotNumber = number $ getSnapshot confirmedSnapshot, headId}
                }
          FanoutTx{} ->
            callback $
              Observation
                { newChainState = cst
                , observedTx =
                    OnFanoutTx{headId}
                }
        pure ()
    }
