{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DisambiguateRecordFields#-}
module Hydra.Chain.Offline.Handlers (
  mkFakeL1Chain
) where

import Hydra.Prelude
import Hydra.Chain.Direct.State (ChainStateAt(ChainStateAt), chainState, ChainState (Initial), ChainContext, commit')
import Hydra.Chain.Direct.Handlers (DirectChainLog(ToPost, toPost), LocalChainState, getLatest)
import Hydra.Chain (PostChainTx(headParameters, InitTx, AbortTx, CollectComTx, CloseTx, ContestTx, confirmedSnapshot, FanoutTx), ChainEvent (Observation, newChainState, observedTx), snapshotNumber, confirmedSnapshot, headId, HeadParameters (HeadParameters), Chain (postTx, draftCommitTx, submitTx, Chain), contestationDeadline, OnChainTx (OnInitTx, headId, OnAbortTx, OnCollectComTx, OnCloseTx, parties, contestationDeadline, contestationPeriod, OnContestTx, OnFanoutTx), HeadParameters (HeadParameters), snapshotNumber, PostTxError (FailedToDraftTxNotInitializing))
import Hydra.Snapshot (getSnapshot, Snapshot (number))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Ledger.Cardano (Tx)

import Hydra.HeadId(HeadId)

mkFakeL1Chain ::
  LocalChainState IO Tx
  -> Tracer IO DirectChainLog
  -> ChainContext
  -> HeadId
  -> (ChainEvent Tx -> IO ())
  -> Chain Tx IO
mkFakeL1Chain localChainState tracer ctx ownHeadId callback =
  Chain {
    submitTx = const $ pure (),
    -- TODO(Elaine): this won't succeed currently because ctx will be bottom
    -- it doesn't make much sense in offline mode in general, so we should refactor it out
    draftCommitTx = \utxoToCommit -> do
      ChainStateAt{chainState} <- atomically (getLatest localChainState)
      case chainState of
        Initial st ->
          -- callback $ Observation { newChainState = cst, observedTx = OnCommitTx {party = ownParty ctx, committed = utxoToCommit}}
          pure (commit' ctx st utxoToCommit)
        _ ->  pure $ Left FailedToDraftTxNotInitializing,
    postTx = \tx -> do
      cst@ChainStateAt{chainState=_chainState} <- atomically (getLatest localChainState)
      traceWith tracer $ ToPost{toPost = tx}
      
      let headId = ownHeadId
      _ <- case tx of
                InitTx{headParameters=HeadParameters contestationPeriod parties} ->
                  callback $ Observation { newChainState = cst, observedTx = OnInitTx {headId = headId, parties=parties, contestationPeriod}}
                AbortTx{} ->
                  callback $ Observation { newChainState = cst, observedTx = OnAbortTx {}}
                CollectComTx{} ->
                  callback $ Observation { newChainState = cst, observedTx = OnCollectComTx {}}
                CloseTx{confirmedSnapshot} -> do
                  inOneMinute <- addUTCTime 60 <$> getCurrentTime
                  callback $ Observation { newChainState = cst, observedTx =
                    OnCloseTx {headId, snapshotNumber = number $ getSnapshot confirmedSnapshot, contestationDeadline=inOneMinute}} -- ELAINE TODO: probably we shouldnt allow the clietn to do contestation in offline mode ?
                ContestTx{confirmedSnapshot} -> -- this shouldnt really happen, i dont think we should allow contesting in offline mode
                  callback $ Observation { newChainState = cst, observedTx =
                      OnContestTx{snapshotNumber = number $ getSnapshot confirmedSnapshot}}
                FanoutTx{} ->
                  callback $ Observation { newChainState = cst, observedTx =
                      OnFanoutTx{}}
      pure ()}