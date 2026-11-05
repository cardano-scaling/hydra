{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Closing and contesting share snapshot adoption, but differ in when they
-- counter-contest: a successfully adopted contest needs no further post.
module Hydra.HeadLogic.Rules.Close where

import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (
  Program,
  change,
  decide,
  emit,
  verifySnapshot,
 )
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    confirmedSnapshot,
    headId,
    parameters,
    unconfirmedSnapshot,
    version
  ),
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  SeenSnapshot (SeenSnapshot, signableBytes, snapshot),
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx)
import Hydra.Tx.Crypto (MultiSignature, Verified (..), getSignableRepresentation)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

onOpenClientClose :: OpenState tx -> Program tx ledger signing ()
onOpenClientClose OpenState{headId, parameters, coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot, version}} =
  emit $ OnChainEffect CloseTx{headId, headParameters = parameters, openVersion = version, closingSnapshot = confirmedSnapshot}

onOpenChainCloseTx :: IsTx tx => OpenState tx -> ChainStateType tx -> SnapshotNumber -> UTCTime -> MultiSignature (Snapshot tx) -> Program tx ledger signing ()
onOpenChainCloseTx OpenState{headId, parameters, coordinatedHeadState} newChainState closedNumber contestationDeadline signatures = do
  void $ adoptSnapshot headId parameters.parties closedNumber signatures candidate
  change HeadClosed{headId, snapshotNumber = closedNumber, chainState = newChainState, contestationDeadline}
  -- Compare the confirmed snapshot from before adoption, as the existing rule does.
  postNewerSnapshot headId parameters version confirmedSnapshot closedNumber
 where
  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, version} = coordinatedHeadState
  candidate = case seenSnapshot of
    SeenSnapshot{snapshot, signableBytes} -> Just (snapshot, signableBytes)
    _ -> Nothing

onClosedChainContestTx :: IsTx tx => ClosedState tx -> ChainStateType tx -> SnapshotNumber -> UTCTime -> MultiSignature (Snapshot tx) -> Program tx ledger signing ()
onClosedChainContestTx ClosedState{headId, parameters, confirmedSnapshot, version, unconfirmedSnapshot} newChainState snapshotNumber contestationDeadline signatures = do
  adopted <- adoptSnapshot headId parameters.parties snapshotNumber signatures ((\snapshot -> (snapshot, getSignableRepresentation snapshot)) <$> unconfirmedSnapshot)
  change HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
  unless adopted $ postNewerSnapshot headId parameters version confirmedSnapshot snapshotNumber

adoptSnapshot :: HeadId -> [Party] -> SnapshotNumber -> MultiSignature (Snapshot tx) -> Maybe (Snapshot tx, ByteString) -> Program tx ledger signing Bool
adoptSnapshot headId parties observedNumber signatures = \case
  Just (snapshot, bytes) | snapshot.number == observedNumber -> do
    void $ decide "chain observation matches the unconfirmed snapshot number" True
    verified <- verifySnapshot (vkey <$> parties) signatures bytes
    valid <- decide "observed snapshot signatures verify" (verified == Verified)
    when valid $ change SnapshotConfirmed{headId, snapshot = Just snapshot, signatures}
    pure valid
  _ -> decide "chain observation matches the unconfirmed snapshot number" False

postNewerSnapshot :: IsTx tx => HeadId -> HeadParameters -> SnapshotVersion -> ConfirmedSnapshot tx -> SnapshotNumber -> Program tx ledger signing ()
postNewerSnapshot headId headParameters openVersion confirmedSnapshot observedNumber = do
  newer <- decide "confirmed snapshot is newer than the on-chain snapshot" ((getSnapshot confirmedSnapshot).number > observedNumber)
  when newer $ emit $ OnChainEffect ContestTx{headId, headParameters, openVersion, contestingSnapshot = confirmedSnapshot}
