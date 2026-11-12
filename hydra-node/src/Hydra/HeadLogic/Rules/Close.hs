{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Closing and contesting share snapshot adoption, but differ in when they
-- counter-contest: a successfully adopted contest needs no further post.
module Hydra.HeadLogic.Rules.Close where

import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (
  Program,
  change,
  decide,
  emit,
 )
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot, verifiedSnapshot, verifySeenSnapshot)
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
  SeenSnapshot (SeenSnapshot, snapshot),
  mkSeenSnapshot,
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

onOpenClientClose :: OpenState tx -> Program tx ()
onOpenClientClose OpenState{headId, parameters, coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot, version}} =
  emit $ OnChainEffect CloseTx{headId, headParameters = parameters, openVersion = version, closingSnapshot = confirmedSnapshot}

onOpenChainCloseTx :: IsChainState tx => OpenState tx -> ChainStateType tx -> SnapshotNumber -> UTCTime -> MultiSignature (Snapshot tx) -> Program tx ()
onOpenChainCloseTx os@OpenState{headId, parameters, coordinatedHeadState} newChainState closedNumber contestationDeadline signatures = do
  void $ adoptSnapshot parameters.parties closedNumber signatures candidate (void . Coordinated.stage (Coordinated.start os) . Coordinated.adoptSnapshot)
  change HeadClosed{headId, snapshotNumber = closedNumber, chainState = newChainState, contestationDeadline}
  -- Compare the confirmed snapshot from before adoption, as the existing rule does.
  postNewerSnapshot headId parameters version confirmedSnapshot closedNumber
 where
  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, version} = coordinatedHeadState
  candidate = case seenSnapshot of
    seen@SeenSnapshot{} -> Just seen
    _ -> Nothing

onClosedChainContestTx :: IsTx tx => ClosedState tx -> ChainStateType tx -> SnapshotNumber -> UTCTime -> MultiSignature (Snapshot tx) -> Program tx ()
onClosedChainContestTx ClosedState{headId, parameters, confirmedSnapshot, version, unconfirmedSnapshot} newChainState snapshotNumber contestationDeadline signatures = do
  adopted <- adoptSnapshot parameters.parties snapshotNumber signatures ((`mkSeenSnapshot` mempty) <$> unconfirmedSnapshot) $ \evidence ->
    change SnapshotConfirmed{headId, snapshot = Just $ verifiedSnapshot evidence, signatures}
  change HeadContested{headId, chainState = newChainState, contestationDeadline, snapshotNumber}
  unless adopted $ postNewerSnapshot headId parameters version confirmedSnapshot snapshotNumber

adoptSnapshot :: IsTx tx => [Party] -> SnapshotNumber -> MultiSignature (Snapshot tx) -> Maybe (SeenSnapshot tx) -> (VerifiedSnapshot tx -> Program tx ()) -> Program tx Bool
adoptSnapshot parties observedNumber signatures candidate retain = case candidate of
  Just seen@SeenSnapshot{snapshot} | snapshot.number == observedNumber -> do
    void $ decide "chain observation matches the unconfirmed snapshot number" True
    verified <- verifySeenSnapshot (vkey <$> parties) seen signatures
    valid <- decide "observed snapshot signatures verify" (isRight verified)
    traverse_ retain verified
    pure valid
  _ -> decide "chain observation matches the unconfirmed snapshot number" False

postNewerSnapshot :: IsTx tx => HeadId -> HeadParameters -> SnapshotVersion -> ConfirmedSnapshot tx -> SnapshotNumber -> Program tx ()
postNewerSnapshot headId headParameters openVersion confirmedSnapshot observedNumber = do
  newer <- decide "confirmed snapshot is newer than the on-chain snapshot" ((getSnapshot confirmedSnapshot).number > observedNumber)
  when newer $ emit $ OnChainEffect ContestTx{headId, headParameters, openVersion, contestingSnapshot = confirmedSnapshot}
