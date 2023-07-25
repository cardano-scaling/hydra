module Hydra.HeadLogic.Snapshot where

import Hydra.Prelude
import Hydra.Snapshot (SnapshotNumber, Snapshot (..), getSnapshot)
import Hydra.Chain (HeadParameters (..))
import Hydra.Party (Party)
import Hydra.HeadLogic.State (Environment (..), HeadState (..), OpenState (..), CoordinatedHeadState (..), SeenSnapshot (..), seenSnapshotNumber)
import Hydra.HeadLogic.Outcome (Outcome (..), Effect(NetworkEffect))
import Hydra.Ledger (IsTx (txId))
import Data.List (elemIndex)
import Hydra.Network.Message (Message(ReqSn))


-- * Snapshot helper functions

data SnapshotOutcome tx
  = ShouldSnapshot SnapshotNumber [tx] -- TODO(AB) : should really be a Set (TxId tx)
  | ShouldNotSnapshot NoSnapshotReason
  deriving (Eq, Show, Generic)

data NoSnapshotReason
  = NotLeader SnapshotNumber
  | SnapshotInFlight SnapshotNumber
  | NoTransactionsToSnapshot
  deriving (Eq, Show, Generic)

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- | Snapshot emission decider
newSn :: Environment -> HeadParameters -> CoordinatedHeadState tx -> SnapshotOutcome tx
newSn Environment{party} parameters CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs} =
  if
      | not (isLeader parameters party nextSn) ->
          ShouldNotSnapshot $ NotLeader nextSn
      | -- NOTE: This is different than in the spec. If we use seenSn /=
        -- confirmedSn here, we implicitly require confirmedSn <= seenSn. Which
        -- may be an acceptable invariant, but we have property tests which are
        -- more strict right now. Anyhow, we can be more expressive.
        snapshotInFlight ->
          ShouldNotSnapshot $ SnapshotInFlight nextSn
      | null seenTxs ->
          ShouldNotSnapshot NoTransactionsToSnapshot
      | otherwise ->
          ShouldSnapshot nextSn seenTxs
 where
  nextSn = confirmedSn + 1

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

-- | Emit a snapshot if we are the next snapshot leader. 'Outcome' modifying
-- signature so it can be chained with other 'update' functions.
emitSnapshot :: IsTx tx => Environment -> Outcome tx -> Outcome tx
emitSnapshot env outcome =
  case outcome of
    NewState (Open OpenState{parameters, coordinatedHeadState, chainState, headId, currentSlot}) ->
      case newSn env parameters coordinatedHeadState of
        ShouldSnapshot sn txs -> do
          let CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
          NewState
            ( Open
                OpenState
                  { parameters
                  , coordinatedHeadState =
                      coordinatedHeadState
                        { seenSnapshot =
                            RequestedSnapshot
                              { lastSeen = seenSnapshotNumber seenSnapshot
                              , requested = sn
                              }
                        }
                  , chainState
                  , headId
                  , currentSlot
                  }
            )
            `Combined` Effects [NetworkEffect (ReqSn sn (txId <$> txs))]
        _ -> outcome
    Combined l r -> Combined (emitSnapshot env l) (emitSnapshot env r)
    _ -> outcome
