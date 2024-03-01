module Hydra.Explorer.ExplorerState where

import Hydra.Prelude

import Hydra.HeadId (HeadId (..), HeadSeed)

import Data.Aeson (Value (..))
import Hydra.Cardano.Api (BlockNo, SlotNo, TxIn, UTxO)
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Tx (
  headSeedToTxIn,
 )
import Hydra.ChainObserver (HeadObservationAt (..))
import Hydra.ContestationPeriod (ContestationPeriod, toNominalDiffTime)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party)
import Hydra.Snapshot (SnapshotNumber (..))

data HeadMember = HeadMember
  { party :: Party
  , onChainId :: Observed OnChainId
  , commits :: Observed UTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadMember where
  arbitrary = genericArbitrary

data HeadStatus
  = Initializing
  | Aborted
  | Open
  | Closed
  | Finalized
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadStatus where
  arbitrary = genericArbitrary

data Observed a = Unknown | Seen a
  deriving stock (Eq, Show, Generic, Functor)

instance ToJSON a => ToJSON (Observed a) where
  toJSON Unknown = Null
  toJSON (Seen a) = toJSON a

instance FromJSON a => FromJSON (Observed a) where
  parseJSON Null = pure Unknown
  parseJSON value = Seen <$> parseJSON value

instance Arbitrary a => Arbitrary (Observed a) where
  arbitrary = genericArbitrary

-- | Represents the external appearance of a head state.
--
-- The decision to observe certain attributes or not is designed to address situations
-- where the explorer observes a head transaction on the chain without its
-- previously expected observation, preventing the loss of information during the transition.
-- Additionally, this simplifies the API for clients, eliminating the need to match against
-- different states.
data HeadState = HeadState
  { headId :: HeadId
  , seedTxIn :: Observed TxIn
  , status :: HeadStatus
  , contestationPeriod :: Observed ContestationPeriod
  , members :: Observed [HeadMember]
  , contestations :: Observed Natural
  , snapshotNumber :: Observed Natural
  , contestationDeadline :: Observed UTCTime
  , lastUpdatedAtSlotNo :: SlotNo
  , lastUpdatedAtBlockNo :: BlockNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadState where
  arbitrary = genericArbitrary

data ExplorerState = ExplorerState
  { heads :: [HeadState]
  , slotNo :: SlotNo
  , blockNo :: BlockNo
  }

aggregateInitObservation :: HeadId -> SlotNo -> BlockNo -> HeadSeed -> HeadParameters -> [OnChainId] -> [HeadState] -> [HeadState]
aggregateInitObservation headId slotNo blockNo headSeed HeadParameters{parties, contestationPeriod} participants currentHeads =
  case findHeadState headId currentHeads of
    Just _headState -> replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newHeadState]
 where
  newHeadState =
    HeadState
      { headId
      , seedTxIn = maybe Unknown Seen (headSeedToTxIn headSeed)
      , status = Initializing
      , contestationPeriod = Seen contestationPeriod
      , members =
          Seen $
            fmap
              ( \(party, onChainId) ->
                  HeadMember
                    { party
                    , onChainId = Seen onChainId
                    , commits = Unknown
                    }
              )
              (parties `zip` participants)
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

aggregateAbortObservation :: HeadId -> SlotNo -> BlockNo -> [HeadState] -> [HeadState]
aggregateAbortObservation headId slotNo blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Aborted}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Aborted
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

aggregateCommitObservation :: HeadId -> SlotNo -> BlockNo -> Party -> UTxO -> [HeadState] -> [HeadState]
aggregateCommitObservation headId slotNo blockNo party committed currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = updateMember headState
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  updateMember headState@HeadState{members} =
    let members' = case members of
          Unknown -> []
          Seen ms -> ms
     in case find (\HeadMember{party = partyMember} -> partyMember == party) members' of
          Nothing -> headState{members = Seen $ newUnknownMember : members'}
          Just headMember@HeadMember{commits = currentCommits} ->
            let currentCommits' = case currentCommits of
                  Unknown -> mempty
                  Seen utxo -> utxo
                newMember = headMember{commits = Seen $ committed <> currentCommits'}
                newMembers = replaceMember members' newMember
             in headState{members = Seen newMembers}

  replaceMember members newMember@HeadMember{party = newHeadMember} =
    case members of
      [] -> [newMember]
      (headMember@HeadMember{party = currentHeadMember} : tailMembers) ->
        if newHeadMember == currentHeadMember
          then newMember : tailMembers
          else headMember : replaceMember tailMembers newMember

  newUnknownMember =
    HeadMember
      { party
      , onChainId = Unknown
      , commits = Seen committed
      }

  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Initializing
      , contestationPeriod = Unknown
      , members = Seen [newUnknownMember]
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

aggregateCollectComObservation :: HeadId -> SlotNo -> BlockNo -> [HeadState] -> [HeadState]
aggregateCollectComObservation headId slotNo blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Open
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen 0
      , contestationDeadline = Unknown
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

aggregateCloseObservation :: HeadId -> SlotNo -> BlockNo -> SnapshotNumber -> UTCTime -> [HeadState] -> [HeadState]
aggregateCloseObservation headId slotNo blockNo (UnsafeSnapshotNumber sn) contestationDeadline currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Closed, contestations = Seen 0, snapshotNumber = Seen sn}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Closed
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 0
      , snapshotNumber = Seen sn
      , contestationDeadline = Seen contestationDeadline
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

aggregateContestObservation :: HeadId -> SlotNo -> BlockNo -> SnapshotNumber -> [HeadState] -> [HeadState]
aggregateContestObservation headId slotNo blockNo (UnsafeSnapshotNumber sn) currentHeads =
  case findHeadState headId currentHeads of
    Just headState@HeadState{contestations, contestationPeriod, contestationDeadline} ->
      let newHeadState =
            headState
              { contestations = (+ 1) <$> contestations
              , snapshotNumber = Seen sn
              , contestationDeadline =
                  case (contestationPeriod, contestationDeadline) of
                    (Seen cp, Seen cd) -> Seen $ addUTCTime (toNominalDiffTime cp) cd
                    _ -> Unknown
              }
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Closed
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Seen 1
      , snapshotNumber = Seen sn
      , contestationDeadline = Unknown
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

aggregateFanoutObservation :: HeadId -> SlotNo -> BlockNo -> [HeadState] -> [HeadState]
aggregateFanoutObservation headId slotNo blockNo currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState = headState{status = Finalized}
       in replaceHeadState newHeadState currentHeads
    Nothing -> currentHeads <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Finalized
      , contestationPeriod = Unknown
      , members = Unknown
      , contestations = Unknown
      , snapshotNumber = Unknown
      , contestationDeadline = Unknown
      , lastUpdatedAtSlotNo = slotNo
      , lastUpdatedAtBlockNo = blockNo
      }

replaceHeadState :: HeadState -> [HeadState] -> [HeadState]
replaceHeadState newHeadState@HeadState{headId = newHeadStateId} currentHeads =
  case currentHeads of
    [] -> [newHeadState]
    (currentHeadState@HeadState{headId = currentHeadStateId} : tailStates) ->
      if newHeadStateId == currentHeadStateId
        then newHeadState : tailStates
        else currentHeadState : replaceHeadState newHeadState tailStates

aggregateHeadObservations :: [HeadObservationAt] -> ExplorerState -> ExplorerState
aggregateHeadObservations observations explorerState =
  foldl' aggregateOnChainTx explorerState observations
 where
  aggregateOnChainTx :: ExplorerState -> HeadObservationAt -> ExplorerState
  aggregateOnChainTx ExplorerState{heads} =
    \case
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnInitTx{headId, headSeed, headParameters, participants}} ->
        ExplorerState
          { heads = aggregateInitObservation headId slotNo blockNo headSeed headParameters participants heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnAbortTx{headId}} ->
        ExplorerState
          { heads = aggregateAbortObservation headId slotNo blockNo heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnCommitTx{headId, party, committed}} ->
        ExplorerState
          { heads = aggregateCommitObservation headId slotNo blockNo party committed heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnCollectComTx{headId}} ->
        ExplorerState
          { heads = aggregateCollectComObservation headId slotNo blockNo heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnCloseTx{headId, snapshotNumber, contestationDeadline}} ->
        ExplorerState
          { heads = aggregateCloseObservation headId slotNo blockNo snapshotNumber contestationDeadline heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnContestTx{headId, snapshotNumber}} ->
        ExplorerState
          { heads = aggregateContestObservation headId slotNo blockNo snapshotNumber heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Just OnFanoutTx{headId}} ->
        ExplorerState
          { heads = aggregateFanoutObservation headId slotNo blockNo heads
          , slotNo
          , blockNo
          }
      HeadObservationAt{slotNo, blockNo, onChainTx = Nothing} ->
        ExplorerState
          { heads
          , slotNo
          , blockNo
          }

findHeadState :: HeadId -> [HeadState] -> Maybe HeadState
findHeadState idToFind = find (\HeadState{headId} -> headId == idToFind)
