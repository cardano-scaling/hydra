module Hydra.Explorer.ExplorerState where

import Hydra.Prelude

import Hydra.HeadId (HeadId (..), HeadSeed)

import Data.Aeson (Value (..))
import Hydra.Cardano.Api (Tx, TxIn, UTxO)
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.Chain.Direct.Tx (
  HeadObservation (..),
  headSeedToTxIn,
 )
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
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadState where
  arbitrary = genericArbitrary

type ExplorerState = [HeadState]

aggregateInitObservation :: HeadId -> HeadSeed -> HeadParameters -> [OnChainId] -> ExplorerState -> ExplorerState
aggregateInitObservation headId headSeed HeadParameters{parties, contestationPeriod} participants explorerState =
  case findHeadState headId explorerState of
    Just _headState -> replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newHeadState]
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
      }

aggregateAbortObservation :: HeadId -> ExplorerState -> ExplorerState
aggregateAbortObservation headId explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Aborted}
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
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
      }

aggregateCommitObservation :: HeadId -> Party -> UTxO -> ExplorerState -> ExplorerState
aggregateCommitObservation headId party committed explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = updateMember headState
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
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
      }

aggregateCollectComObservation :: HeadId -> ExplorerState -> ExplorerState
aggregateCollectComObservation headId explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
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
      }

aggregateCloseObservation :: HeadId -> SnapshotNumber -> UTCTime -> ExplorerState -> ExplorerState
aggregateCloseObservation headId (UnsafeSnapshotNumber sn) contestationDeadline explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Closed, contestations = Seen 0, snapshotNumber = Seen sn}
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
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
      }

aggregateContestObservation :: HeadId -> SnapshotNumber -> ExplorerState -> ExplorerState
aggregateContestObservation headId (UnsafeSnapshotNumber sn) explorerState =
  case findHeadState headId explorerState of
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
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
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
      }

aggregateFanoutObservation :: HeadId -> ExplorerState -> ExplorerState
aggregateFanoutObservation headId explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Finalized}
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
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
      }

replaceHeadState :: HeadState -> ExplorerState -> ExplorerState
replaceHeadState newHeadState@HeadState{headId = newHeadStateId} explorerState =
  case explorerState of
    [] -> [newHeadState]
    (currentHeadState@HeadState{headId = currentHeadStateId} : tailStates) ->
      if newHeadStateId == currentHeadStateId
        then newHeadState : tailStates
        else currentHeadState : replaceHeadState newHeadState tailStates

aggregateHeadObservations :: [HeadObservation] -> ExplorerState -> ExplorerState
aggregateHeadObservations observations currentState =
  foldl' aggregateOnChainTx currentState (mapMaybe convertObservation observations)
 where
  aggregateOnChainTx :: ExplorerState -> OnChainTx Tx -> ExplorerState
  aggregateOnChainTx explorerState =
    \case
      OnInitTx{headId, headSeed, headParameters, participants} -> aggregateInitObservation headId headSeed headParameters participants explorerState
      OnAbortTx{headId} -> aggregateAbortObservation headId explorerState
      OnCommitTx{headId, party, committed} -> aggregateCommitObservation headId party committed explorerState
      OnCollectComTx{headId} -> aggregateCollectComObservation headId explorerState
      OnCloseTx{headId, snapshotNumber, contestationDeadline} -> aggregateCloseObservation headId snapshotNumber contestationDeadline explorerState
      OnContestTx{headId, snapshotNumber} -> aggregateContestObservation headId snapshotNumber explorerState
      OnFanoutTx{headId} -> aggregateFanoutObservation headId explorerState

findHeadState :: HeadId -> ExplorerState -> Maybe HeadState
findHeadState idToFind = find (\HeadState{headId} -> headId == idToFind)
