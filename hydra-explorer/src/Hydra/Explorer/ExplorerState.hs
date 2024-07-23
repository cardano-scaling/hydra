module Hydra.Explorer.ExplorerState where

import Hydra.Prelude

import Hydra.HeadId (HeadId (..), HeadSeed)

import Data.Aeson (Value (..))
import Hydra.Cardano.Api (BlockNo, ChainPoint (..), TxIn, UTxO)
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Tx (
  headSeedToTxIn,
 )
import Hydra.ChainObserver (ChainObservation (..))
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
  , point :: ChainPoint
  , blockNo :: BlockNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadState where
  arbitrary = genericArbitrary

-- | Represents the latest point in time observed on chain.
data TickState = TickState
  { point :: ChainPoint
  , blockNo :: BlockNo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary TickState where
  arbitrary = genericArbitrary

initialTickState :: TickState
initialTickState = TickState ChainPointAtGenesis 0

data ExplorerState = ExplorerState
  { heads :: [HeadState]
  , tick :: TickState
  }

instance Arbitrary ExplorerState where
  arbitrary = ExplorerState <$> arbitrary <*> arbitrary

aggregateInitObservation :: HeadId -> ChainPoint -> BlockNo -> HeadSeed -> HeadParameters -> [OnChainId] -> [HeadState] -> [HeadState]
aggregateInitObservation headId point blockNo headSeed HeadParameters{parties, contestationPeriod} participants currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

aggregateAbortObservation :: HeadId -> ChainPoint -> BlockNo -> [HeadState] -> [HeadState]
aggregateAbortObservation headId point blockNo currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

aggregateCommitObservation :: HeadId -> ChainPoint -> BlockNo -> Party -> UTxO -> [HeadState] -> [HeadState]
aggregateCommitObservation headId point blockNo party committed currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

aggregateCollectComObservation :: HeadId -> ChainPoint -> BlockNo -> [HeadState] -> [HeadState]
aggregateCollectComObservation headId point blockNo currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

aggregateDecrementObservation :: HeadId -> ChainPoint -> BlockNo -> [HeadState] -> [HeadState]
aggregateDecrementObservation headId point blockNo currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

aggregateCloseObservation :: HeadId -> ChainPoint -> BlockNo -> SnapshotNumber -> UTCTime -> [HeadState] -> [HeadState]
aggregateCloseObservation headId point blockNo (UnsafeSnapshotNumber sn) contestationDeadline currentHeads =
  case findHeadState headId currentHeads of
    Just headState ->
      let newHeadState =
            headState
              { status = Closed
              , contestations = Seen 0
              , snapshotNumber = Seen sn
              , contestationDeadline = Seen contestationDeadline
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
      , contestations = Seen 0
      , snapshotNumber = Seen sn
      , contestationDeadline = Seen contestationDeadline
      , point = point
      , blockNo = blockNo
      }

aggregateContestObservation :: HeadId -> ChainPoint -> BlockNo -> SnapshotNumber -> [HeadState] -> [HeadState]
aggregateContestObservation headId point blockNo (UnsafeSnapshotNumber sn) currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

aggregateFanoutObservation :: HeadId -> ChainPoint -> BlockNo -> [HeadState] -> [HeadState]
aggregateFanoutObservation headId point blockNo currentHeads =
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
      , point = point
      , blockNo = blockNo
      }

replaceHeadState :: HeadState -> [HeadState] -> [HeadState]
replaceHeadState newHeadState@HeadState{headId = newHeadStateId} currentHeads =
  case currentHeads of
    [] -> [newHeadState]
    (currentHeadState@HeadState{headId = currentHeadStateId} : tailStates) ->
      if newHeadStateId == currentHeadStateId
        then newHeadState : tailStates
        else currentHeadState : replaceHeadState newHeadState tailStates

aggregateHeadObservations :: [ChainObservation] -> ExplorerState -> ExplorerState
aggregateHeadObservations observations explorerState =
  foldl' aggregateOnChainTx explorerState observations
 where
  aggregateOnChainTx :: ExplorerState -> ChainObservation -> ExplorerState
  aggregateOnChainTx ExplorerState{heads} =
    \case
      HeadObservation{point, blockNo, onChainTx = OnInitTx{headId, headSeed, headParameters, participants}} ->
        ExplorerState
          { heads = aggregateInitObservation headId point blockNo headSeed headParameters participants heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnAbortTx{headId}} ->
        ExplorerState
          { heads = aggregateAbortObservation headId point blockNo heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnCommitTx{headId, party, committed}} ->
        ExplorerState
          { heads = aggregateCommitObservation headId point blockNo party committed heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnCollectComTx{headId}} ->
        ExplorerState
          { heads = aggregateCollectComObservation headId point blockNo heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnDecrementTx{headId}} ->
        ExplorerState
          { heads = aggregateDecrementObservation headId point blockNo heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnCloseTx{headId, snapshotNumber, contestationDeadline}} ->
        ExplorerState
          { heads = aggregateCloseObservation headId point blockNo snapshotNumber contestationDeadline heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnContestTx{headId, snapshotNumber}} ->
        ExplorerState
          { heads = aggregateContestObservation headId point blockNo snapshotNumber heads
          , tick = TickState point blockNo
          }
      HeadObservation{point, blockNo, onChainTx = OnFanoutTx{headId}} ->
        ExplorerState
          { heads = aggregateFanoutObservation headId point blockNo heads
          , tick = TickState point blockNo
          }
      Tick{point, blockNo} ->
        ExplorerState
          { heads
          , tick = TickState point blockNo
          }

findHeadState :: HeadId -> [HeadState] -> Maybe HeadState
findHeadState idToFind = find (\HeadState{headId} -> headId == idToFind)
