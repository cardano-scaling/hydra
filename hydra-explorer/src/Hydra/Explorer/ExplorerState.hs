module Hydra.Explorer.ExplorerState where

import Hydra.Prelude

-- XXX: Depending on hydra-node will be problematic to support versions
import Hydra.HeadId (HeadId (..))

import Cardano.Api.UTxO qualified as UTxO
import Data.Aeson (Value (..))
import Hydra.Cardano.Api (TxIn, UTxO)
import Hydra.Chain.Direct.Tx (
  AbortObservation (..),
  CloseObservation (..),
  ClosedThreadOutput (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  InitObservation (..),
  OpenThreadOutput (..),
 )
import Hydra.ContestationPeriod (ContestationPeriod, fromChain)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party, partyFromChain)

data HeadMember = HeadMember
  { party :: Party
  , onChainId :: Observed OnChainId
  , commits :: [UTxO]
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
  deriving stock (Eq, Show, Generic)

instance ToJSON a => ToJSON (Observed a) where
  toJSON Unknown = Null
  toJSON (Seen a) = toJSON a

instance FromJSON a => FromJSON (Observed a) where
  parseJSON Null = pure Unknown
  parseJSON value = Seen <$> parseJSON value

instance Arbitrary a => Arbitrary (Observed a) where
  arbitrary = genericArbitrary

data HeadState = HeadState
  { headId :: HeadId
  , seedTxIn :: Observed TxIn
  , status :: HeadStatus
  , contestationPeriod :: Observed ContestationPeriod
  , members :: [HeadMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary HeadState where
  arbitrary = genericArbitrary

type ExplorerState = [HeadState]

aggregateInitObservation :: InitObservation -> ExplorerState -> ExplorerState
aggregateInitObservation InitObservation{headId, seedTxIn, contestationPeriod, parties, participants} explorerState =
  case findHeadState headId explorerState of
    Just _headState -> replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newHeadState]
 where
  newHeadState =
    HeadState
      { headId
      , seedTxIn = Seen seedTxIn
      , status = Initializing
      , contestationPeriod = Seen contestationPeriod
      , members =
          fmap
            ( \(party, onChainId) ->
                HeadMember
                  { party
                  , onChainId
                  , commits = []
                  }
            )
            (parties `zip` fmap Seen participants)
      }

aggregateAbortObservation :: AbortObservation -> ExplorerState -> ExplorerState
aggregateAbortObservation AbortObservation{headId} explorerState =
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
      , members = []
      }

aggregateCommitObservation :: CommitObservation -> ExplorerState -> ExplorerState
aggregateCommitObservation CommitObservation{headId, commitOutput, party} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = updateMember headState
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
 where
  updateMember headState@HeadState{members} =
    case find (\HeadMember{party = partyMember} -> partyMember == party) members of
      Nothing -> headState{members = newUnknownMember : members}
      Just headMember@HeadMember{commits = currentCommits} ->
        let (txIn, txOut) = commitOutput
            newPartyCommit = UTxO.singleton (txIn, txOut)
            newMember = headMember{commits = newPartyCommit : currentCommits}
            newMembers = replaceMember members newMember
         in headState{members = newMembers}

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
      , commits =
          [ let (txIn, txOut) = commitOutput
             in UTxO.singleton (txIn, txOut)
          ]
      }

  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Initializing
      , contestationPeriod = Unknown
      , members = [newUnknownMember]
      }

aggregateCollectComObservation :: CollectComObservation -> ExplorerState -> ExplorerState
aggregateCollectComObservation CollectComObservation{headId, threadOutput = OpenThreadOutput{openContestationPeriod, openParties}} explorerState =
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
      , contestationPeriod = Seen (fromChain openContestationPeriod)
      , members =
          concatMap
            ( fmap
                ( \partyMember ->
                    HeadMember
                      { party = partyMember
                      , onChainId = Unknown
                      , commits = []
                      }
                )
                . partyFromChain
            )
            openParties
      }

aggregateCloseObservation :: CloseObservation -> ExplorerState -> ExplorerState
aggregateCloseObservation CloseObservation{headId, threadOutput = ClosedThreadOutput{closedParties}} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Closed}
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Closed
      , contestationPeriod = Unknown
      , members =
          concatMap
            ( fmap
                ( \partyMember ->
                    HeadMember
                      { party = partyMember
                      , onChainId = Unknown
                      , commits = []
                      }
                )
                . partyFromChain
            )
            closedParties
      }

aggregateContestObservation :: ContestObservation -> ExplorerState -> ExplorerState
aggregateContestObservation ContestObservation{headId} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      -- REVIEW: should we do smth here?
      let newHeadState = headState
       in replaceHeadState newHeadState explorerState
    Nothing -> explorerState <> [newUnknownHeadState]
 where
  newUnknownHeadState =
    HeadState
      { headId
      , seedTxIn = Unknown
      , status = Closed
      , contestationPeriod = Unknown
      , members = []
      }

aggregateFanoutObservation :: FanoutObservation -> ExplorerState -> ExplorerState
aggregateFanoutObservation FanoutObservation{headId} explorerState =
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
      , members = []
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
  foldl' aggregateObservation currentState observations
 where
  aggregateObservation explorerState =
    \case
      NoHeadTx -> explorerState
      Init obs -> aggregateInitObservation obs explorerState
      Abort obs -> aggregateAbortObservation obs explorerState
      Commit obs -> aggregateCommitObservation obs explorerState
      CollectCom obs -> aggregateCollectComObservation obs explorerState
      Close obs -> aggregateCloseObservation obs explorerState
      Contest obs -> aggregateContestObservation obs explorerState
      Fanout obs -> aggregateFanoutObservation obs explorerState

findHeadState :: HeadId -> ExplorerState -> Maybe HeadState
findHeadState idToFind = find (\HeadState{headId} -> headId == idToFind)
