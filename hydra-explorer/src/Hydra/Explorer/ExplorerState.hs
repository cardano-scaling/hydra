module Hydra.Explorer.ExplorerState where

import Hydra.Prelude

-- XXX: Depending on hydra-node will be problematic to support versions
import Hydra.HeadId (HeadId (..))

import Hydra.Cardano.Api (TxIn, TxOut)
import Hydra.Cardano.Api.Prelude (CtxUTxO)
import Hydra.Chain.Direct.Tx (
  AbortObservation (..),
  CloseObservation (..),
  CollectComObservation (..),
  CommitObservation (..),
  ContestObservation (..),
  FanoutObservation (..),
  HeadObservation (..),
  InitObservation (..),
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party)

data PartyCommit = PartyCommit
  { txIn :: TxIn
  , txOut :: TxOut CtxUTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary PartyCommit where
  arbitrary = genericArbitrary

data HeadMember = HeadMember
  { party :: Party
  , onChainId :: OnChainId
  , commits :: [PartyCommit]
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

data HeadState = HeadState
  { headId :: HeadId
  , seedTxIn :: TxIn
  , status :: HeadStatus
  , contestationPeriod :: ContestationPeriod
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
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Just _headState -> replaceHeadState newHeadState explorerState
    Nothing -> newHeadState : explorerState
 where
  newHeadState =
    HeadState
      { headId
      , seedTxIn
      , status = Initializing
      , contestationPeriod
      , members =
          fmap
            ( \(party, onChainId) ->
                HeadMember
                  { party
                  , onChainId
                  , commits = []
                  }
            )
            (parties `zip` participants)
      }

aggregateAbortObservation :: AbortObservation -> ExplorerState -> ExplorerState
aggregateAbortObservation AbortObservation{headId} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Aborted}
       in replaceHeadState newHeadState explorerState
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Nothing -> explorerState

aggregateCommitObservation :: CommitObservation -> ExplorerState -> ExplorerState
aggregateCommitObservation CommitObservation{headId, commitOutput, party} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = updateMember headState
       in replaceHeadState newHeadState explorerState
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Nothing -> explorerState
 where
  updateMember headState@HeadState{members} =
    case find (\HeadMember{party = partyMember} -> partyMember == party) members of
      -- REVIEW: this should never happen; how should we deal with this scenario?
      Nothing -> headState
      Just headMember@HeadMember{commits = currentCommits} ->
        let (txIn, txOut) = commitOutput
            newPartyCommit = PartyCommit{txIn, txOut}
            newMember = headMember{commits = newPartyCommit : currentCommits}
            newMembers = replaceMember members newMember
         in headState{members = newMembers}

  replaceMember members newMember@HeadMember{party = newHeadMember} =
    case members of
      -- REVIEW: this should never happen; how should we deal with this scenario?
      [] -> [newMember]
      (headMember@HeadMember{party = currentHeadMember} : tailMembers) ->
        if newHeadMember == currentHeadMember
          then newMember : tailMembers
          else headMember : replaceMember tailMembers newMember

aggregateCollectComObservation :: CollectComObservation -> ExplorerState -> ExplorerState
aggregateCollectComObservation CollectComObservation{headId} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Open}
       in replaceHeadState newHeadState explorerState
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Nothing -> explorerState

aggregateCloseObservation :: CloseObservation -> ExplorerState -> ExplorerState
aggregateCloseObservation CloseObservation{headId} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Closed}
       in replaceHeadState newHeadState explorerState
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Nothing -> explorerState

aggregateContestObservation :: ContestObservation -> ExplorerState -> ExplorerState
aggregateContestObservation ContestObservation{headId} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      -- REVIEW: should we do smth here?
      let newHeadState = headState
       in replaceHeadState newHeadState explorerState
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Nothing -> explorerState

aggregateFanoutObservation :: FanoutObservation -> ExplorerState -> ExplorerState
aggregateFanoutObservation FanoutObservation{headId} explorerState =
  case findHeadState headId explorerState of
    Just headState ->
      let newHeadState = headState{status = Finalized}
       in replaceHeadState newHeadState explorerState
    -- REVIEW: this should never happen; how should we deal with this scenario?
    Nothing -> explorerState

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
