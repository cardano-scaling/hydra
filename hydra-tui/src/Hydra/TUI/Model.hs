{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Model where

import Hydra.Prelude hiding (Down, State)

import Hydra.Cardano.Api hiding (Active)

import Brick.Forms (Form)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.Chain.Direct.State ()
import Hydra.Client (HydraEvent (..))
import Hydra.HeadLogic.State (CoordinatedHeadState (CoordinatedHeadState))
import Hydra.HeadLogic.State qualified as State
import Hydra.Network (Host (..))
import Hydra.Node.State (Deposit (..), NodeState (..))
import Hydra.TUI.Logging.Types (LogState)
import Hydra.Tx (HeadId, Party (..), Snapshot (..))
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.HeadParameters as HeadParameters
import Hydra.Tx.Snapshot qualified as Snapshot
import Lens.Micro ((^?))
import Lens.Micro.TH (makeLensesFor)

data RootState = RootState
  { nodeHost :: Host
  , now :: UTCTime
  , connectedState :: ConnectedState
  , logState :: LogState
  }

-- | Connection to the hydra node.
data ConnectedState
  = Disconnected
  | Connected {connection :: Connection}

data IdentifiedState
  = Unidentified
  | Identified Party

-- | Connectivity of the hydra node to the hydra network.
data NetworkState = NetworkConnected | NetworkDisconnected

data PeerStatus = PeerIsConnected | PeerIsDisconnected | PeerIsUnknown

data Connection = Connection
  { me :: IdentifiedState
  , peers :: [(Host, PeerStatus)]
  , networkState :: Maybe NetworkState
  , headState :: HeadState
  }

type UTxOCheckboxForm e n = Form (Map TxIn (TxOut CtxUTxO, Bool)) e n

type UTxORadioFieldForm e n = Form (TxIn, TxOut CtxUTxO) e n

type TxIdRadioFieldForm e n = Form (TxId, TxIn, TxOut CtxUTxO) e n

type ConfirmingRadioFieldForm e n = Form Bool e n

data InitializingState = InitializingState
  { remainingParties :: [Party]
  , initializingScreen :: InitializingScreen
  }

data InitializingScreen
  = InitializingHome
  | CommitMenu {commitMenu :: UTxOCheckboxForm (HydraEvent Tx) Name}
  | ConfirmingAbort {confirmingAbortForm :: ConfirmingRadioFieldForm (HydraEvent Tx) Name}

data OpenScreen
  = OpenHome
  | SelectingUTxO {selectingUTxOForm :: UTxORadioFieldForm (HydraEvent Tx) Name}
  | SelectingUTxOToDecommit {selectingUTxOToDecommitForm :: UTxORadioFieldForm (HydraEvent Tx) Name}
  | SelectingUTxOToIncrement {selectingUTxOToIncrementForm :: UTxORadioFieldForm (HydraEvent Tx) Name}
  | SelectingDepositIdToRecover {selectingDepositIdToRecoverForm :: TxIdRadioFieldForm (HydraEvent Tx) Name}
  | EnteringAmount {utxoSelected :: (TxIn, TxOut CtxUTxO), enteringAmountForm :: Form Integer (HydraEvent Tx) Name}
  | SelectingRecipient
      { utxoSelected :: (TxIn, TxOut CtxUTxO)
      , amountEntered :: Integer
      , selectingRecipientForm :: Form SelectAddressItem (HydraEvent Tx) Name
      }
  | EnteringRecipientAddress
      { utxoSelected :: (TxIn, TxOut CtxUTxO)
      , amountEntered :: Integer
      , enteringRecipientAddressForm :: Form AddressInEra (HydraEvent Tx) Name
      }
  | ConfirmingClose {confirmingCloseForm :: ConfirmingRadioFieldForm (HydraEvent Tx) Name}

data SelectAddressItem
  = ManualEntry
  | SelectAddress AddressInEra
  deriving (Eq, Show)

instance Pretty SelectAddressItem where
  pretty = \case
    ManualEntry -> "Manual entry"
    SelectAddress addr -> pretty $ serialiseAddress addr

newtype ClosedState = ClosedState {contestationDeadline :: UTCTime}

data HeadState
  = Idle
  | Active {activeLink :: ActiveLink}

data PendingIncrementStatus
  = PendingDeposit
  | FinalizingDeposit
  deriving (Show)

data PendingIncrement
  = PendingIncrement
  { utxoToCommit :: UTxO
  , deposit :: TxId
  , depositDeadline :: UTCTime
  , status :: PendingIncrementStatus
  }

data ActiveLink = ActiveLink
  { utxo :: UTxO
  , pendingUTxOToDecommit :: UTxO
  , pendingIncrements :: [PendingIncrement]
  , parties :: [Party]
  , headId :: HeadId
  , activeHeadState :: ActiveHeadState
  }

data ActiveHeadState
  = Initializing {initializingState :: InitializingState}
  | Open {openState :: OpenScreen}
  | Closed {closedState :: ClosedState}
  | FanoutPossible
  | Final

type Name = Text

makeLensesFor
  [ ("selectingUTxOForm", "selectingUTxOFormL")
  , ("selectingUTxOToDecommitForm", "selectingUTxOToDecommitFormL")
  , ("selectingUTxOToIncrementForm", "selectingUTxOToIncrementFormL")
  , ("selectingDepositIdToRecoverForm", "selectingDepositIdToRecoverFormL")
  , ("enteringAmountForm", "enteringAmountFormL")
  , ("selectingRecipientForm", "selectingRecipientFormL")
  , ("enteringRecipientAddressForm", "enteringRecipientAddressFormL")
  , ("confirmingCloseForm", "confirmingCloseFormL")
  ]
  ''OpenScreen

makeLensesFor
  [ ("initializingState", "initializingStateL")
  , ("openState", "openStateL")
  , ("closedState", "closedStateL")
  ]
  ''ActiveHeadState

makeLensesFor
  [ ("connectedState", "connectedStateL")
  , ("nodeHost", "nodeHostL")
  , ("now", "nowL")
  , ("logState", "logStateL")
  ]
  ''RootState

makeLensesFor
  [("connection", "connectionL")]
  ''ConnectedState

makeLensesFor
  [ ("commitMenu", "commitMenuL")
  , ("confirmingAbortForm", "confirmingAbortFormL")
  ]
  ''InitializingScreen

makeLensesFor
  [ ("transitionNote", "transitionNoteL")
  , ("me", "meL")
  , ("peers", "peersL")
  , ("networkState", "networkStateL")
  , ("headState", "headStateL")
  ]
  ''Connection

makeLensesFor
  [ ("remainingParties", "remainingPartiesL")
  , ("initializingScreen", "initializingScreenL")
  ]
  ''InitializingState

makeLensesFor
  [ ("activeLink", "activeLinkL")
  ]
  ''HeadState

makeLensesFor
  [ ("utxo", "utxoL")
  , ("pendingUTxOToDecommit", "pendingUTxOToDecommitL")
  , ("pendingIncrements", "pendingIncrementsL")
  , ("parties", "partiesL")
  , ("activeHeadState", "activeHeadStateL")
  , ("headId", "headIdL")
  ]
  ''ActiveLink

fullFeedbackViewportName :: Name
fullFeedbackViewportName = "full-feedback-view-port"

shortFeedbackViewportName :: Name
shortFeedbackViewportName = "short-feedback-view-port"

emptyConnection :: Connection
emptyConnection =
  Connection
    { me = Unidentified
    , peers = []
    , networkState = Nothing
    , headState = Idle
    }

newActiveLink :: [Party] -> HeadId -> ActiveLink
newActiveLink parties headId =
  ActiveLink
    { parties
    , activeHeadState =
        Initializing
          { initializingState =
              InitializingState
                { remainingParties = parties
                , initializingScreen = InitializingHome
                }
          }
    , utxo = mempty
    , pendingUTxOToDecommit = mempty
    , pendingIncrements = mempty
    , headId
    }

isModalOpen :: RootState -> Bool
isModalOpen s =
  case s
    ^? connectedStateL
      . connectionL
      . headStateL
      . activeLinkL
      . activeHeadStateL
      . openStateL of
    Nothing -> False
    Just OpenHome -> False
    Just _ -> True

recoverHeadState :: UTCTime -> HeadState -> NodeState Tx -> HeadState
recoverHeadState now current NodeState{headState, pendingDeposits} =
  case headState of
    State.Idle State.IdleState{} -> current
    State.Initial
      State.InitialState
        { parameters
        , committed
        , headId
        , pendingCommits
        } ->
        Active
          ActiveLink
            { utxo = fold committed
            , pendingUTxOToDecommit = mempty
            , pendingIncrements
            , parties = HeadParameters.parties parameters
            , headId
            , activeHeadState =
                Initializing
                  InitializingState
                    { remainingParties =
                        filter
                          (`Set.member` pendingCommits)
                          (HeadParameters.parties parameters)
                    , initializingScreen = InitializingHome
                    }
            }
    State.Open
      State.OpenState
        { parameters
        , headId
        , coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}
        } ->
        let Snapshot{utxo, utxoToDecommit} = Snapshot.getSnapshot confirmedSnapshot
         in Active
              ActiveLink
                { utxo
                , pendingUTxOToDecommit = fromMaybe mempty utxoToDecommit
                , pendingIncrements
                , parties = HeadParameters.parties parameters
                , headId
                , activeHeadState = Open OpenHome
                }
    State.Closed
      State.ClosedState
        { parameters
        , headId
        , confirmedSnapshot
        , readyToFanoutSent
        } ->
        let Snapshot{utxo, utxoToDecommit} = Snapshot.getSnapshot confirmedSnapshot
            contestationDeadline = addUTCTime (CP.toNominalDiffTime $ HeadParameters.contestationPeriod parameters) now
         in Active
              ActiveLink
                { utxo
                , pendingUTxOToDecommit = fromMaybe mempty utxoToDecommit
                , pendingIncrements
                , parties = HeadParameters.parties parameters
                , headId
                , activeHeadState =
                    if readyToFanoutSent
                      then FanoutPossible
                      else Closed{closedState = ClosedState{contestationDeadline}}
                }
 where
  pendingIncrements =
    Map.toList pendingDeposits
      <&> ( \(txId', Deposit{deposited, deadline}) ->
              PendingIncrement
                { utxoToCommit = deposited
                , deposit = txId'
                , depositDeadline = deadline
                , status = PendingDeposit
                }
          )
