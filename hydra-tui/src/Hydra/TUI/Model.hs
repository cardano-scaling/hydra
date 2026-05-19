{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Model where

import Hydra.Prelude hiding (Down, State)

import Data.Time.LocalTime (TimeZone)

import Hydra.Cardano.Api hiding (Active)

import Brick.Forms (Form)
import Brick.Widgets.List qualified as BrickList
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Hydra.Chain.Direct.State ()
import Hydra.Client (HydraEvent (..))
import Hydra.HeadLogic.State (CoordinatedHeadState (CoordinatedHeadState))
import Hydra.HeadLogic.State qualified as State
import Hydra.Network (Host (..))
import Hydra.Node.State (Deposit (..), NodeState (..))
import Hydra.TUI.Config (Theme (..))
import Hydra.TUI.Logging.Types (EventHistoryFilter, LogMessage, LogState)
import Hydra.Tx (HeadId, Party (..), Snapshot (..))
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.HeadParameters as HeadParameters
import Hydra.Tx.Snapshot qualified as Snapshot
import Lens.Micro ((^.), (^?))
import Lens.Micro.TH (makeLensesFor)

-- | TUI-local event wrapper so we can inject async results alongside Hydra node events.
data TUIEvent tx
  = NodeEvent (HydraEvent tx)
  | UTxOQueryResult (Map TxIn (TxOut CtxUTxO))
  | L1UTxORefresh (Map TxIn (TxOut CtxUTxO))
  | TxBuildError Text

data RootState = RootState
  { nodeHost :: Host
  , now :: UTCTime
  , timeZone :: TimeZone
  , connectedState :: ConnectedState
  , logState :: LogState
  , activeTab :: ActiveTab
  , eventDetailRaw :: Bool
  , eventHistoryList :: BrickList.List Name LogMessage
  , pendingAction :: Maybe Text
  , l1UTxO :: Maybe (Map TxIn (TxOut CtxUTxO))
  , previousTab :: ActiveTab
  , theme :: Theme
  , recoveryForm :: Maybe (TxIdRadioFieldForm (HydraEvent Tx) Name)
  , eventHistoryFilter :: EventHistoryFilter
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

-- | Synchronization status of the hydra node to the cardano chain.
data ChainSyncedStatus = InSync | CatchingUp

data PeerStatus = PeerIsConnected | PeerIsDisconnected | PeerIsUnknown

data Connection = Connection
  { me :: IdentifiedState
  , peers :: [(Host, PeerStatus)]
  , networkState :: Maybe NetworkState
  , chainSyncedStatus :: ChainSyncedStatus
  , headState :: HeadState
  }

type UTxOCheckboxForm e n = Form (Map TxIn (TxOut CtxUTxO, Bool)) e n

type UTxORadioFieldForm e n = Form (TxIn, TxOut CtxUTxO) e n

type TxIdRadioFieldForm e n = Form TxId e n

type ConfirmingRadioFieldForm e n = Form Bool e n

data OpenScreen
  = OpenHome
  | LoadingUTxOForIncrement
  | SelectingUTxO {selectingUTxOForm :: UTxORadioFieldForm (HydraEvent Tx) Name}
  | SelectingUTxOToDecommit {selectingUTxOToDecommitForm :: UTxORadioFieldForm (HydraEvent Tx) Name}
  | SelectingUTxOToIncrement {selectingUTxOToIncrementForm :: UTxORadioFieldForm (HydraEvent Tx) Name}
  | SelectingDepositIdToRecover {selectingDepositIdToRecoverForm :: TxIdRadioFieldForm (HydraEvent Tx) Name}
  | EnteringAmount {utxoSelected :: (TxIn, TxOut CtxUTxO), enteringAmountForm :: Form Double (HydraEvent Tx) Name}
  | SelectingRecipient
      { utxoSelected :: (TxIn, TxOut CtxUTxO)
      , amountEntered :: Double
      , selectingRecipientForm :: Form SelectAddressItem (HydraEvent Tx) Name
      }
  | EnteringRecipientAddress
      { utxoSelected :: (TxIn, TxOut CtxUTxO)
      , amountEntered :: Double
      , enteringRecipientAddressForm :: Form AddressInEra (HydraEvent Tx) Name
      }
  | ConfirmingClose {confirmingCloseForm :: ConfirmingRadioFieldForm (HydraEvent Tx) Name}

data SelectAddressItem
  = ManualEntry
  | SelectAddress AddressInEra
  deriving stock (Eq, Show)

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
  deriving stock (Show)

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
  = Open {openState :: OpenScreen}
  | Closed {closedState :: ClosedState}
  | FanoutPossible
  | Final

type Name = Text

data ActiveTab = MainTab | FundsTab | EventHistoryTab | ModalTab
  deriving stock (Eq)

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
  , ("timeZone", "timeZoneL")
  , ("logState", "logStateL")
  , ("activeTab", "activeTabL")
  , ("eventDetailRaw", "eventDetailRawL")
  , ("eventHistoryList", "eventHistoryListL")
  , ("pendingAction", "pendingActionL")
  , ("l1UTxO", "l1UTxOL")
  , ("previousTab", "previousTabL")
  , ("theme", "themeL")
  , ("recoveryForm", "recoveryFormL")
  , ("eventHistoryFilter", "eventHistoryFilterL")
  ]
  ''RootState

makeLensesFor
  [("connection", "connectionL")]
  ''ConnectedState

makeLensesFor
  [ ("me", "meL")
  , ("peers", "peersL")
  , ("networkState", "networkStateL")
  , ("chainSyncedStatus", "chainSyncedStatusL")
  , ("headState", "headStateL")
  ]
  ''Connection

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

eventHistoryListName :: Name
eventHistoryListName = "event-history-list"

mainUTxOViewportName :: Name
mainUTxOViewportName = "main-utxo"

fundsL2ViewportName :: Name
fundsL2ViewportName = "funds-l2"

fundsL1ViewportName :: Name
fundsL1ViewportName = "funds-l1"

emptyEventHistoryList :: BrickList.List Name LogMessage
emptyEventHistoryList = BrickList.list eventHistoryListName Vec.empty 1

emptyConnection :: Connection
emptyConnection =
  Connection
    { me = Unidentified
    , peers = []
    , networkState = Nothing
    , chainSyncedStatus = CatchingUp
    , headState = Idle
    }

newActiveLink :: [Party] -> HeadId -> ActiveLink
newActiveLink parties headId =
  ActiveLink
    { parties
    , activeHeadState = Open{openState = OpenHome}
    , utxo = mempty
    , pendingUTxOToDecommit = mempty
    , pendingIncrements = mempty
    , headId
    }

isModalOpen :: RootState -> Bool
isModalOpen s =
  s ^. activeTabL == ModalTab
    || case s
      ^? connectedStateL
        . connectionL
        . headStateL
        . activeLinkL
        . activeHeadStateL
        . openStateL of
      Nothing -> False
      Just OpenHome -> False
      Just LoadingUTxOForIncrement -> False -- handled by ModalTab check above
      Just _ -> True

recoverHeadState :: UTCTime -> HeadState -> NodeState Tx -> HeadState
recoverHeadState now current nodeState =
  case nodeState.headState of
    State.Idle State.IdleState{} -> current
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
    Map.toList nodeState.pendingDeposits
      <&> ( \(txId', Deposit{deposited, deadline}) ->
              PendingIncrement
                { utxoToCommit = deposited
                , deposit = txId'
                , depositDeadline = deadline
                , status = PendingDeposit
                }
          )
