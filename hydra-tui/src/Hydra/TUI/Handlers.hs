{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Handlers where

import Hydra.Prelude hiding (Down)

import Brick
import Brick.BChan (BChan, writeBChan)
import Brick.Forms (Form (formState), editField, editShowableFieldWithValidate, handleFormEvent, newForm)
import Brick.Widgets.List qualified as BrickList
import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent (forkIO)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Graphics.Vty (
  Event (EvKey),
  Key (..),
  Modifier (MCtrl),
 )
import Graphics.Vty qualified as Vty
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (NetworkInfo (..), TimedServerOutput (..))
import Hydra.API.ServerOutput qualified as API
import Hydra.Cardano.Api hiding (Active)
import Hydra.Cardano.Api.Prelude ()
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (AllPossibleAPIMessages (..), Client (..), HydraEvent (..))
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Network (Host, readHost)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State qualified as NodeState
import Hydra.TUI.Config (TuiConfig (..), toggleTheme, writeConfig)
import Hydra.TUI.Forms
import Hydra.TUI.Logging.Types (EventHistoryFilter (..), LogMessage (..), Severity (..), logMessagesL)
import Hydra.TUI.Model
import Hydra.TUI.RenderMessage (renderMessage, toLogMessage)
import Hydra.TUI.Style (own)
import Hydra.Tx (IsTx (..), Snapshot (..))
import Lens.Micro ((^.), (^?), _Just)
import Lens.Micro.Mtl (use, (%=), (.=))

handleEvent ::
  CardanoClient ->
  Client Tx IO ->
  BChan (TUIEvent Tx) ->
  BrickEvent Name (TUIEvent Tx) ->
  EventM Name RootState ()
handleEvent cardanoClient client chan = \case
  AppEvent (NodeEvent e) -> do
    handleTick e
    now <- use nowL
    zoom connectedStateL $ do
      handleHydraEventsConnectedState e
      zoom connectionL $ handleHydraEventsConnection now e
    beforeLen <- length <$> use (logStateL . logMessagesL)
    zoom (logStateL . logMessagesL) $
      handleHydraEventsLog now e
    afterLen <- length <$> use (logStateL . logMessagesL)
    when (afterLen > beforeLen) $
      pendingActionL .= Nothing
    syncEventHistoryList
    case e of
      Update (ApiTimedServerOutput TimedServerOutput{output = API.CommitRecorded{}}) ->
        triggerL1Query cardanoClient client chan
      Update (ApiTimedServerOutput TimedServerOutput{output = API.DecommitFinalized{}}) ->
        triggerL1Query cardanoClient client chan
      _ -> pure ()
  AppEvent (L1UTxORefresh utxo) -> do
    l1UTxOL .= Just utxo
    pendingActionL .= Just "Update complete"
  AppEvent (TxBuildError msg) -> pendingActionL .= Just msg
  AppEvent (UTxOQueryResult utxo) -> do
    pendingActionL .= Nothing
    connState <- use connectedStateL
    case connState of
      Connected c ->
        case c ^. headStateL of
          Active (ActiveLink{activeHeadState = Open{openState = LoadingUTxOForIncrement}}) ->
            case utxoRadioField utxo of
              Just form ->
                zoom (connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL) $
                  put $
                    SelectingUTxOToIncrement form
              Nothing -> do
                -- Empty or failed query — close modal, return to previous tab, show message
                zoom (connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL) $
                  put OpenHome
                prev <- use previousTabL
                activeTabL .= prev
                pendingActionL .= Just "No L1 funds available to increment."
          _ -> pure ()
      _ -> pure ()
  MouseDown name Vty.BScrollUp _ _ ->
    vScrollBy (viewportScroll name) (-3)
  MouseDown name Vty.BScrollDown _ _ ->
    vScrollBy (viewportScroll name) 3
  MouseDown{} -> pure ()
  MouseUp{} -> pure ()
  VtyEvent e -> do
    pendingActionL .= Nothing
    modalOpen <- gets isModalOpen
    case e of
      EvKey (KChar 'c') [MCtrl] -> halt
      EvKey (KChar 'd') [MCtrl] -> halt
      EvKey (KFun 3) [] -> do
        newTheme <- toggleTheme <$> use themeL
        themeL .= newTheme
        liftIO $ writeConfig TuiConfig{theme = newTheme}
      EvKey (KChar 'q') []
        | not modalOpen -> halt
      EvKey (KChar 'Q') []
        | not modalOpen -> halt
      EvKey (KChar '1') []
        | not modalOpen -> activeTabL .= MainTab
      EvKey (KChar '2') []
        | not modalOpen -> do
            activeTabL .= FundsTab
            triggerL1IfNeeded cardanoClient client chan
      EvKey (KChar '3') []
        | not modalOpen -> do
            activeTabL .= EventHistoryTab
            eventHistoryListL %= BrickList.listMoveTo 0
      EvKey (KChar 'c') [] | not modalOpen -> do
        tab <- use activeTabL
        zoom (connectedStateL . connectionL . headStateL) $
          handleVtyEventsHeadState cardanoClient client chan e
        newOpenScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
        case newOpenScreen of
          Just (ConfirmingClose _) -> do
            previousTabL .= tab
            activeTabL .= ModalTab
          _ -> pure ()
      EvKey (KChar 'e') [] | not modalOpen -> do
        tab <- use activeTabL
        case tab of
          EventHistoryTab -> do
            eventHistoryFilterL %= toggleEventHistoryFilter
            eventHistoryListL %= BrickList.listMoveTo 0
            syncEventHistoryList
          _ -> pure ()
      EvKey (KChar 'd') [] | not modalOpen -> do
        tab <- use activeTabL
        case tab of
          EventHistoryTab -> eventDetailRawL %= not
          _ -> do
            setPendingAction e
            zoom (connectedStateL . connectionL . headStateL) $
              handleVtyEventsHeadState cardanoClient client chan e
            newOpenScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
            case newOpenScreen of
              Just (SelectingUTxOToDecommit _) -> do
                previousTabL .= tab
                activeTabL .= ModalTab
              Just OpenHome ->
                pendingActionL .= Just "No L2 UTxO available to decommit."
              _ -> pure ()
      EvKey (KChar 'u') [] | not modalOpen -> do
        tab <- use activeTabL
        case tab of
          FundsTab -> do
            l1UTxOL .= Nothing
            pendingActionL .= Just "Updating L1 wallet…"
            triggerL1Query cardanoClient client chan
          _ -> pure ()
      EvKey (KChar 'f') [] | not modalOpen -> do
        setPendingAction e
        zoom (connectedStateL . connectionL . headStateL) $
          handleVtyEventsHeadState cardanoClient client chan e
      EvKey (KChar 'r') [] | not modalOpen -> do
        tab <- use activeTabL
        mActiveHeadState <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL)
        case mActiveHeadState of
          Just (Open _) -> do
            zoom (connectedStateL . connectionL . headStateL) $
              handleVtyEventsHeadState cardanoClient client chan e
            newOpenScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
            case newOpenScreen of
              Just (SelectingDepositIdToRecover _) -> do
                previousTabL .= tab
                activeTabL .= ModalTab
              _ -> pure ()
          Just _ -> do
            mPending <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . pendingIncrementsL)
            case mPending of
              Just pis@(_ : _) -> do
                let pendingDepositIds = (\PendingIncrement{deposit, utxoToCommit} -> (deposit, utxoToCommit)) <$> pis
                case depositIdRadioField pendingDepositIds of
                  Just form -> do
                    recoveryFormL .= Just form
                    previousTabL .= tab
                    activeTabL .= ModalTab
                  Nothing -> pure ()
              _ -> pure ()
          Nothing -> pure ()
      EvKey KRight []
        | not modalOpen -> do
            activeTabL %= cycleTab
            newTab <- use activeTabL
            when (newTab == FundsTab) $ triggerL1IfNeeded cardanoClient client chan
            when (newTab == EventHistoryTab) $ eventHistoryListL %= BrickList.listMoveTo 0
      EvKey KLeft []
        | not modalOpen -> do
            activeTabL %= prevTab
            newTab <- use activeTabL
            when (newTab == FundsTab) $ triggerL1IfNeeded cardanoClient client chan
            when (newTab == EventHistoryTab) $ eventHistoryListL %= BrickList.listMoveTo 0
      _ -> do
        tab <- use activeTabL
        case tab of
          ModalTab -> do
            let closeModal = do
                  prev <- use previousTabL
                  activeTabL .= prev
                  recoveryFormL .= Nothing
                  zoom (connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL) $
                    put OpenHome
            currentRecoveryForm <- use recoveryFormL
            case (currentRecoveryForm, e) of
              (Just _, EvKey KEsc []) -> closeModal
              (Just _, EvKey (KChar 'c') []) -> closeModal
              (Just form, EvKey KEnter []) -> do
                let (selectedTxId, _, _) = formState form
                liftIO $ recoverCommit client selectedTxId
                closeModal
              (Just _, _) -> zoom (recoveryFormL . _Just) $ handleFormEvent (VtyEvent e)
              (Nothing, EvKey KEsc []) -> closeModal
              (Nothing, EvKey (KChar 'c') []) -> closeModal
              (Nothing, _) -> do
                -- Set pending action for close confirmation
                openScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
                case (openScreen, e) of
                  (Just (ConfirmingClose form), EvKey KEnter []) ->
                    when (formState form) $ pendingActionL .= Just "Sending Close…"
                  _ -> pure ()
                zoom (connectedStateL . connectionL . headStateL) $
                  handleVtyEventsHeadState cardanoClient client chan e
                -- Auto-close modal if action completed and we returned to OpenHome
                newOpenScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
                case newOpenScreen of
                  Just OpenHome -> do
                    prev <- use previousTabL
                    activeTabL .= prev
                  _ -> pure ()
          _ -> do
            unless modalOpen $ setPendingAction e
            zoom (connectedStateL . connectionL . headStateL) $
              handleVtyEventsHeadState cardanoClient client chan e
            -- Switch to ModalTab when a modal flow starts
            newOpenScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
            case newOpenScreen of
              Just LoadingUTxOForIncrement -> do
                previousTabL .= tab
                activeTabL .= ModalTab
              Just (SelectingUTxO _) -> do
                previousTabL .= tab
                activeTabL .= ModalTab
              _ -> pure ()
            unless modalOpen $
              handleVtyEventsScrollable e

-- * AppEvent handlers

handleTick :: HydraEvent Tx -> EventM Name RootState ()
handleTick = \case
  Tick now -> nowL .= now
  _ -> pure ()

handleHydraEventsConnectedState :: HydraEvent Tx -> EventM Name ConnectedState ()
handleHydraEventsConnectedState = \case
  ClientConnected -> put $ Connected emptyConnection
  ClientDisconnected -> put Disconnected
  _ -> pure ()

handleHydraEventsConnection :: UTCTime -> HydraEvent Tx -> EventM Name Connection ()
handleHydraEventsConnection now = \case
  -- Note: Greetings is the last event seen after restart.
  Update
    ( ApiGreetings
        API.Greetings
          { me
          , env = Environment{configuredPeers}
          , networkInfo = NetworkInfo{networkConnected, peersInfo}
          , chainSyncedStatus
          }
      ) -> do
      meL .= Identified me
      networkStateL .= if networkConnected then Just NetworkConnected else Just NetworkDisconnected
      chainSyncedStatusL
        .= case chainSyncedStatus of
          NodeState.InSync -> InSync
          NodeState.CatchingUp -> CatchingUp

      if T.null configuredPeers
        then
          peersL .= mempty
        else do
          let peerStrs = map T.unpack (T.splitOn "," configuredPeers)
              peerAddrs = map (takeWhile (/= '=')) peerStrs
          case (traverse readHost peerAddrs :: Either String [Host]) of
            Left _ -> do
              peersL .= mempty
            Right parsedPeers -> do
              existing <- use peersL
              let existingMap = Map.fromList existing

                  statusFor p =
                    case Map.lookup p peersInfo of
                      Just True -> PeerIsConnected
                      Just False -> PeerIsDisconnected
                      Nothing -> Map.findWithDefault PeerIsUnknown p existingMap

              peersL .= [(p, statusFor p) | p <- parsedPeers]
  Update (ApiTimedServerOutput TimedServerOutput{output = API.PeerConnected p}) ->
    peersL %= updatePeerStatus p PeerIsConnected
  Update (ApiTimedServerOutput TimedServerOutput{output = API.PeerDisconnected p}) ->
    peersL %= updatePeerStatus p PeerIsDisconnected
  Update (ApiTimedServerOutput TimedServerOutput{output = API.NetworkConnected}) -> do
    networkStateL .= Just NetworkConnected
    peersL %= map (\(h, _) -> (h, PeerIsUnknown))
  Update (ApiTimedServerOutput TimedServerOutput{output = API.NetworkDisconnected}) -> do
    networkStateL .= Just NetworkDisconnected
    peersL %= map (\(h, _) -> (h, PeerIsUnknown))
  Update (ApiTimedServerOutput TimedServerOutput{output = API.NodeUnsynced{}}) -> do
    chainSyncedStatusL .= CatchingUp
  Update (ApiTimedServerOutput TimedServerOutput{output = API.NodeSynced{}}) -> do
    chainSyncedStatusL .= InSync
  e -> zoom headStateL $ handleHydraEventsHeadState now e
 where
  updatePeerStatus :: Host -> PeerStatus -> [(Host, PeerStatus)] -> [(Host, PeerStatus)]
  updatePeerStatus host status peers =
    (host, status) : filter ((/= host) . fst) peers

handleHydraEventsHeadState :: UTCTime -> HydraEvent Tx -> EventM Name HeadState ()
handleHydraEventsHeadState now e = do
  st <- get
  case e of
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsOpen{parties, headId}}) ->
      put $ Active (newActiveLink (toList parties) headId)
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.EventLogRotated{checkpoint}}) -> do
      modify $ \current -> recoverHeadState now current checkpoint
    _ -> pure ()
  zoom activeLinkL $ handleHydraEventsActiveLink e

handleHydraEventsActiveLink :: HydraEvent Tx -> EventM Name ActiveLink ()
handleHydraEventsActiveLink e = do
  case e of
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsOpen{}}) -> do
      activeHeadStateL .= Open OpenHome
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.SnapshotConfirmed{snapshot = Snapshot{utxo}}}) ->
      utxoL .= utxo
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsClosed{headId, snapshotNumber, contestationDeadline}}) -> do
      activeHeadStateL .= Closed{closedState = ClosedState{contestationDeadline}}
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.ReadyToFanout{}}) ->
      activeHeadStateL .= FanoutPossible
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsFinalized{}}) -> do
      activeHeadStateL .= Final
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.DecommitRequested{utxoToDecommit}}) -> do
      ActiveLink{utxo} <- get
      pendingUTxOToDecommitL .= utxoToDecommit
      utxoL .= UTxO.difference utxo utxoToDecommit
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.DecommitFinalized{}}) -> do
      ActiveLink{utxo, pendingUTxOToDecommit} <- get
      pendingUTxOToDecommitL .= mempty
      utxoL .= utxo
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitRecorded{utxoToCommit, pendingDeposit, deadline}}) -> do
      ActiveLink{pendingIncrements} <- get
      pendingIncrementsL .= pendingIncrements <> [PendingIncrement utxoToCommit pendingDeposit deadline PendingDeposit]
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitApproved{utxoToCommit = approvedUtxoToCommit}}) -> do
      ActiveLink{pendingIncrements} <- get
      pendingIncrementsL
        .= fmap
          ( \inc@PendingIncrement{utxoToCommit = pendingUtxoToCommit, deposit, depositDeadline} ->
              if hashUTxO pendingUtxoToCommit == hashUTxO approvedUtxoToCommit
                then PendingIncrement pendingUtxoToCommit deposit depositDeadline FinalizingDeposit
                else inc
          )
          pendingIncrements
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitFinalized{depositTxId}}) -> do
      ActiveLink{utxo, pendingIncrements} <- get
      let activePendingIncrements = filter (\PendingIncrement{deposit} -> deposit /= depositTxId) pendingIncrements
      let approvedIncrement = find (\PendingIncrement{deposit} -> deposit == depositTxId) pendingIncrements
      let activeUtxoToCommit = maybe mempty (\PendingIncrement{utxoToCommit} -> utxoToCommit) approvedIncrement
      pendingIncrementsL .= activePendingIncrements
      utxoL .= utxo <> activeUtxoToCommit
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitRecovered{recoveredTxId}}) -> do
      ActiveLink{pendingIncrements} <- get
      let activePendingIncrements = filter (\PendingIncrement{deposit} -> deposit /= recoveredTxId) pendingIncrements
      pendingIncrementsL .= activePendingIncrements
    _ -> pure ()

handleHydraEventsLog :: UTCTime -> HydraEvent Tx -> EventM Name [LogMessage] ()
handleHydraEventsLog now = \case
  Update msg -> id %= (toLogMessage (renderMessage now msg) :)
  _ -> pure ()

-- * VtyEvent handlers

handleVtyEventsHeadState :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> Vty.Event -> EventM Name HeadState ()
handleVtyEventsHeadState cardanoClient hydraClient chan e = do
  h <- use id
  case h of
    Idle -> case e of
      EvKey (KChar 'i') [] -> liftIO (sendInput hydraClient Init)
      _ -> pure ()
    _ -> pure ()
  zoom activeLinkL $ handleVtyEventsActiveLink cardanoClient hydraClient chan e

handleVtyEventsActiveLink :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> Vty.Event -> EventM Name ActiveLink ()
handleVtyEventsActiveLink cardanoClient hydraClient chan e = do
  utxo <- use utxoL
  pendingIncrements <- use pendingIncrementsL
  zoom activeHeadStateL $ handleVtyEventsActiveHeadState cardanoClient hydraClient chan utxo pendingIncrements e

handleVtyEventsActiveHeadState :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> UTxO -> [PendingIncrement] -> Vty.Event -> EventM Name ActiveHeadState ()
handleVtyEventsActiveHeadState cardanoClient hydraClient chan utxo pendingIncrements e = do
  zoom openStateL $ handleVtyEventsOpen cardanoClient hydraClient chan utxo pendingIncrements e
  s <- use id
  case s of
    FanoutPossible -> handleVtyEventsFanoutPossible hydraClient e
    Final -> handleVtyEventsFinal hydraClient e
    _ -> pure ()

handleVtyEventsOpen :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> UTxO -> [PendingIncrement] -> Vty.Event -> EventM Name OpenScreen ()
handleVtyEventsOpen cardanoClient hydraClient chan utxo pendingIncrements e =
  get >>= \case
    OpenHome -> do
      case e of
        EvKey (KChar 'n') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          case utxoRadioField utxo' of
            Just form -> put (SelectingUTxO form)
            Nothing -> liftIO $ writeBChan chan (TxBuildError "No UTxO available to send from.")
        EvKey (KChar 'd') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          case utxoRadioField utxo' of
            Just form -> put (SelectingUTxOToDecommit form)
            Nothing -> liftIO $ writeBChan chan (TxBuildError "No L2 UTxO available to decommit.")
        EvKey (KChar 'i') [] -> do
          put LoadingUTxOForIncrement
          let myAddr = mkMyAddress cardanoClient hydraClient
          liftIO $
            void $
              forkIO $
                handle (\(_ :: SomeException) -> writeBChan chan (UTxOQueryResult Map.empty)) $ do
                  utxo' <- queryUTxOByAddress cardanoClient [myAddr]
                  writeBChan chan (UTxOQueryResult (UTxO.toMap utxo'))
        EvKey (KChar 'r') [] -> do
          let pendingDepositIds = (\PendingIncrement{deposit, utxoToCommit} -> (deposit, utxoToCommit)) <$> pendingIncrements
          case depositIdRadioField pendingDepositIds of
            Just form -> put (SelectingDepositIdToRecover form)
            Nothing -> liftIO $ writeBChan chan (TxBuildError "No pending deposits to recover.")
        EvKey (KChar 'c') [] ->
          put $ ConfirmingClose confirmRadioField
        _ -> pure ()
    LoadingUTxOForIncrement ->
      case e of
        EvKey KEsc [] -> put OpenHome
        _ -> pure ()
    ConfirmingClose i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let selected = formState i
          if selected
            then do
              liftIO $ sendInput hydraClient Close
              put OpenHome
            else put OpenHome
        _ -> zoom confirmingCloseFormL $ handleFormEvent (VtyEvent e)
    SelectingUTxO i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let utxoSelected@(_, TxOut{txOutValue = v}) = formState i
          let Coin lovelaceLimit = selectLovelace v
          let limitAda = fromIntegral lovelaceLimit / 1_000_000 :: Double
          let enteringAmountForm =
                let field = editShowableFieldWithValidate id "amount (ADA)" (\n -> n > 0 && n <= limitAda)
                 in newForm [field] limitAda
          put EnteringAmount{utxoSelected, enteringAmountForm}
        _ -> zoom selectingUTxOFormL $ handleFormEvent (VtyEvent e)
    SelectingUTxOToDecommit i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let utxoSelected@(_, TxOut{txOutValue = v}) = formState i
          let recipient = mkVkAddress @Era (networkId cardanoClient) (getVerificationKey $ sk hydraClient)
          case mkSimpleTx utxoSelected (recipient, v) (sk hydraClient) of
            Left err -> liftIO $ writeBChan chan (TxBuildError $ "Could not build decommit transaction: " <> show err)
            Right tx -> do
              liftIO (sendInput hydraClient (Decommit tx))
              put OpenHome
        _ -> zoom selectingUTxOToDecommitFormL $ handleFormEvent (VtyEvent e)
    SelectingUTxOToIncrement i -> do
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let utxoSelected = formState i
          let commitUTxO = uncurry UTxO.singleton utxoSelected
          liftIO $ externalCommit hydraClient commitUTxO
          put OpenHome
        _ -> zoom selectingUTxOToIncrementFormL $ handleFormEvent (VtyEvent e)
    SelectingDepositIdToRecover i -> do
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let (selectedTxId, _, _) = formState i
          liftIO $ recoverCommit hydraClient selectedTxId
          put OpenHome
        _ -> zoom selectingDepositIdToRecoverFormL $ handleFormEvent (VtyEvent e)
    EnteringAmount utxoSelected i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let
            field =
              customRadioField '[' 'X' ']' id $
                [ (u, show u, show $ pretty u)
                | u <- nub $ toList addresses
                ]
            decorator a _ _ =
              if a == SelectAddress ownAddress
                then withAttr own
                else id
            addresses =
              ManualEntry
                :| (SelectAddress . txOutAddress <$> UTxO.txOutputs utxo)
          put
            SelectingRecipient
              { utxoSelected
              , amountEntered = formState i
              , selectingRecipientForm = newForm [field decorator] ManualEntry
              }
        _ -> zoom enteringAmountFormL $ handleFormEvent (VtyEvent e)
    SelectingRecipient utxoSelected amountEntered i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          case formState i of
            SelectAddress recipient -> do
              case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Coin $ round (amountEntered * 1_000_000)) (sk hydraClient) of
                Left err -> liftIO $ writeBChan chan (TxBuildError $ "Could not build transaction: " <> show err)
                Right tx -> do
                  liftIO (sendInput hydraClient (NewTx tx))
                  put OpenHome
            ManualEntry ->
              put $
                EnteringRecipientAddress
                  { utxoSelected
                  , amountEntered
                  , enteringRecipientAddressForm =
                      newForm
                        [ editField
                            id
                            "manual address entry"
                            (Just 1)
                            serialiseAddress
                            (nonEmpty >=> parseAddress . head)
                            (txt . fold)
                            id
                        ]
                        ownAddress
                  }
        _ -> zoom selectingRecipientFormL $ handleFormEvent (VtyEvent e)
    EnteringRecipientAddress utxoSelected amountEntered i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let recipient = formState i
          case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Coin $ round (amountEntered * 1_000_000)) (sk hydraClient) of
            Left err -> liftIO $ writeBChan chan (TxBuildError $ "Could not build transaction: " <> show err)
            Right tx -> do
              liftIO (sendInput hydraClient (NewTx tx))
              put OpenHome
        _ -> zoom enteringRecipientAddressFormL $ handleFormEvent (VtyEvent e)
 where
  ownAddress = mkVkAddress @Era (networkId cardanoClient) (getVerificationKey $ sk hydraClient)

  parseAddress =
    fmap ShelleyAddressInEra . deserialiseAddress (AsAddress AsShelleyAddr)

handleVtyEventsFanoutPossible :: Client Tx IO -> Vty.Event -> EventM Name s ()
handleVtyEventsFanoutPossible hydraClient e = do
  case e of
    EvKey (KChar 'f') [] ->
      liftIO (sendInput hydraClient Fanout)
    _ -> pure ()

handleVtyEventsFinal :: Client Tx IO -> Vty.Event -> EventM Name s ()
handleVtyEventsFinal hydraClient e = do
  case e of
    EvKey (KChar 'i') [] ->
      liftIO (sendInput hydraClient Init)
    _ -> pure ()

handleVtyEventsScrollable :: Vty.Event -> EventM Name RootState ()
handleVtyEventsScrollable e = do
  tab <- use activeTabL
  case tab of
    EventHistoryTab -> case e of
      EvKey KPageUp [] -> vScrollBy (viewportScroll "event-detail") (-10)
      EvKey KPageDown [] -> vScrollBy (viewportScroll "event-detail") 10
      Vty.EvMouseDown _ _ Vty.BScrollUp _ ->
        zoom eventHistoryListL $ BrickList.handleListEvent (EvKey KUp [])
      Vty.EvMouseDown _ _ Vty.BScrollDown _ ->
        zoom eventHistoryListL $ BrickList.handleListEvent (EvKey KDown [])
      _ -> zoom eventHistoryListL $ BrickList.handleListEvent e
    MainTab -> case e of
      EvKey KPageUp [] -> vScrollBy (viewportScroll mainUTxOViewportName) (-10)
      EvKey KPageDown [] -> vScrollBy (viewportScroll mainUTxOViewportName) 10
      _ -> pure ()
    FundsTab -> case e of
      EvKey KPageUp [] -> vScrollBy (viewportScroll fundsL2ViewportName) (-10)
      EvKey KPageDown [] -> vScrollBy (viewportScroll fundsL2ViewportName) 10
      _ -> pure ()
    _ -> pure ()

syncEventHistoryList :: EventM Name RootState ()
syncEventHistoryList = do
  msgs <- use (logStateL . logMessagesL)
  flt <- use eventHistoryFilterL
  let filteredMsgs = applyEventHistoryFilter flt msgs
  eventHistoryListL %= \l ->
    let oldLen = Vec.length (BrickList.listElements l)
        newVec = Vec.fromList filteredMsgs
        newLen = Vec.length newVec
        added = newLen - oldLen
        newList = BrickList.listReplace newVec (BrickList.listSelected l) l
     in if added > 0
          then BrickList.listMoveTo (maybe 0 (+ added) (BrickList.listSelected l)) newList
          else newList

applyEventHistoryFilter :: EventHistoryFilter -> [LogMessage] -> [LogMessage]
applyEventHistoryFilter = \case
  ShowAll -> id
  ErrorsOnly -> filter (\LogMessage{severity} -> severity == Error)

toggleEventHistoryFilter :: EventHistoryFilter -> EventHistoryFilter
toggleEventHistoryFilter = \case
  ShowAll -> ErrorsOnly
  ErrorsOnly -> ShowAll

myAvailableUTxO :: NetworkId -> VerificationKey PaymentKey -> UTxO -> Map TxIn (TxOut CtxUTxO)
myAvailableUTxO networkId vk (UTxO u) =
  let myAddress = mkVkAddress networkId vk
   in Map.filter (\TxOut{txOutAddress = addr} -> addr == myAddress) u

mkMyAddress :: CardanoClient -> Client Tx IO -> Address ShelleyAddr
mkMyAddress cardanoClient hydraClient =
  makeShelleyAddress
    (networkId cardanoClient)
    (PaymentCredentialByKey . verificationKeyHash $ getVerificationKey $ sk hydraClient)
    NoStakeAddress

setPendingAction :: Vty.Event -> EventM Name RootState ()
setPendingAction e = do
  connState <- use connectedStateL
  case connState of
    Disconnected -> pure ()
    Connected c -> case (c ^. headStateL, e) of
      (Idle, EvKey (KChar 'i') []) ->
        pendingActionL .= Just "Sending Init…"
      (Active (ActiveLink{activeHeadState = Final}), EvKey (KChar 'i') []) ->
        pendingActionL .= Just "Sending Init…"
      (Active (ActiveLink{activeHeadState = FanoutPossible}), EvKey (KChar 'f') []) ->
        pendingActionL .= Just "Sending Fanout…"
      (Active (ActiveLink{activeHeadState = Open{openState = OpenHome}}), EvKey (KChar 'i') []) ->
        pendingActionL .= Just "Querying Cardano node…"
      (Active (ActiveLink{activeHeadState = Open{openState}}), EvKey KEnter []) ->
        case openState of
          SelectingUTxOToDecommit _ ->
            pendingActionL .= Just "Sending decommit…"
          SelectingUTxOToIncrement _ ->
            pendingActionL .= Just "Sending increment…"
          SelectingDepositIdToRecover _ ->
            pendingActionL .= Just "Sending recovery…"
          EnteringRecipientAddress{} ->
            pendingActionL .= Just "Sending transaction…"
          SelectingRecipient{selectingRecipientForm = form} ->
            case formState form of
              SelectAddress _ -> pendingActionL .= Just "Sending transaction…"
              ManualEntry -> pure ()
          ConfirmingClose{confirmingCloseForm = form} ->
            when (formState form) $
              pendingActionL .= Just "Sending Close…"
          _ -> pure ()
      _ -> pure ()

triggerL1Query :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> EventM Name RootState ()
triggerL1Query cardanoClient client chan = do
  let myAddr = mkMyAddress cardanoClient client
  liftIO $
    void $
      forkIO $
        handle (\(_ :: SomeException) -> writeBChan chan (L1UTxORefresh Map.empty)) $ do
          utxo' <- queryUTxOByAddress cardanoClient [myAddr]
          writeBChan chan (L1UTxORefresh (UTxO.toMap utxo'))

triggerL1IfNeeded :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> EventM Name RootState ()
triggerL1IfNeeded cardanoClient client chan = do
  l1 <- use l1UTxOL
  when (isNothing l1) $ triggerL1Query cardanoClient client chan

cycleTab :: ActiveTab -> ActiveTab
cycleTab MainTab = FundsTab
cycleTab FundsTab = EventHistoryTab
cycleTab EventHistoryTab = MainTab
cycleTab ModalTab = MainTab

prevTab :: ActiveTab -> ActiveTab
prevTab MainTab = EventHistoryTab
prevTab FundsTab = MainTab
prevTab EventHistoryTab = FundsTab
prevTab ModalTab = MainTab
