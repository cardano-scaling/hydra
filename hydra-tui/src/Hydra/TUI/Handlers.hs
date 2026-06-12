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
import Hydra.Cardano.Api hiding (Active, getVerificationKey)
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
import Hydra.Tx.Crypto (getVerificationKey)
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
    refreshRecoveryForm
    case e of
      Update (ApiTimedServerOutput TimedServerOutput{output = API.CommitRecorded{}}) ->
        triggerL1Query cardanoClient client chan
      Update (ApiTimedServerOutput TimedServerOutput{output = API.CommitFinalized{}}) ->
        triggerL1Query cardanoClient client chan
      Update (ApiTimedServerOutput TimedServerOutput{output = API.CommitRecovered{}}) ->
        triggerL1Query cardanoClient client chan
      Update (ApiTimedServerOutput TimedServerOutput{output = API.DecommitFinalized{}}) ->
        triggerL1Query cardanoClient client chan
      Update (ApiTimedServerOutput TimedServerOutput{output = API.HeadIsFinalized{}}) ->
        triggerL1Query cardanoClient client chan
      ClientDisconnected -> do
        recoveryFormL .= Nothing
        pendingActionL .= Nothing
        l1UTxOL .= Nothing
        tab <- use activeTabL
        when (tab == ModalTab) leaveModal
      _ -> pure ()
  AppEvent (L1UTxORefresh utxo) -> do
    l1UTxOL .= Just utxo
    pendingActionL .= Just "Update complete"
  AppEvent (TxBuildError msg) -> pendingActionL .= Just msg
  AppEvent (UTxOQueryResult utxo) -> do
    pendingActionL .= Nothing
    openScreen <- useOpenScreen
    case openScreen of
      Just LoadingUTxOForIncrement -> case utxoRadioField utxo of
        Just form ->
          zoomOpenScreen $ put $ SelectingUTxOToIncrement form
        Nothing -> do
          -- Empty or failed query — close modal, return to previous tab, show message
          zoomOpenScreen $ put OpenHome
          leaveModal
          pendingActionL .= Just "No L1 funds available to increment."
      Nothing -> do
        -- Head is no longer Open (e.g. it closed mid-query). If we're still
        -- stuck on the loading modal, return the user to their previous tab.
        -- Without this, the result would be silently dropped and the modal
        -- would be stranded showing a stale loading panel.
        tab <- use activeTabL
        hasRecoveryForm <- isJust <$> use recoveryFormL
        when (tab == ModalTab && not hasRecoveryForm) leaveModal
      _ -> pure () -- User navigated away (Esc, started a different flow) — drop silently.
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
        zoom (connectedStateL . connectionL . headStateL) $
          handleVtyEventsHeadState cardanoClient client chan e
        newOpenScreen <- useOpenScreen
        case newOpenScreen of
          Just (ConfirmingClose _) -> enterModal
          _ -> pure ()
      EvKey (KChar 'e') []
        | not modalOpen -> do
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
            newOpenScreen <- useOpenScreen
            case newOpenScreen of
              Just (SelectingUTxOToDecommit _) -> enterModal
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
        -- Recovery is a single modal flow regardless of head state. The form
        -- is stored in 'recoveryFormL' (not the per-head 'openState') so the
        -- same code path works whether the head is Open, Closed, or Final.
        mPending <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . pendingIncrementsL)
        let noDeposits = liftIO $ writeBChan chan (TxBuildError "No pending deposits to recover.")
        case mPending of
          Nothing -> pure () -- Idle or Disconnected: no head, nothing to recover.
          Just [] -> noDeposits
          Just pis -> do
            let pendingDepositIds = (\PendingIncrement{deposit, utxoToCommit} -> (deposit, utxoToCommit)) <$> pis
            case depositIdRadioField pendingDepositIds of
              Just form -> do
                recoveryFormL .= Just form
                enterModal
              Nothing -> noDeposits
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
                  leaveModal
                  recoveryFormL .= Nothing
                  zoomOpenScreen $ put OpenHome
            currentRecoveryForm <- use recoveryFormL
            case (currentRecoveryForm, e) of
              (_, EvKey KEsc []) -> closeModal
              (_, EvKey (KChar 'c') []) -> closeModal
              (Just form, EvKey KEnter []) -> do
                let selectedTxId = formState form
                pendingActionL .= Just "Sending recovery…"
                liftIO $ recoverCommitAsync client chan selectedTxId
                closeModal
              (Just _, _) -> zoom (recoveryFormL . _Just) $ handleFormEvent (VtyEvent e)
              (Nothing, _) -> do
                -- Show "Sending …" status for in-modal Enter actions
                -- (decommit / increment / recover / close confirm). Done
                -- before handleVtyEventsHeadState so the read of openState
                -- sees the action being submitted, not the post-action
                -- 'OpenHome'.
                setPendingAction e
                zoom (connectedStateL . connectionL . headStateL) $
                  handleVtyEventsHeadState cardanoClient client chan e
                -- Auto-close modal if action completed and we returned to OpenHome
                newOpenScreen <- useOpenScreen
                case newOpenScreen of
                  Just OpenHome -> leaveModal
                  _ -> pure ()
          _ -> do
            unless modalOpen $ setPendingAction e
            zoom (connectedStateL . connectionL . headStateL) $
              handleVtyEventsHeadState cardanoClient client chan e
            -- Switch to ModalTab when a modal flow starts
            newOpenScreen <- useOpenScreen
            case newOpenScreen of
              Just LoadingUTxOForIncrement -> enterModal
              Just (SelectingUTxO _) -> enterModal
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
          liftIO $ queryL1UTxOAsync cardanoClient myAddr chan UTxOQueryResult "L1 UTxO query failed"
        -- 'r' (recover) is handled at the outer event loop as a top-level
        -- modal flow (see 'EvKey (KChar 'r')' in handleEvent), not as an
        -- 'OpenScreen' transition.
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
          liftIO $ externalCommitAsync hydraClient chan commitUTxO
          put OpenHome
        _ -> zoom selectingUTxOToIncrementFormL $ handleFormEvent (VtyEvent e)
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
              let Coin lovelaceLimit = selectLovelace (txOutValue (snd utxoSelected))
              let lovelaceAmount = adaToLovelace amountEntered lovelaceLimit
              case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Coin lovelaceAmount) (sk hydraClient) of
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
          let Coin lovelaceLimit = selectLovelace (txOutValue (snd utxoSelected))
          let lovelaceAmount = adaToLovelace amountEntered lovelaceLimit
          case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Coin lovelaceAmount) (sk hydraClient) of
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

-- | Convert a user-entered ADA amount to lovelace. The TUI accepts amounts
-- as 'Double' for usability (typing decimals), so any precision finer than
-- one lovelace is silently rounded. 'Double' represents integers exactly up
-- to 2^53 lovelace (~9e15), well above any realistic UTxO value, so the
-- only practical loss is sub-lovelace fractional input from the user. The
-- 'min' clamp defends against floating-point overshoot of the form
-- validator's upper bound.
adaToLovelace :: Double -> Integer -> Integer
adaToLovelace ada lovelaceLimit =
  min lovelaceLimit (round (ada * 1_000_000))

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
      -- Note: 'i' on Open OpenHome does not set a pending-action message —
      -- the modal panel itself shows "Querying Cardano node for available
      -- UTxO…" while LoadingUTxOForIncrement, which is enough.
      (Active (ActiveLink{activeHeadState = Open{openState}}), EvKey KEnter []) ->
        case openState of
          SelectingUTxOToDecommit _ ->
            pendingActionL .= Just "Sending decommit…"
          SelectingUTxOToIncrement _ ->
            pendingActionL .= Just "Sending increment…"
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

-- | Read the current 'OpenScreen', if the head is currently 'Open'.
useOpenScreen :: EventM Name RootState (Maybe OpenScreen)
useOpenScreen = gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)

-- | Mutate the current 'OpenScreen'. No-op if the head isn't 'Open'.
zoomOpenScreen :: EventM Name OpenScreen () -> EventM Name RootState ()
zoomOpenScreen = zoom (connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)

-- | Switch to the modal tab, remembering the currently-active tab so we can
-- return to it later via 'leaveModal'.
enterModal :: EventM Name RootState ()
enterModal = do
  tab <- use activeTabL
  previousTabL .= tab
  activeTabL .= ModalTab

-- | Return from the modal tab to whichever tab was active before.
leaveModal :: EventM Name RootState ()
leaveModal = do
  prev <- use previousTabL
  activeTabL .= prev

-- | If the recovery modal is open, rebuild its form from the current
-- 'pendingIncrements'. Keeps the user's selection if that deposit is still
-- pending, otherwise falls back to the first entry. If the list is now
-- empty (e.g. the selected deposit was finalized or recovered), close the
-- modal and surface a message in the pending-action slot. Should be called
-- after any node event that may mutate 'pendingIncrements'.
refreshRecoveryForm :: EventM Name RootState ()
refreshRecoveryForm = do
  mForm <- use recoveryFormL
  case mForm of
    Nothing -> pure ()
    Just form -> do
      let prevSelection = formState form
      mPending <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . pendingIncrementsL)
      let pis = fromMaybe [] mPending
      let pendingDepositIds = (\PendingIncrement{deposit, utxoToCommit} -> (deposit, utxoToCommit)) <$> pis
      case depositIdRadioFieldWith (Just prevSelection) pendingDepositIds of
        Just refreshed -> recoveryFormL .= Just refreshed
        Nothing -> do
          recoveryFormL .= Nothing
          tab <- use activeTabL
          when (tab == ModalTab) $ do
            leaveModal
            pendingActionL .= Just "No pending deposits left to recover."

-- | Run an IO action in a background thread, reporting any exception through
-- the TUI's event channel as a 'TxBuildError' (visible in the pending-action
-- slot). 'cleanup' events, if any, are written before the error so the TUI
-- can reset transient loading state. Order matters — 'TxBuildError' is
-- written last so its message wins over anything 'cleanup' might display.
--
-- Forking is essential for any blocking call against the hydra-node or
-- cardano-node: if the remote is unresponsive a synchronous call would block
-- the brick event loop, leaving no way to press Esc or Ctrl-C.
forkWithErrorReport ::
  BChan (TUIEvent Tx) ->
  Text ->
  [TUIEvent Tx] ->
  IO () ->
  IO ()
forkWithErrorReport chan errorPrefix cleanup action =
  void $
    forkIO $
      handle
        ( \(ex :: SomeException) -> do
            mapM_ (writeBChan chan) cleanup
            writeBChan chan (TxBuildError (errorPrefix <> ": " <> show ex))
        )
        action

-- | Query L1 UTxO at the given address in a background thread. On success,
-- the result is wrapped via 'mkResult' and written to the channel. On
-- exception, an empty result is written first (so the TUI clears any
-- loading-state spinner) followed by a 'TxBuildError' with the real cause.
queryL1UTxOAsync ::
  CardanoClient ->
  Address ShelleyAddr ->
  BChan (TUIEvent Tx) ->
  (Map TxIn (TxOut CtxUTxO) -> TUIEvent Tx) ->
  Text ->
  IO ()
queryL1UTxOAsync cardanoClient myAddr chan mkResult errorPrefix =
  forkWithErrorReport chan errorPrefix [mkResult Map.empty] $ do
    utxo' <- queryUTxOByAddress cardanoClient [myAddr]
    writeBChan chan (mkResult (UTxO.toMap utxo'))

recoverCommitAsync :: Client Tx IO -> BChan (TUIEvent Tx) -> TxId -> IO ()
recoverCommitAsync client chan depositTxId =
  forkWithErrorReport chan "Recovery request failed" [] $
    recoverCommit client depositTxId

externalCommitAsync :: Client Tx IO -> BChan (TUIEvent Tx) -> UTxO -> IO ()
externalCommitAsync client chan commitUTxO =
  forkWithErrorReport chan "Increment request failed" [] $
    externalCommit client commitUTxO

triggerL1Query :: CardanoClient -> Client Tx IO -> BChan (TUIEvent Tx) -> EventM Name RootState ()
triggerL1Query cardanoClient client chan = do
  let myAddr = mkMyAddress cardanoClient client
  liftIO $ queryL1UTxOAsync cardanoClient myAddr chan L1UTxORefresh "L1 wallet refresh failed"

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
