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
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
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
import Hydra.Cardano.Api.Pretty (renderUTxO)
import Hydra.Chain (PostTxError (InternalWalletError, NotEnoughFuel), reason)
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (AllPossibleAPIMessages (..), Client (..), HydraEvent (..))
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Network (Host, readHost)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State qualified as NodeState
import Hydra.TUI.Forms
import Hydra.TUI.Logging.Handlers (info, report, warn)
import Hydra.TUI.Logging.Types (LogMessage (..), Severity (..), logMessagesL)
import Hydra.TUI.Model
import Hydra.TUI.Style (own)
import Hydra.Tx (IsTx (..), Snapshot (..))
import Lens.Micro ((^.), (^?))
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
      handleHydraEventsInfo e
    afterLen <- length <$> use (logStateL . logMessagesL)
    when (afterLen > beforeLen) $ do
      pendingActionL .= Nothing
      case e of
        Update (ApiTimedServerOutput tso) ->
          logStateL . logMessagesL %= \case
            (msg : rest) -> msg{rawJson = Just (TL.toStrict $ TLE.decodeUtf8 $ encodePretty (toJSON tso))} : rest
            [] -> []
        _ -> pure ()
    syncEventHistoryList
    case e of
      Update (ApiTimedServerOutput TimedServerOutput{output = API.CommitRecorded{}}) ->
        triggerL1Query cardanoClient client chan
      Update (ApiTimedServerOutput TimedServerOutput{output = API.DecommitFinalized{}}) ->
        triggerL1Query cardanoClient client chan
      _ -> pure ()
  AppEvent (L1UTxORefresh utxo) -> l1UTxOL .= Just utxo
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
                  put $ SelectingUTxOToIncrement form
              Nothing -> do
                -- Empty or failed query — close modal, return to previous tab, show message
                zoom (connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL) $
                  put OpenHome
                prev <- use previousTabL
                activeTabL .= prev
                pendingActionL .= Just "No L1 funds available to increment."
          _ -> pure ()
      _ -> pure ()
  MouseDown{} -> pure ()
  MouseUp{} -> pure ()
  VtyEvent e -> do
    modalOpen <- gets isModalOpen
    case e of
      EvKey (KChar 'c') [MCtrl] -> halt
      EvKey (KChar 'd') [MCtrl] -> halt
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
      EvKey (KChar 'r') [] | not modalOpen -> do
        tab <- use activeTabL
        case tab of
          FundsTab -> do
            l1UTxOL .= Nothing
            triggerL1Query cardanoClient client chan
          _ -> do
            zoom (connectedStateL . connectionL . headStateL) $
              handleVtyEventsHeadState cardanoClient client chan e
            newOpenScreen <- gets (^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL)
            case newOpenScreen of
              Just (SelectingDepositIdToRecover _) -> do
                previousTabL .= tab
                activeTabL .= ModalTab
              _ -> pure ()
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
                  zoom (connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL) $
                    put OpenHome
            case e of
              EvKey KEsc [] -> closeModal
              EvKey (KChar 'c') [] -> closeModal
              _ -> do
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
          case traverse readHost peerAddrs of
            Left err -> do
              liftIO $ putStrLn $ "Failed to parse configured peers: " <> err
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
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsFinalized{utxo}}) -> do
      utxoL .= utxo
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
      ActiveLink{utxo, pendingIncrements} <- get
      pendingIncrementsL .= pendingIncrements <> [PendingIncrement utxoToCommit pendingDeposit deadline PendingDeposit]
      utxoL .= utxo
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitApproved{utxoToCommit = approvedUtxoToCommit}}) -> do
      ActiveLink{utxo, pendingIncrements} <- get
      pendingIncrementsL
        .= fmap
          ( \inc@PendingIncrement{utxoToCommit = pendingUtxoToCommit, deposit, depositDeadline} ->
              if hashUTxO pendingUtxoToCommit == hashUTxO approvedUtxoToCommit
                then PendingIncrement pendingUtxoToCommit deposit depositDeadline FinalizingDeposit
                else inc
          )
          pendingIncrements
      utxoL .= utxo
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitFinalized{depositTxId}}) -> do
      ActiveLink{utxo, pendingIncrements} <- get
      let activePendingIncrements = filter (\PendingIncrement{deposit} -> deposit /= depositTxId) pendingIncrements
      let approvedIncrement = find (\PendingIncrement{deposit} -> deposit == depositTxId) pendingIncrements
      let activeUtxoToCommit = maybe mempty (\PendingIncrement{utxoToCommit} -> utxoToCommit) approvedIncrement
      pendingIncrementsL .= activePendingIncrements
      utxoL .= utxo <> activeUtxoToCommit
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitRecovered{recoveredUTxO, recoveredTxId}}) -> do
      ActiveLink{utxo, pendingIncrements} <- get
      let activePendingIncrements = filter (\PendingIncrement{deposit} -> deposit /= recoveredTxId) pendingIncrements
      pendingIncrementsL .= activePendingIncrements
      utxoL .= UTxO.difference utxo recoveredUTxO
    _ -> pure ()

handleHydraEventsInfo :: HydraEvent Tx -> EventM Name [LogMessage] ()
handleHydraEventsInfo = \case
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NetworkConnected}) ->
    report Success time "Network connected"
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NetworkDisconnected}) ->
    report Error time "Network disconnected"
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.PeerConnected{peer}}) ->
    info time $ "Peer connected: " <> show peer
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.PeerDisconnected{peer}}) ->
    info time $ "Peer disconnected: " <> show peer
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsOpen{parties, headId}}) ->
    info time "Head is now open!"
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.SnapshotConfirmed{snapshot = Snapshot{number, version}}}) ->
    info time ("Snapshot #" <> show number <> "." <> show version <> " confirmed.")
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.SnapshotSideLoaded{snapshotNumber}}) ->
    info time ("Snapshot #" <> show snapshotNumber <> " side loaded.")
  Update (ApiClientMessage API.CommandFailed{clientInput}) -> do
    time <- liftIO getCurrentTime
    warn time $ "Invalid command: " <> show clientInput
  Update (ApiClientMessage API.RejectedInputBecauseUnsynced{clientInput, drift}) -> do
    time <- liftIO getCurrentTime
    warn time $ "Rejected command: " <> show clientInput <> " Reason: " <> "Node is out of sync with chain, drift: " <> show drift
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NodeUnsynced{chainTime, drift}}) -> do
    warn time $
      "Node state is out of sync with chain backend. Chain time: "
        <> show chainTime
        <> ", Drift: "
        <> show drift
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NodeSynced{chainTime, drift}}) ->
    warn time $
      "Node state is back in sync with chain backend. Chain time: "
        <> show chainTime
        <> ", Drift: "
        <> show drift
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsClosed{snapshotNumber}}) ->
    info time $ "Head closed with snapshot number " <> show snapshotNumber
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsContested{snapshotNumber, contestationDeadline}}) ->
    info time ("Head contested with snapshot number " <> show snapshotNumber <> " and deadline " <> show contestationDeadline)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.TxValid{transactionId}}) ->
    report Success time ("Transaction " <> show transactionId <> " submitted successfully")
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.TxInvalid{transaction, validationError}}) ->
    warn time ("Transaction " <> show (Hydra.Tx.txId transaction) <> " is not applicable: " <> show validationError)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.DecommitApproved{decommitTxId, utxoToDecommit}}) ->
    report Success time $
      "Decommit approved and submitted to Cardano "
        <> show decommitTxId
        <> " "
        <> foldMap renderUTxO (UTxO.toList utxoToDecommit)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.DecommitFinalized{distributedUTxO}}) ->
    report Success time $
      "Decommit finalized "
        <> show distributedUTxO
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.DecommitInvalid{decommitTx, decommitInvalidReason}}) ->
    warn time $
      "Decommit Transaction with id "
        <> show (Hydra.Tx.txId decommitTx)
        <> " is not applicable: "
        <> show decommitInvalidReason
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitRecorded{utxoToCommit, pendingDeposit}}) ->
    report Success time $
      "Commit deposit recorded with "
        <> " deposit tx id "
        <> show pendingDeposit
        <> "and pending for approval "
        <> foldMap renderUTxO (UTxO.toList utxoToCommit)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitApproved{utxoToCommit}}) ->
    report Success time $
      "Commit approved and submitted to Cardano "
        <> foldMap renderUTxO (UTxO.toList utxoToCommit)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitRecovered{recoveredTxId, recoveredUTxO}}) ->
    report Success time $
      "Commit recovered "
        <> show recoveredTxId
        <> " "
        <> foldMap renderUTxO (UTxO.toList recoveredUTxO)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.CommitFinalized{depositTxId}}) ->
    report Success time $
      "Commit finalized "
        <> show depositTxId
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsFinalized{utxo}}) -> do
    info time "Head is finalized"
  Update (ApiInvalidInput API.InvalidInput{reason}) -> do
    time <- liftIO getCurrentTime
    warn time ("Invalid input error: " <> toText reason)
  Update (ApiClientMessage API.PostTxOnChainFailed{postTxError}) -> do
    time <- liftIO getCurrentTime
    case postTxError of
      NotEnoughFuel _ ->
        warn time "Not enough Fuel. Please provide more to the internal wallet and try again."
      InternalWalletError{reason} ->
        warn time reason
      _ -> warn time ("An error happened while trying to post a transaction on-chain: " <> show postTxError)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.EventLogRotated{}}) -> do
    info time "Checkpoint triggered: head state event log rotated"
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
          forM_ (utxoRadioField utxo') $ put . SelectingUTxO
        EvKey (KChar 'd') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          forM_ (utxoRadioField utxo') $ put . SelectingUTxOToDecommit
        EvKey (KChar 'i') [] -> do
          put LoadingUTxOForIncrement
          let myAddr = mkMyAddress cardanoClient hydraClient
          liftIO $ void $ forkIO $
            handle (\(_ :: SomeException) -> writeBChan chan (UTxOQueryResult Map.empty)) $ do
              utxo' <- queryUTxOByAddress cardanoClient [myAddr]
              writeBChan chan (UTxOQueryResult (UTxO.toMap utxo'))
        EvKey (KChar 'r') [] -> do
          let pendingDepositIds = (\PendingIncrement{deposit, utxoToCommit} -> (deposit, utxoToCommit)) <$> pendingIncrements
          forM_ (depositIdRadioField pendingDepositIds) $ put . SelectingDepositIdToRecover
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
            then liftIO $ sendInput hydraClient Close
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
  when (tab == EventHistoryTab) $ case e of
    EvKey KPageUp [] -> vScrollBy (viewportScroll "event-detail") (-10)
    EvKey KPageDown [] -> vScrollBy (viewportScroll "event-detail") 10
    Vty.EvMouseDown _ _ Vty.BScrollUp _ ->
      zoom eventHistoryListL $ BrickList.handleListEvent (EvKey KUp [])
    Vty.EvMouseDown _ _ Vty.BScrollDown _ ->
      zoom eventHistoryListL $ BrickList.handleListEvent (EvKey KDown [])
    _ -> zoom eventHistoryListL $ BrickList.handleListEvent e

syncEventHistoryList :: EventM Name RootState ()
syncEventHistoryList = do
  msgs <- use (logStateL . logMessagesL)
  eventHistoryListL %= \l ->
    let oldLen = Vec.length (BrickList.listElements l)
        newVec = Vec.fromList msgs
        newLen = Vec.length newVec
        added = newLen - oldLen
        newList = BrickList.listReplace newVec (BrickList.listSelected l) l
     in if added > 0
          then BrickList.listMoveTo (maybe 0 (+ added) (BrickList.listSelected l)) newList
          else newList

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
  liftIO $ void $ forkIO $
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
