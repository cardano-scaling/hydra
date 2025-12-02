{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Handlers where

import Hydra.Prelude hiding (Down)

import Brick
import Brick.Forms (Form (formState), editField, editShowableFieldWithValidate, handleFormEvent, newForm)
import Cardano.Api.UTxO qualified as UTxO
import Data.List (nub, (\\))
import Data.Map qualified as Map
import Data.Text qualified as T
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
import Hydra.TUI.Logging.Types (LogMessage, LogState, LogVerbosity (..), Severity (..), logMessagesL, logVerbosityL)
import Hydra.TUI.Model
import Hydra.TUI.Style (own)
import Hydra.Tx (IsTx (..), Party, Snapshot (..), balance)
import Lens.Micro.Mtl (use, (%=), (.=))

handleEvent ::
  CardanoClient ->
  Client Tx IO ->
  BrickEvent Name (HydraEvent Tx) ->
  EventM Name RootState ()
handleEvent cardanoClient client = \case
  AppEvent e -> do
    handleTick e
    now <- use nowL
    zoom connectedStateL $ do
      handleHydraEventsConnectedState e
      zoom connectionL $ handleHydraEventsConnection now e
    zoom (logStateL . logMessagesL) $
      handleHydraEventsInfo e
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
      _ -> do
        zoom (connectedStateL . connectionL . headStateL) $
          handleVtyEventsHeadState cardanoClient client e

        unless modalOpen $ do
          zoom logStateL $ handleVtyEventsLogState e

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
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NodeUnsynced}) -> do
    chainSyncedStatusL .= CatchingUp
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NodeSynced}) -> do
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
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsInitializing{parties, headId}}) ->
      put $ Active (newActiveLink (toList parties) headId)
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.EventLogRotated{checkpoint}}) -> do
      modify $ \current -> recoverHeadState now current checkpoint
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsAborted{}}) ->
      put Idle
    _ -> pure ()
  zoom activeLinkL $ handleHydraEventsActiveLink e

handleHydraEventsActiveLink :: HydraEvent Tx -> EventM Name ActiveLink ()
handleHydraEventsActiveLink e = do
  case e of
    Update (ApiTimedServerOutput TimedServerOutput{output = API.Committed{party, utxo}}) -> do
      partyCommitted party utxo
    Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsOpen{utxo}}) -> do
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
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsInitializing{parties, headId}}) ->
    info time "Head is initializing"
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.Committed{party, utxo}}) ->
    info time $ show party <> " committed " <> renderValue (balance @Tx utxo)
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsOpen{utxo}}) ->
    info time "Head is now open!"
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.HeadIsAborted{}}) ->
    info time "Head aborted, back to square one."
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.SnapshotConfirmed{snapshot = Snapshot{number}}}) ->
    info time ("Snapshot #" <> show number <> " confirmed.")
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.SnapshotSideLoaded{snapshotNumber}}) ->
    info time ("Snapshot #" <> show snapshotNumber <> " side loaded.")
  Update (ApiClientMessage API.CommandFailed{clientInput}) -> do
    time <- liftIO getCurrentTime
    warn time $ "Invalid command: " <> show clientInput
  Update (ApiClientMessage API.RejectedInput{clientInput, reason}) -> do
    time <- liftIO getCurrentTime
    warn time $ "Rejected command: " <> show clientInput <> " Reason: " <> show reason
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NodeUnsynced}) -> do
    warn time "Node state is out of sync with chain backend."
  Update (ApiTimedServerOutput TimedServerOutput{time, output = API.NodeSynced}) -> do
    warn time "Node state is back in sync with chain backend."
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

partyCommitted :: Party -> UTxO -> EventM n ActiveLink ()
partyCommitted party commit = do
  zoom (activeHeadStateL . initializingStateL) $ do
    remainingPartiesL %= (\\ [party])
  utxoL %= (<> commit)

-- * VtyEvent handlers

handleVtyEventsHeadState :: CardanoClient -> Client Tx IO -> Vty.Event -> EventM Name HeadState ()
handleVtyEventsHeadState cardanoClient hydraClient e = do
  h <- use id
  case h of
    Idle -> case e of
      EvKey (KChar 'i') [] -> liftIO (sendInput hydraClient Init)
      _ -> pure ()
    _ -> pure ()
  zoom activeLinkL $ handleVtyEventsActiveLink cardanoClient hydraClient e

handleVtyEventsActiveLink :: CardanoClient -> Client Tx IO -> Vty.Event -> EventM Name ActiveLink ()
handleVtyEventsActiveLink cardanoClient hydraClient e = do
  utxo <- use utxoL
  pendingIncrements <- use pendingIncrementsL
  zoom activeHeadStateL $ handleVtyEventsActiveHeadState cardanoClient hydraClient utxo pendingIncrements e

handleVtyEventsActiveHeadState :: CardanoClient -> Client Tx IO -> UTxO -> [PendingIncrement] -> Vty.Event -> EventM Name ActiveHeadState ()
handleVtyEventsActiveHeadState cardanoClient hydraClient utxo pendingIncrements e = do
  zoom (initializingStateL . initializingScreenL) $ handleVtyEventsInitializingScreen cardanoClient hydraClient e
  zoom openStateL $ handleVtyEventsOpen cardanoClient hydraClient utxo pendingIncrements e
  s <- use id
  case s of
    FanoutPossible -> handleVtyEventsFanoutPossible hydraClient e
    Final -> handleVtyEventsFinal hydraClient e
    _ -> pure ()

handleVtyEventsInitializingScreen :: CardanoClient -> Client Tx IO -> Vty.Event -> EventM Name InitializingScreen ()
handleVtyEventsInitializingScreen cardanoClient hydraClient e = do
  case e of
    EvKey (KChar 'a') [] ->
      put $ ConfirmingAbort confirmRadioField
    _ -> pure ()
  initializingScreen <- use id
  case initializingScreen of
    InitializingHome -> case e of
      EvKey (KChar 'c') [] -> do
        utxo <- liftIO $ queryUTxOByAddress cardanoClient [mkMyAddress cardanoClient hydraClient]
        put $ CommitMenu (utxoCheckboxField $ UTxO.toMap utxo)
      _ -> pure ()
    CommitMenu i -> do
      case e of
        EvKey KEsc [] -> put InitializingHome
        EvKey KEnter [] -> do
          let u = formState i
          let commitUTxO = UTxO $ Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) u
          liftIO $ externalCommit hydraClient commitUTxO
          put InitializingHome
        _ -> pure ()
      zoom commitMenuL $ handleFormEvent (VtyEvent e)
    ConfirmingAbort i -> do
      case e of
        EvKey KEsc [] -> put InitializingHome
        EvKey KEnter [] -> do
          let selected = formState i
          if selected
            then liftIO $ sendInput hydraClient Abort
            else put InitializingHome
        _ -> pure ()
      zoom confirmingAbortFormL $ handleFormEvent (VtyEvent e)

handleVtyEventsOpen :: CardanoClient -> Client Tx IO -> UTxO -> [PendingIncrement] -> Vty.Event -> EventM Name OpenScreen ()
handleVtyEventsOpen cardanoClient hydraClient utxo pendingIncrements e =
  get >>= \case
    OpenHome -> do
      case e of
        EvKey (KChar 'n') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          put $ SelectingUTxO (utxoRadioField utxo')
        EvKey (KChar 'd') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          put $ SelectingUTxOToDecommit (utxoRadioField utxo')
        EvKey (KChar 'i') [] -> do
          utxo' <- liftIO $ queryUTxOByAddress cardanoClient [mkMyAddress cardanoClient hydraClient]
          put $ SelectingUTxOToIncrement (utxoRadioField $ UTxO.toMap utxo')
        EvKey (KChar 'r') [] -> do
          let pendingDepositIds = (\PendingIncrement{deposit, utxoToCommit} -> (deposit, utxoToCommit)) <$> pendingIncrements
          put $ SelectingDepositIdToRecover (depositIdRadioField pendingDepositIds)
        EvKey (KChar 'c') [] ->
          put $ ConfirmingClose confirmRadioField
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
          let Coin limit = selectLovelace v
          let enteringAmountForm =
                let field = editShowableFieldWithValidate id "amount" (\n -> n > 0 && n <= limit)
                 in newForm [field] limit
          put EnteringAmount{utxoSelected, enteringAmountForm}
        _ -> zoom selectingUTxOFormL $ handleFormEvent (VtyEvent e)
    SelectingUTxOToDecommit i ->
      case e of
        EvKey KEsc [] -> put OpenHome
        EvKey KEnter [] -> do
          let utxoSelected@(_, TxOut{txOutValue = v}) = formState i
          let recipient = mkVkAddress @Era (networkId cardanoClient) (getVerificationKey $ sk hydraClient)
          case mkSimpleTx utxoSelected (recipient, v) (sk hydraClient) of
            Left _ -> pure ()
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
              case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Coin amountEntered) (sk hydraClient) of
                Left _ -> pure ()
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
          case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Coin amountEntered) (sk hydraClient) of
            Left _ -> pure ()
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

handleVtyEventsLogState :: Vty.Event -> EventM Name LogState ()
handleVtyEventsLogState = \case
  EvKey (KChar '<') [] -> scroll Up
  EvKey (KChar '>') [] -> scroll Down
  EvKey (KChar 'h') [] -> logVerbosityL .= Full
  EvKey (KChar 's') [] -> logVerbosityL .= Short
  _ -> pure ()

scroll :: Direction -> EventM Name LogState ()
scroll direction = do
  x <- use logVerbosityL
  case x of
    Full -> do
      let vp = viewportScroll fullFeedbackViewportName
      vScrollPage vp direction
    Short -> do
      let vp = viewportScroll shortFeedbackViewportName
      hScrollPage vp direction

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
