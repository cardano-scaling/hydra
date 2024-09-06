{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Handlers where

import Hydra.Prelude hiding (Down, padLeft)

import Brick
import Hydra.Cardano.Api hiding (Active)
import Hydra.Chain (PostTxError (InternalWalletError, NotEnoughFuel), reason)

import Brick.Forms (Form (formState), editField, editShowableFieldWithValidate, handleFormEvent, newForm)
import Cardano.Api.UTxO qualified as UTxO
import Data.List (nub, (\\))
import Data.Map qualified as Map
import Graphics.Vty (
  Event (EvKey),
  Key (..),
  Modifier (MCtrl),
 )
import Graphics.Vty qualified as Vty
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..))
import Hydra.Cardano.Api.Prelude ()
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..), HydraEvent (..))
import Hydra.Ledger (IsTx (..), balance)
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Party (Party)
import Hydra.Snapshot (Snapshot (..))
import Hydra.TUI.Forms
import Hydra.TUI.Logging.Handlers (info, report, warn)
import Hydra.TUI.Logging.Types (LogMessage, LogState, LogVerbosity (..), Severity (..), logMessagesL, logVerbosityL)
import Hydra.TUI.Model
import Hydra.TUI.Style (own)
import Lens.Micro.Mtl (use, (%=), (.=))

handleEvent ::
  CardanoClient ->
  Client Tx IO ->
  BrickEvent Name (HydraEvent Tx) ->
  EventM Name RootState ()
handleEvent cardanoClient client = \case
  AppEvent e -> do
    handleTick e
    zoom connectedStateL $ do
      handleHydraEventsConnectedState e
      zoom connectionL $ handleHydraEventsConnection e
    zoom (logStateL . logMessagesL) $
      handleHydraEventsInfo e
  MouseDown{} -> pure ()
  MouseUp{} -> pure ()
  VtyEvent e -> case e of
    EvKey (KChar 'c') [MCtrl] -> halt
    EvKey (KChar 'd') [MCtrl] -> halt
    EvKey (KChar 'q') [] -> halt
    EvKey (KChar 'Q') [] -> halt
    _ -> do
      -- FIXME: the log event handler conflicts with edit field input; e.g.
      -- pressing 'f' in an edit field will have the full event log be shown.
      -- Should: pattern match on event / key binding and handle depending on
      -- state using lenses. See also:
      -- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#customizable-keybindings
      zoom logStateL $ handleVtyEventsLogState e
      zoom (connectedStateL . connectionL . headStateL) $
        handleVtyEventsHeadState cardanoClient client e

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

handleHydraEventsConnection :: HydraEvent Tx -> EventM Name Connection ()
handleHydraEventsConnection = \case
  Update TimedServerOutput{output = Greetings{me}} -> meL .= Identified me
  Update TimedServerOutput{output = PeerConnected p} -> peersL %= \cp -> nub $ cp <> [p]
  Update TimedServerOutput{output = PeerDisconnected p} -> peersL %= \cp -> cp \\ [p]
  e -> zoom headStateL $ handleHydraEventsHeadState e

handleHydraEventsHeadState :: HydraEvent Tx -> EventM Name HeadState ()
handleHydraEventsHeadState e = do
  case e of
    Update TimedServerOutput{time, output = HeadIsInitializing{parties, headId}} ->
      put $ Active (newActiveLink (toList parties) headId)
    Update TimedServerOutput{time, output = HeadIsAborted{}} ->
      put Idle
    _ -> pure ()
  zoom activeLinkL $ handleHydraEventsActiveLink e

handleHydraEventsActiveLink :: HydraEvent Tx -> EventM Name ActiveLink ()
handleHydraEventsActiveLink e = do
  case e of
    Update TimedServerOutput{output = Committed{party, utxo}} -> do
      partyCommitted party utxo
    Update TimedServerOutput{time, output = HeadIsOpen{utxo}} -> do
      activeHeadStateL .= Open OpenHome
    Update TimedServerOutput{time, output = SnapshotConfirmed{snapshot = Snapshot{utxo}}} ->
      utxoL .= utxo
    Update TimedServerOutput{time, output = HeadIsClosed{headId, snapshotNumber, contestationDeadline}} -> do
      activeHeadStateL .= Closed{closedState = ClosedState{contestationDeadline}}
    Update TimedServerOutput{time, output = ReadyToFanout{}} ->
      activeHeadStateL .= FanoutPossible
    Update TimedServerOutput{time, output = HeadIsFinalized{utxo}} -> do
      utxoL .= utxo
      activeHeadStateL .= Final
    Update TimedServerOutput{time, output = DecommitRequested{utxoToDecommit}} ->
      pendingUTxOToDecommitL .= utxoToDecommit
    Update TimedServerOutput{time, output = DecommitFinalized{}} ->
      pendingUTxOToDecommitL .= mempty
    _ -> pure ()

handleHydraEventsInfo :: HydraEvent Tx -> EventM Name [LogMessage] ()
handleHydraEventsInfo = \case
  Update TimedServerOutput{time, output = HeadIsInitializing{parties, headId}} ->
    info time "Head is initializing"
  Update TimedServerOutput{time, output = Committed{party, utxo}} -> do
    info time $ show party <> " committed " <> renderValue (balance @Tx utxo)
  Update TimedServerOutput{time, output = HeadIsOpen{utxo}} -> do
    info time "Head is now open!"
  Update TimedServerOutput{time, output = HeadIsAborted{}} -> do
    info time "Head aborted, back to square one."
  Update TimedServerOutput{time, output = SnapshotConfirmed{snapshot = Snapshot{number}}} ->
    info time ("Snapshot #" <> show number <> " confirmed.")
  Update TimedServerOutput{time, output = CommandFailed{clientInput}} -> do
    warn time $ "Invalid command: " <> show clientInput
  Update TimedServerOutput{time, output = HeadIsClosed{snapshotNumber}} -> do
    info time $ "Head closed with snapshot number " <> show snapshotNumber
  Update TimedServerOutput{time, output = HeadIsContested{snapshotNumber, contestationDeadline}} -> do
    info time ("Head contested with snapshot number " <> show snapshotNumber <> " and deadline " <> show contestationDeadline)
  Update TimedServerOutput{time, output = TxValid{}} ->
    report Success time "Transaction submitted successfully"
  Update TimedServerOutput{time, output = TxInvalid{transaction, validationError}} ->
    warn time ("Transaction with id " <> show (txId transaction) <> " is not applicable: " <> show validationError)
  Update TimedServerOutput{time, output = DecommitApproved{}} ->
    report Success time "Decommit approved and submitted to Cardano"
  Update TimedServerOutput{time, output = DecommitInvalid{decommitTx, decommitInvalidReason}} ->
    warn time ("Decommit Transaction with id " <> show (txId decommitTx) <> " is not applicable: " <> show decommitInvalidReason)
  Update TimedServerOutput{time, output = HeadIsFinalized{utxo}} -> do
    info time "Head is finalized"
  Update TimedServerOutput{time, output = InvalidInput{reason}} ->
    warn time ("Invalid input error: " <> toText reason)
  Update TimedServerOutput{time, output = PostTxOnChainFailed{postTxError}} ->
    case postTxError of
      NotEnoughFuel -> do
        warn time "Not enough Fuel. Please provide more to the internal wallet and try again."
      InternalWalletError{reason} ->
        warn time reason
      _ -> warn time ("An error happened while trying to post a transaction on-chain: " <> show postTxError)
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
  zoom activeHeadStateL $ handleVtyEventsActiveHeadState cardanoClient hydraClient utxo e

handleVtyEventsActiveHeadState :: CardanoClient -> Client Tx IO -> UTxO -> Vty.Event -> EventM Name ActiveHeadState ()
handleVtyEventsActiveHeadState cardanoClient hydraClient utxo e = do
  zoom (initializingStateL . initializingScreenL) $ handleVtyEventsInitializingScreen cardanoClient hydraClient e
  zoom openStateL $ handleVtyEventsOpen cardanoClient hydraClient utxo e
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

handleVtyEventsOpen :: CardanoClient -> Client Tx IO -> UTxO -> Vty.Event -> EventM Name OpenScreen ()
handleVtyEventsOpen cardanoClient hydraClient utxo e =
  get >>= \case
    OpenHome -> do
      case e of
        EvKey (KChar 'n') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          put $ SelectingUTxO (utxoRadioField utxo')
        EvKey (KChar 'd') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          put $ SelectingUTxOToDecommit (utxoRadioField utxo')
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
                :| (SelectAddress . txOutAddress <$> toList utxo)
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

handleVtyEventsConnection ::
  CardanoClient ->
  Client Tx IO ->
  Vty.Event ->
  EventM Name Connection ()
handleVtyEventsConnection cardanoClient hydraClient e = do
  zoom headStateL $ handleVtyEventsHeadState cardanoClient hydraClient e

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
