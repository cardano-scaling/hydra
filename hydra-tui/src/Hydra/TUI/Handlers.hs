{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Handlers where

import Hydra.Prelude hiding (Down, padLeft)

import Brick
import Hydra.Cardano.Api hiding (Active)
import Hydra.Chain (PostTxError (InternalWalletError, NotEnoughFuel), reason)

import Brick.Forms (Form (formState), editShowableFieldWithValidate, handleFormEvent, newForm, radioField)
import Cardano.Api.UTxO qualified as UTxO
import Data.List (nub, (\\))
import Data.Map qualified as Map
import Graphics.Vty (
  Event (EvKey),
  Key (..),
 )
import Graphics.Vty qualified as Vty
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..), TimedServerOutput (..))
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..), HydraEvent (..))
import Hydra.Ledger (IsTx (..), balance)
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Party (Party)
import Hydra.Snapshot (Snapshot (..))
import Hydra.TUI.Forms
import Hydra.TUI.Handlers.Global (handleVtyGlobalEvents)
import Hydra.TUI.Logging.Handlers (info, report, warn)
import Hydra.TUI.Logging.Types (LogMessage, LogState, LogVerbosity (..), Severity (..), logMessagesL, logVerbosityL)
import Hydra.TUI.Model
import Lens.Micro.Mtl (use, (%=), (.=))
import Prelude qualified

handleEvent ::
  CardanoClient ->
  Client Tx IO ->
  BrickEvent Name (HydraEvent Tx) ->
  EventM Name RootState ()
handleEvent cardanoClient client e = do
  handleVtyEventVia (handleExtraHotkeys (handleEvent cardanoClient client)) () e
  zoom logStateL $ handleVtyEventVia handleVtyEventsLogState () e
  handleAppEventVia handleTick () e
  zoom connectedStateL $ do
    handleAppEventVia handleHydraEventsConnectedState () e
    zoom connectionL $ handleBrickEventsConnection cardanoClient client e
  zoom (logStateL . logMessagesL) $
    handleAppEventVia handleHydraEventsInfo () e
  -- XXX: Global events must be handled as the very last step.
  -- Any `EventM` that decides to `Continue` would override the `Halt` decision.
  handleGlobalEvents e

handleExtraHotkeys :: (BrickEvent w e -> EventM n s ()) -> Vty.Event -> EventM n s ()
handleExtraHotkeys f = \case
  EvKey KDown [] -> f $ VtyEvent $ EvKey (KChar '\t') []
  EvKey KUp [] -> f $ VtyEvent $ EvKey KBackTab []
  _ -> pure ()

handleTick :: HydraEvent Tx -> EventM Name RootState ()
handleTick = \case
  Tick now -> nowL .= now
  _ -> pure ()

handleAppEventVia :: (e -> EventM n s a) -> a -> BrickEvent w e -> EventM n s a
handleAppEventVia f x = \case
  AppEvent e -> f e
  _ -> pure x

handleVtyEventVia :: (Vty.Event -> EventM n s a) -> a -> BrickEvent w e -> EventM n s a
handleVtyEventVia f x = \case
  VtyEvent e -> f e
  _ -> pure x

handleGlobalEvents :: BrickEvent Name (HydraEvent Tx) -> EventM Name RootState ()
handleGlobalEvents = \case
  AppEvent _ -> pure ()
  VtyEvent e -> handleVtyGlobalEvents e
  _ -> pure ()

handleHydraEventsConnectedState :: HydraEvent Tx -> EventM Name ConnectedState ()
handleHydraEventsConnectedState = \case
  ClientConnected -> id .= Connected emptyConnection
  ClientDisconnected -> id .= Disconnected
  _ -> pure ()

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
      id .= ConfirmingAbort confirmRadioField
    _ -> pure ()
  initializingScreen <- use id
  case initializingScreen of
    InitializingHome -> case e of
      EvKey (KChar 'c') [] -> do
        utxo <- liftIO $ queryUTxOByAddress cardanoClient [mkMyAddress cardanoClient hydraClient]
        id .= CommitMenu (utxoCheckboxField $ UTxO.toMap utxo)
      _ -> pure ()
    CommitMenu i -> do
      case e of
        EvKey KEsc [] -> id .= InitializingHome
        EvKey KEnter [] -> do
          let u = formState i
          let commitUTxO = UTxO $ Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) u
          liftIO $ externalCommit hydraClient commitUTxO
          id .= InitializingHome
        _ -> pure ()
      zoom commitMenuL $ handleFormEvent (VtyEvent e)
    ConfirmingAbort i -> do
      case e of
        EvKey KEsc [] -> id .= InitializingHome
        EvKey KEnter [] -> do
          let selected = formState i
          if selected
            then liftIO $ sendInput hydraClient Abort
            else id .= InitializingHome
        _ -> pure ()
      zoom confirmingAbortFormL $ handleFormEvent (VtyEvent e)

handleVtyEventsOpen :: CardanoClient -> Client Tx IO -> UTxO -> Vty.Event -> EventM Name OpenScreen ()
handleVtyEventsOpen cardanoClient hydraClient utxo e = do
  case e of
    EvKey (KChar 'c') [] ->
      id .= ConfirmingClose confirmRadioField
    _ -> pure ()
  k <- use id
  case k of
    ConfirmingClose i -> do
      case e of
        EvKey KEsc [] -> id .= OpenHome
        EvKey KEnter [] -> do
          let selected = formState i
          if selected
            then liftIO $ sendInput hydraClient Close
            else id .= OpenHome
        _ -> pure ()
      zoom confirmingCloseFormL $ handleFormEvent (VtyEvent e)
    OpenHome -> do
      case e of
        EvKey (KChar 'n') [] -> do
          let utxo' = myAvailableUTxO (networkId cardanoClient) (getVerificationKey $ sk hydraClient) utxo
          id .= SelectingUTxO (utxoRadioField utxo')
        _ -> pure ()
    SelectingUTxO i -> do
      case e of
        EvKey KEsc [] -> id .= OpenHome
        EvKey KEnter [] -> do
          let utxoSelected@(_, TxOut{txOutValue = v}) = formState i
          let Lovelace limit = selectLovelace v
          let enteringAmountForm =
                let field = editShowableFieldWithValidate id "amount" (\n -> n > 0 && n <= limit)
                 in newForm [field] limit
          id .= EnteringAmount{utxoSelected, enteringAmountForm}
        _ -> pure ()
      zoom selectingUTxOFormL $ handleFormEvent (VtyEvent e)
    EnteringAmount utxoSelected i -> do
      case e of
        EvKey KEsc [] -> id .= OpenHome
        EvKey KEnter [] -> do
          let amountEntered = formState i
          let ownAddress = mkVkAddress (networkId cardanoClient) (getVerificationKey $ sk hydraClient)
          let field =
                radioField
                  id
                  [ (u, show u, decodeUtf8 $ encodePretty u)
                  | u <- filter (/= ownAddress) (nub addresses)
                  ]
              addresses = getRecipientAddress <$> Map.elems (UTxO.toMap utxo)
              getRecipientAddress TxOut{txOutAddress = addr} = addr
          let selectingRecipientForm = newForm [field] (Prelude.head addresses)
          id .= SelectingRecipient{utxoSelected, amountEntered, selectingRecipientForm}
        _ -> pure ()
      zoom enteringAmountFormL $ handleFormEvent (VtyEvent e)
    SelectingRecipient utxoSelected amountEntered i -> do
      case e of
        EvKey KEsc [] -> id .= OpenHome
        EvKey KEnter [] -> do
          let recipient = formState i
          case mkSimpleTx utxoSelected (recipient, lovelaceToValue $ Lovelace amountEntered) (sk hydraClient) of
            Left _ -> pure ()
            Right tx -> do
              liftIO (sendInput hydraClient (NewTx tx))
          id .= OpenHome
        _ -> pure ()
      zoom selectingRecipientFormL $ handleFormEvent (VtyEvent e)

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
      id .= Active (newActiveLink (toList parties) headId)
    Update TimedServerOutput{time, output = HeadIsAborted{}} ->
      id .= Idle
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
  Update TimedServerOutput{time, output = TxValid{}} ->
    report Success time "Transaction submitted successfully!"
  Update TimedServerOutput{time, output = TxInvalid{transaction, validationError}} ->
    warn time ("Transaction with id " <> show (txId transaction) <> " is not applicable: " <> show validationError)
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

handleBrickEventsConnection ::
  CardanoClient ->
  Client Tx IO ->
  BrickEvent w (HydraEvent Tx) ->
  EventM Name Connection ()
handleBrickEventsConnection cardanoClient hydraClient x = case x of
  AppEvent e -> handleHydraEventsConnection e
  VtyEvent e -> handleVtyEventsConnection cardanoClient hydraClient e
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

--
-- View
--
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
