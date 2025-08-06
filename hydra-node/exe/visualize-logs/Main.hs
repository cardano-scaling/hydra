{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Hydra.Cardano.Api (Tx)
import Hydra.Prelude hiding (encodeUtf8)
import Hydra.Prelude qualified as P

import Conduit
import Control.Lens ((^?))
import Control.Monad (foldM)
import Data.Aeson (eitherDecode')
import Data.Aeson.Lens (key, _String)
import Data.Text.Encoding (encodeUtf8)
import Hydra.Chain (ChainEvent (..))
import Hydra.HeadLogic (Effect (..), Input (..), Outcome (..), StateChanged (..))
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Node (HydraNodeLog (..))

data InfoLine = InfoLine {toplabel :: LogType, details :: Text} deriving (Eq, Show)

data Decoded tx
  = DecodedHydraLog {t :: UTCTime, n :: Text, info :: InfoLine}
  | DropLog
  deriving (Eq, Show)

-- | This instance is needed to sort results by timestamp
instance Ord (Decoded tx) where
  compare DropLog DropLog = EQ
  compare DropLog DecodedHydraLog{} = LT
  compare DecodedHydraLog{} DropLog = GT
  compare (DecodedHydraLog t1 _ _) (DecodedHydraLog t2 _ _) = compare t1 t2

-- | Log type labels for visualization
data LogType
  = NodeOptionsLabel
  | ClientSentLabel
  | ObservationLabel
  | NetworkLabel
  | ChainEffectLabel
  | ErrorLabel
  | LogicLabel Text
  | LogicError Text
  | Other Text
  deriving (Eq, Show)

labelLog :: LogType -> Text
labelLog NodeOptionsLabel = "NODE OPTIONS"
labelLog ClientSentLabel = "CLIENT SENT"
labelLog ObservationLabel = "OBSERVATION"
labelLog NetworkLabel = "NETWORK EFFECT"
labelLog ChainEffectLabel = "POSTING TX"
labelLog ErrorLabel = "ERROR"
labelLog (LogicLabel t) = unlines ["LOGIC", t]
labelLog (LogicError t) = unlines ["LOGIC ERROR", t]
labelLog (Other t) = unlines ["OTHER", t]

colorLog :: LogType -> Text
colorLog = \case
  NodeOptionsLabel -> green
  ClientSentLabel -> green
  ObservationLabel -> blue
  NetworkLabel -> magenta
  ChainEffectLabel -> blue
  ErrorLabel -> red
  LogicLabel t -> case t of
    "DepositExpired" -> red
    "DecommitInvalid" -> red
    "IgnoredHeadInitializing" -> red
    "TxInvalid" -> red
    _ -> cyan
  LogicError _ -> red
  Other _ -> green

main :: IO ()
main = visualize ["../devnet/alice-logs.txt", "../devnet/bob-logs.txt"]

visualize :: [FilePath] -> IO ()
visualize paths = do
  decodedLines <-
    runConduitRes $
      mapM_ sourceFileBS paths
        .| linesUnboundedAsciiC
        .| mapMC decodeAndProcess
        .| filterC (/= DropLog)
        .| sinkList

  forM_ (sort decodedLines) render

decodeAndProcess :: ByteString -> ResourceT IO (Decoded Tx)
decodeAndProcess l =
  case l ^? key "message" . _String of
    Nothing -> P.error "Failed to find key 'message' which was expected"
    Just line ->
      let envelope = fromStrict $ encodeUtf8 line
       in case eitherDecode' envelope :: Either String (Envelope (HydraLog Tx)) of
            Left e -> P.error $ "Decoding failed" <> show e <> "for line: " <> line
            Right decoded -> lift $ processLogs decoded

-- | Ideally we would have Data instances for all types so we could get data type String representation
-- instead of providing strings but that would add some compilation time overhead so not worth it.
processLogs :: Envelope (HydraLog Tx) -> IO (Decoded Tx)
processLogs decoded =
  case decoded.message of
    NodeOptions opt -> logIt NodeOptionsLabel (show opt)
    Node msg ->
      case msg of
        BeginInput{input} ->
          case input of
            ClientInput{clientInput} -> logIt ClientSentLabel (show clientInput)
            NetworkInput{} -> pure DropLog
            ChainInput{chainEvent} ->
              case chainEvent of
                Observation{observedTx} -> logIt ObservationLabel (show observedTx)
                Rollback{} -> pure DropLog
                Tick{} -> pure DropLog
                PostTxError{postTxError} -> logIt ErrorLabel (show postTxError)
        EndInput{} -> pure DropLog
        BeginEffect{effect} ->
          case effect of
            ClientEffect{} -> pure DropLog
            NetworkEffect{message} -> logIt NetworkLabel (show message)
            OnChainEffect{postChainTx} -> logIt ChainEffectLabel (show postChainTx)
        EndEffect{} -> pure DropLog
        LogicOutcome{outcome} ->
          case outcome of
            Continue{stateChanges} ->
              foldM
                ( \_ a -> case a of
                    HeadInitialized{} -> logIt (LogicLabel "HeadInitialized") ""
                    HeadOpened{} -> logIt (LogicLabel "HeadOpened") ""
                    CommittedUTxO{} -> logIt (LogicLabel "CommittedUTxO") ""
                    HeadAborted{} -> logIt (LogicLabel "HeadAborted") ""
                    SnapshotRequestDecided{} -> logIt (LogicLabel "SnapshotRequestDecided") ""
                    SnapshotRequested{} -> logIt (LogicLabel "SnapshotRequested") ""
                    PartySignedSnapshot{} -> logIt (LogicLabel "PartySignedSnapshot") ""
                    SnapshotConfirmed{} -> logIt (LogicLabel "SnapshotConfirmed") ""
                    DepositRecorded{} -> logIt (LogicLabel "DepositRecorded") ""
                    DepositActivated{} -> logIt (LogicLabel "DepositActivated") ""
                    DepositExpired{} -> logIt (LogicLabel "DepositExpired") ""
                    DepositRecovered{} -> logIt (LogicLabel "DepositRecovered") ""
                    CommitApproved{} -> logIt (LogicLabel "CommitApproved") ""
                    CommitFinalized{} -> logIt (LogicLabel "CommitFinalized") ""
                    DecommitRecorded{} -> logIt (LogicLabel "DecommitRecorded") ""
                    DecommitApproved{} -> logIt (LogicLabel "DecommitApproved") ""
                    DecommitInvalid{} -> logIt (LogicLabel "DecommitInvalid") ""
                    DecommitFinalized{} -> logIt (LogicLabel "DecommitFinalized") ""
                    HeadClosed{} -> logIt (LogicLabel "HeadClosed") ""
                    HeadContested{} -> logIt (LogicLabel "HeadContested") ""
                    HeadIsReadyToFanout{} -> logIt (LogicLabel "HeadIsReadyToFanout") ""
                    HeadFannedOut{} -> logIt (LogicLabel "HeadFannedOut") ""
                    IgnoredHeadInitializing{} -> logIt (LogicLabel "IgnoredHeadInitializing") ""
                    TxInvalid{} -> logIt (LogicLabel "TxInvalid") ""
                    NetworkConnected{} -> pure DropLog
                    NetworkDisconnected{} -> pure DropLog
                    PeerConnected{} -> pure DropLog
                    PeerDisconnected{} -> pure DropLog
                    NetworkVersionMismatch{} -> pure DropLog
                    NetworkClusterIDMismatch{} -> pure DropLog
                    TransactionReceived{} -> pure DropLog
                    TransactionAppliedToLocalUTxO{} -> pure DropLog
                    ChainRolledBack{} -> pure DropLog
                    TickObserved{} -> pure DropLog
                    LocalStateCleared{} -> pure DropLog
                    Checkpoint{} -> pure DropLog
                )
                DropLog
                stateChanges
            Wait{} -> pure DropLog
            Error{error = err} -> logIt (LogicError "LOGIC ERROR") (show err)
        DroppedFromQueue{} -> pure DropLog
        LoadingState -> logIt (Other "Loading state...") ""
        LoadedState{} -> logIt (Other "Loaded.") ""
        ReplayingState -> logIt (Other "Replaying state...") ""
        Misconfiguration{} -> logIt (Other "MISCONFIG!") ""
    _ -> pure DropLog
 where
  logIt :: LogType -> Text -> IO (Decoded Tx)
  logIt l s =
    pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine l s)

render :: Decoded tx -> IO ()
render = \case
  DecodedHydraLog{t, n, info = InfoLine{toplabel, details}} -> do
    putTextLn $
      unlines
        [ "-----------------------------------"
        , "[" <> show t <> "]"
        , "NAMESPACE:" <> show n
        , colorLog toplabel
        , labelLog toplabel
        , details
        , reset
        ]
  DropLog -> putTextLn ""

-- ANSI escape codes for colors
red :: Text
red = "\ESC[31m"

green :: Text
green = "\ESC[32m"

blue :: Text
blue = "\ESC[34m"

cyan :: Text
cyan = "\ESC[36m"

magenta :: Text
magenta = "\ESC[45m"

reset :: Text
reset = "\ESC[0m"
