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
import Options.Applicative

data InfoLine = InfoLine {toplabel :: LogType, details :: Text} deriving (Eq, Show)

data Decoded tx
  = DecodedHydraLog {t :: UTCTime, n :: Text, infoLine :: InfoLine}
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

newtype Options = Options
  { paths :: [FilePath]
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> many
      ( strArgument
          ( metavar "FILES"
              <> help "One or more log file paths."
          )
      )

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "Group logs by the timestamp and display using colors and separators for easy inspection."
        <> header "Visualize hydra-node logs"
    )

main :: IO ()
main = do
  args <- execParser opts
  visualize $ paths args

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

-- | Ideally we would have Data instances for all types so we could get data type string representation
-- instead of providing strings directly but that would add some compilation time overhead so not worth it.
processLogs :: Envelope (HydraLog Tx) -> IO (Decoded Tx)
processLogs decoded =
  case decoded.message of
    NodeOptions opt -> logIt NodeOptionsLabel opt
    Node msg ->
      case msg of
        BeginInput{input} ->
          case input of
            ClientInput{clientInput} -> logIt ClientSentLabel clientInput
            NetworkInput{} -> pure DropLog
            ChainInput{chainEvent} ->
              case chainEvent of
                Observation{observedTx} -> logIt ObservationLabel observedTx
                Rollback{} -> pure DropLog
                Tick{} -> pure DropLog
                PostTxError{postTxError} -> logIt ErrorLabel postTxError
        EndInput{} -> pure DropLog
        BeginEffect{effect} ->
          case effect of
            ClientEffect{} -> pure DropLog
            NetworkEffect{message} -> logIt NetworkLabel message
            OnChainEffect{postChainTx} -> logIt ChainEffectLabel postChainTx
        EndEffect{} -> pure DropLog
        LogicOutcome{outcome} ->
          case outcome of
            Continue{stateChanges} ->
              foldM
                ( \_ a -> case a of
                    details@HeadInitialized{} -> logIt (LogicLabel "HeadInitialized") details
                    details@HeadOpened{} -> logIt (LogicLabel "HeadOpened") details
                    details@CommittedUTxO{} -> logIt (LogicLabel "CommittedUTxO") details
                    details@HeadAborted{} -> logIt (LogicLabel "HeadAborted") details
                    details@SnapshotRequestDecided{} -> logIt (LogicLabel "SnapshotRequestDecided") details
                    details@SnapshotRequested{} -> logIt (LogicLabel "SnapshotRequested") details
                    details@PartySignedSnapshot{} -> logIt (LogicLabel "PartySignedSnapshot") details
                    details@SnapshotConfirmed{} -> logIt (LogicLabel "SnapshotConfirmed") details
                    details@DepositRecorded{} -> logIt (LogicLabel "DepositRecorded") details
                    details@DepositActivated{} -> logIt (LogicLabel "DepositActivated") details
                    details@DepositExpired{} -> logIt (LogicLabel "DepositExpired") details
                    details@DepositRecovered{} -> logIt (LogicLabel "DepositRecovered") details
                    details@CommitApproved{} -> logIt (LogicLabel "CommitApproved") details
                    details@CommitFinalized{} -> logIt (LogicLabel "CommitFinalized") details
                    details@DecommitRecorded{} -> logIt (LogicLabel "DecommitRecorded") details
                    details@DecommitApproved{} -> logIt (LogicLabel "DecommitApproved") details
                    details@DecommitInvalid{} -> logIt (LogicLabel "DecommitInvalid") details
                    details@DecommitFinalized{} -> logIt (LogicLabel "DecommitFinalized") details
                    details@HeadClosed{} -> logIt (LogicLabel "HeadClosed") details
                    details@HeadContested{} -> logIt (LogicLabel "HeadContested") details
                    details@HeadIsReadyToFanout{} -> logIt (LogicLabel "HeadIsReadyToFanout") details
                    details@HeadFannedOut{} -> logIt (LogicLabel "HeadFannedOut") details
                    details@IgnoredHeadInitializing{} -> logIt (LogicLabel "IgnoredHeadInitializing") details
                    details@TxInvalid{} -> logIt (LogicLabel "TxInvalid") details
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
            Error{error = err} -> logIt (LogicError "LOGIC ERROR") err
        DroppedFromQueue{} -> pure DropLog
        LoadingState -> logIt (Other "Loading state...") ()
        LoadedState{} -> logIt (Other "Loaded.") ()
        ReplayingState -> logIt (Other "Replaying state...") ()
        details@Misconfiguration{} -> logIt (Other "MISCONFIG!") details
    _ -> pure DropLog
 where
  logIt :: Show x => LogType -> x -> IO (Decoded Tx)
  logIt l s =
    pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine l (show s))

render :: Decoded tx -> IO ()
render = \case
  DecodedHydraLog{t, n, infoLine = InfoLine{toplabel, details}} -> do
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
