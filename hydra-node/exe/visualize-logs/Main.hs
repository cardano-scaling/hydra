{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main where

import Hydra.Cardano.Api (Tx)
import Hydra.Prelude hiding (encodeUtf8)
import Hydra.Prelude qualified as P

import Conduit
import Control.Lens ((^?))
import Control.Monad (foldM)
import Data.Aeson (eitherDecode')
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.Text.Encoding (encodeUtf8)
import Hydra.Chain (ChainEvent (..))
import Hydra.HeadLogic (Effect (..), Input (..), Outcome (..), StateChanged (..))
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (..))
import Hydra.Node (HydraNodeLog (..))

data InfoLine = InfoLine {header :: Text, line :: Text, color :: Text} deriving (Eq, Show)

data Decoded tx
  = DecodedHydraLog {t :: UTCTime, n :: Text, info :: InfoLine}
  | DropLog
  deriving (Eq, Show)

instance Ord (Decoded tx) where
  compare DropLog DropLog = EQ
  compare DropLog DecodedHydraLog{} = LT
  compare DecodedHydraLog{} DropLog = GT
  compare (DecodedHydraLog t1 _ _) (DecodedHydraLog t2 _ _) = compare t1 t2

main :: IO ()
main = visualize ["../devnet/alice-logs.txt", "../devnet/bob-logs.txt"]

visualize :: [FilePath] -> IO ()
visualize paths = do
  decodedLines <-
    runConduitRes $
      mapM_ sourceFileBS paths
        .| linesUnboundedAsciiC
        .| mapMC
          ( \l ->
              case l ^? key "message" . _String of
                Nothing -> P.error "Failed to find key 'message' which was expected"
                Just line ->
                  let envelope = fromStrict $ encodeUtf8 line
                   in case decodeAs envelope (undefined :: Envelope (HydraLog Tx)) of
                        Left e -> P.error $ show e <> line
                        Right decoded ->
                          case decoded.message of
                            NodeOptions opt -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "NODE OPTIONS" (show opt) green)
                            Node msg ->
                              case msg of
                                BeginInput{input} ->
                                  case input of
                                    ClientInput{clientInput} ->
                                      pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "CLIENT SENT" (show clientInput) green)
                                    NetworkInput{} -> pure DropLog
                                    ChainInput{chainEvent} ->
                                      case chainEvent of
                                        Observation{observedTx} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "OBESERVATION" (show observedTx) blue)
                                        Rollback{} -> pure DropLog
                                        Tick{} -> pure DropLog
                                        PostTxError{postTxError} ->
                                          pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "ERROR" (show postTxError) red)
                                EndInput{} -> pure DropLog
                                BeginEffect{effect} ->
                                  case effect of
                                    ClientEffect{} -> pure DropLog
                                    NetworkEffect{message} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "NETWORK EFFECT" (show message) magenta)
                                    OnChainEffect{postChainTx} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "POSTING" (show postChainTx) blue)
                                EndEffect{} -> pure DropLog
                                LogicOutcome{outcome} ->
                                  case outcome of
                                    Continue{stateChanges} ->
                                      foldM
                                        ( \b a -> case a of
                                            HeadInitialized{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadInitialized" "" cyan)
                                            HeadOpened{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadOpened" "" cyan)
                                            CommittedUTxO{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "CommittedUTxO" "" cyan)
                                            HeadAborted{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadAborted" "" red)
                                            SnapshotRequestDecided{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "SnapshotRequestDecided" "" cyan)
                                            SnapshotRequested{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "SnapshotRequested" "" cyan)
                                            PartySignedSnapshot{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "PartySignedSnapshot" "" cyan)
                                            SnapshotConfirmed{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "SnapshotConfirmed" "" cyan)
                                            DepositRecorded{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DepositRecorded" "" cyan)
                                            DepositActivated{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DepositActivated" "" cyan)
                                            DepositExpired{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DepositExpired" "" red)
                                            DepositRecovered{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DepositRecovered" "" cyan)
                                            CommitApproved{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "CommitApproved" "" cyan)
                                            CommitFinalized{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "CommitFinalized" "" cyan)
                                            DecommitRecorded{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DecommitRecorded" "" cyan)
                                            DecommitApproved{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DecommitApproved" "" cyan)
                                            DecommitInvalid{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DecommitInvalid" "" red)
                                            DecommitFinalized{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "DecommitFinalized" "" cyan)
                                            HeadClosed{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadClosed" "" green)
                                            HeadContested{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadContested" "" cyan)
                                            HeadIsReadyToFanout{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadIsReadyToFanout" "" cyan)
                                            HeadFannedOut{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "HeadFannedOut" "" cyan)
                                            IgnoredHeadInitializing{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "IgnoredHeadInitializing" "" red)
                                            TxInvalid{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "TxInvalid" "" red)
                                            _ -> pure b
                                        )
                                        DropLog
                                        stateChanges
                                    Wait{} -> pure DropLog
                                    Error{error = err} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "LOGIC ERROR" (show err) red)
                                DroppedFromQueue{} -> pure DropLog
                                LoadingState -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "Loading state..." "" green)
                                LoadedState{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "Loaded." "" green)
                                ReplayingState -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "Replaying state..." "" green)
                                Misconfiguration{} -> pure $ DecodedHydraLog decoded.timestamp decoded.namespace (InfoLine "MISCONFIG!" "" red)
                            _ -> pure DropLog
          )
        .| filterC (/= DropLog)
        .| sinkList
  forM_ (sort decodedLines) $ \l ->
    render l

decodeAs :: forall a. FromJSON a => C8.ByteString -> a -> Either String a
decodeAs l _ =
  case eitherDecode' l :: Either String a of
    Left e -> Left e
    Right decoded -> pure decoded

render :: Decoded tx -> IO ()
render = \case
  DecodedHydraLog{t, n, info = InfoLine{header, line, color}} -> do
    putTextLn $ color <> unlines ["[" <> show t <> "]", "NAMESPACE:" <> show n, header, line] <> reset
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
