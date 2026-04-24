{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.TUI.RenderMessage (
  RenderedMessage (..),
  renderMessage,
  toLogMessage,
) where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Hydra.API.ServerOutput (
  ClientMessage (..),
  DecommitInvalidReason (..),
  Greetings (..),
  InvalidInput (..),
  ServerOutput (..),
  TimedServerOutput (..),
 )
import Hydra.Cardano.Api (Tx, UTxO)
import Hydra.Cardano.Api.Pretty (renderUTxO)
import Hydra.Chain (PostTxError (InternalWalletError, NotEnoughFuel), reason)
import Hydra.Client (AllPossibleAPIMessages (..))
import Hydra.Tx (IsTx (..), Snapshot (..))
import Hydra.TUI.Logging.Types (LogMessage (..), Severity (..))

data RenderedMessage = RenderedMessage
  { rmSeverity :: Severity
  , rmTime :: UTCTime
  , rmSummary :: Text
  , rmDetail :: Text
  , rmRawJson :: Text
  }

-- | Convert a 'RenderedMessage' to a 'LogMessage'.
toLogMessage :: RenderedMessage -> LogMessage
toLogMessage RenderedMessage{rmSeverity, rmTime, rmSummary, rmDetail, rmRawJson} =
  LogMessage
    { severity = rmSeverity
    , message = rmSummary
    , detail = rmDetail
    , time = rmTime
    , rawJson = rmRawJson
    }

-- | Render any API message into its three representations: summary, detail, raw JSON.
renderMessage :: UTCTime -> AllPossibleAPIMessages Tx -> RenderedMessage
renderMessage now = \case
  ApiTimedServerOutput tso@TimedServerOutput{time, output} ->
    renderServerOutput time output (encodeJson tso)
  ApiClientMessage msg ->
    renderClientMessage now msg (encodeJson msg)
  ApiGreetings g ->
    renderGreetings now g (encodeJson g)
  ApiInvalidInput ii ->
    renderInvalidInput now ii (encodeJson ii)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

encodeJson :: ToJSON a => a -> Text
encodeJson = TL.toStrict . TLE.decodeUtf8 . encodePretty . toJSON

mk :: Severity -> UTCTime -> Text -> [Text] -> Text -> RenderedMessage
mk sev t summary detailLines raw =
  RenderedMessage
    { rmSeverity = sev
    , rmTime = t
    , rmSummary = summary
    , rmDetail = T.intercalate "\n" detailLines
    , rmRawJson = raw
    }

fld :: Text -> Text -> Text
fld lbl val = lbl <> ": " <> val

-- ---------------------------------------------------------------------------
-- ServerOutput (29 constructors)
-- ---------------------------------------------------------------------------

renderServerOutput :: UTCTime -> ServerOutput Tx -> Text -> RenderedMessage
renderServerOutput time output raw = case output of
  NetworkConnected ->
    mk Success time "Network connected" ["Network connected successfully."] raw
  NetworkDisconnected ->
    mk Error time "Network disconnected" ["Lost connection to the network."] raw
  NetworkVersionMismatch{ourVersion, theirVersion} ->
    mk
      Error
      time
      "Network version mismatch"
      [ fld "Our version" (show ourVersion)
      , fld "Their version" (maybe "(unknown)" show theirVersion)
      , "Check that all nodes run compatible versions."
      ]
      raw
  NetworkClusterIDMismatch{clusterPeers, misconfiguredPeers} ->
    mk
      Error
      time
      "Network cluster ID mismatch"
      [ fld "Cluster peers" clusterPeers
      , fld "Misconfigured peers" misconfiguredPeers
      ]
      raw
  PeerConnected{peer} ->
    mk Info time ("Peer connected: " <> show peer) [fld "Peer" (show peer)] raw
  PeerDisconnected{peer} ->
    mk Error time ("Peer disconnected: " <> show peer) [fld "Peer" (show peer)] raw
  HeadIsOpen{headId, parties} ->
    mk
      Success
      time
      "Head is open"
      [ fld "Head ID" (show headId)
      , fld "Parties" (show (length parties))
      ]
      raw
  HeadIsClosed{headId, snapshotNumber, contestationDeadline} ->
    mk
      Info
      time
      ("Head closed at snapshot " <> show snapshotNumber)
      [ fld "Head ID" (show headId)
      , fld "Snapshot" (show snapshotNumber)
      , fld "Contestation deadline" (show contestationDeadline)
      , "Submit a contest transaction before the deadline to challenge."
      ]
      raw
  HeadIsContested{headId, snapshotNumber, contestationDeadline} ->
    mk
      Info
      time
      ("Head contested at snapshot " <> show snapshotNumber)
      [ fld "Head ID" (show headId)
      , fld "Snapshot" (show snapshotNumber)
      , fld "Contestation deadline" (show contestationDeadline)
      ]
      raw
  ReadyToFanout{headId} ->
    mk
      Success
      time
      "Ready to fan out"
      [ fld "Head ID" (show headId)
      , "Contestation period has passed. You can now submit a fanout transaction."
      ]
      raw
  HeadIsFinalized{headId, utxo} ->
    mk
      Success
      time
      "Head finalized"
      [ fld "Head ID" (show headId)
      , fld "Distributed UTxO" (utxoSummary utxo)
      ]
      raw
  TxValid{headId, transactionId} ->
    mk
      Success
      time
      ("Transaction " <> show transactionId <> " valid")
      [ fld "Head ID" (show headId)
      , fld "Transaction ID" (show transactionId)
      , "Transaction accepted and will be included in the next snapshot."
      ]
      raw
  TxInvalid{headId, transaction, validationError} ->
    mk
      Error
      time
      ("Transaction " <> show (txId transaction) <> " invalid")
      [ fld "Head ID" (show headId)
      , fld "Transaction ID" (show (txId transaction))
      , fld "Validation error" (show validationError)
      ]
      raw
  SnapshotConfirmed{headId, snapshot = Snapshot{number, version}} ->
    mk
      Info
      time
      ("Snapshot #" <> show number <> "." <> show version <> " confirmed")
      [ fld "Head ID" (show headId)
      , fld "Snapshot number" (show number)
      , fld "Version" (show version)
      ]
      raw
  IgnoredHeadInitializing{headId, contestationPeriod, parties, participants} ->
    mk
      Info
      time
      "Ignored head initializing"
      [ fld "Head ID" (show headId)
      , fld "Contestation period" (show contestationPeriod)
      , fld "Parties" (show (length parties))
      , fld "Participants" (show (length participants))
      , "This node did not participate in the head initialization."
      ]
      raw
  DecommitRequested{headId, decommitTx, utxoToDecommit} ->
    mk
      Info
      time
      ("Decommit requested: " <> show (txId decommitTx))
      [ fld "Head ID" (show headId)
      , fld "Decommit tx" (show (txId decommitTx))
      , fld "UTxO to decommit" (utxoSummary utxoToDecommit)
      ]
      raw
  DecommitInvalid{headId, decommitTx, decommitInvalidReason} ->
    mk
      Error
      time
      ("Decommit invalid: " <> show (txId decommitTx))
      [ fld "Head ID" (show headId)
      , fld "Decommit tx" (show (txId decommitTx))
      , fld "Reason" (renderDecommitInvalidReason decommitInvalidReason)
      ]
      raw
  DecommitApproved{headId, decommitTxId, utxoToDecommit} ->
    mk
      Success
      time
      ("Decommit approved: " <> show decommitTxId)
      [ fld "Head ID" (show headId)
      , fld "Decommit tx" (show decommitTxId)
      , fld "UTxO to decommit" (utxoSummary utxoToDecommit)
      ]
      raw
  DecommitFinalized{headId, distributedUTxO} ->
    mk
      Success
      time
      "Decommit finalized"
      [ fld "Head ID" (show headId)
      , fld "Distributed UTxO" (utxoSummary distributedUTxO)
      ]
      raw
  CommitRecorded{headId, utxoToCommit, pendingDeposit, deadline} ->
    mk
      Success
      time
      ("Deposit recorded: " <> show pendingDeposit)
      [ fld "Head ID" (show headId)
      , fld "Deposit tx ID" (show pendingDeposit)
      , fld "UTxO to commit" (utxoSummary utxoToCommit)
      , fld "Deadline" (show deadline)
      , "Waiting for approval before funds enter the head."
      ]
      raw
  DepositActivated{headId, depositTxId, deadline, chainTime} ->
    mk
      Info
      time
      ("Deposit activated: " <> show depositTxId)
      [ fld "Head ID" (show headId)
      , fld "Deposit tx ID" (show depositTxId)
      , fld "Deadline" (show deadline)
      , fld "Chain time" (show chainTime)
      ]
      raw
  DepositExpired{headId, depositTxId, deadline, chainTime} ->
    mk
      Error
      time
      ("Deposit expired: " <> show depositTxId)
      [ fld "Head ID" (show headId)
      , fld "Deposit tx ID" (show depositTxId)
      , fld "Deadline" (show deadline)
      , fld "Chain time" (show chainTime)
      , "The deposit was not approved in time and has expired."
      ]
      raw
  CommitApproved{headId, utxoToCommit} ->
    mk
      Success
      time
      "Commit approved"
      [ fld "Head ID" (show headId)
      , fld "UTxO committed" (T.intercalate ", " (renderUTxO <$> UTxO.toList utxoToCommit))
      ]
      raw
  CommitFinalized{headId, depositTxId} ->
    mk
      Success
      time
      ("Commit finalized: " <> show depositTxId)
      [ fld "Head ID" (show headId)
      , fld "Deposit tx ID" (show depositTxId)
      , "Funds are now in the head."
      ]
      raw
  CommitRecovered{headId, recoveredUTxO, recoveredTxId} ->
    mk
      Info
      time
      ("Commit recovered: " <> show recoveredTxId)
      [ fld "Head ID" (show headId)
      , fld "Recovered tx ID" (show recoveredTxId)
      , fld "Recovered UTxO" (utxoSummary recoveredUTxO)
      , "The pending deposit was recovered back to L1."
      ]
      raw
  SnapshotSideLoaded{headId, snapshotNumber} ->
    mk
      Info
      time
      ("Snapshot #" <> show snapshotNumber <> " side-loaded")
      [ fld "Head ID" (show headId)
      , fld "Snapshot number" (show snapshotNumber)
      , "Local state reset; pending transactions pruned."
      ]
      raw
  EventLogRotated{} ->
    mk Info time "Event log rotated (checkpoint)" ["Head state event log checkpoint triggered."] raw
  NodeUnsynced{chainSlot, chainTime, drift} ->
    mk
      Error
      time
      "Node out of sync"
      [ fld "Chain slot" (show chainSlot)
      , fld "Chain time" (show chainTime)
      , fld "Drift" (show drift)
      , "Node state is behind the chain backend. Transactions will be rejected."
      ]
      raw
  NodeSynced{chainSlot, chainTime, drift} ->
    mk
      Info
      time
      "Node back in sync"
      [ fld "Chain slot" (show chainSlot)
      , fld "Chain time" (show chainTime)
      , fld "Drift" (show drift)
      ]
      raw

-- ---------------------------------------------------------------------------
-- ClientMessage (5 constructors)
-- ---------------------------------------------------------------------------

renderClientMessage :: UTCTime -> ClientMessage Tx -> Text -> RenderedMessage
renderClientMessage now msg raw = case msg of
  CommandFailed{clientInput} ->
    mk
      Error
      now
      ("Invalid command: " <> show clientInput)
      [ fld "Command" (show clientInput)
      , "This command is not valid in the current head state."
      ]
      raw
  PostTxOnChainFailed{postTxError} ->
    mk
      Error
      now
      "On-chain transaction failed"
      (renderPostTxError postTxError)
      raw
  RejectedInputBecauseUnsynced{clientInput, drift} ->
    mk
      Error
      now
      "Command rejected: node not synced"
      [ fld "Command" (show clientInput)
      , fld "Sync drift" (show drift)
      , "Wait for the node to catch up with the chain before retrying."
      ]
      raw
  SideLoadSnapshotRejected{clientInput, requirementFailure} ->
    mk
      Error
      now
      "Side-load snapshot rejected"
      [ fld "Command" (show clientInput)
      , fld "Failure" (show requirementFailure)
      ]
      raw
  SyncedStatusReport{chainSlot, chainTime, drift, synced} ->
    mk
      Info
      now
      ("Sync status: " <> show synced)
      [ fld "Chain slot" (show chainSlot)
      , fld "Chain time" (show chainTime)
      , fld "Drift" (show drift)
      , fld "Synced" (show synced)
      ]
      raw

-- ---------------------------------------------------------------------------
-- Greetings
-- ---------------------------------------------------------------------------

renderGreetings :: UTCTime -> Greetings Tx -> Text -> RenderedMessage
renderGreetings now Greetings{me, headStatus, hydraHeadId, hydraNodeVersion, chainSyncedStatus} raw =
  mk
    Info
    now
    ("Connected to hydra-node " <> toText hydraNodeVersion)
    ( [ fld "Node version" (toText hydraNodeVersion)
      , fld "Party" (show me)
      , fld "Head status" (show headStatus)
      , fld "Chain sync" (show chainSyncedStatus)
      ]
        <> maybe [] (\hid -> [fld "Head ID" (show hid)]) hydraHeadId
    )
    raw

-- ---------------------------------------------------------------------------
-- InvalidInput
-- ---------------------------------------------------------------------------

renderInvalidInput :: UTCTime -> InvalidInput -> Text -> RenderedMessage
renderInvalidInput now InvalidInput{reason, input} raw =
  mk
    Error
    now
    "Invalid input"
    [ fld "Reason" (toText reason)
    , fld "Input" input
    ]
    raw

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

utxoSummary :: UTxO -> Text
utxoSummary u =
  let n = UTxO.size u
   in show n <> if n == 1 then " entry" else " entries"

renderDecommitInvalidReason :: DecommitInvalidReason Tx -> Text
renderDecommitInvalidReason = \case
  DecommitTxInvalid{validationError} -> "Transaction invalid: " <> show validationError
  DecommitAlreadyInFlight{otherDecommitTxId} -> "Another decommit already in flight: " <> show otherDecommitTxId

renderPostTxError :: PostTxError Tx -> [Text]
renderPostTxError = \case
  NotEnoughFuel _ ->
    [ "Not enough fuel."
    , "Add ADA to the internal wallet and try again."
    ]
  InternalWalletError{reason} ->
    [ fld "Internal wallet error" reason
    ]
  err ->
    [ fld "Error" (show err)
    ]
