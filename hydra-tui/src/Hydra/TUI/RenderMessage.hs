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
import Hydra.Cardano.Api hiding (Active, txId)
import Hydra.Chain (PostTxError (..), failureReason, reason, redeemerPtr)
import Hydra.Client (AllPossibleAPIMessages (..))
import Hydra.TUI.Drawing.Utils (prettyHeadId, prettyTxId)
import Hydra.TUI.Logging.Types (LogMessage (..), Severity (..))
import Hydra.Tx (Snapshot (..), txId)

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
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Parties" (show (length parties))
      ]
      raw
  HeadIsClosed{headId, snapshotNumber, contestationDeadline} ->
    mk
      Info
      time
      ("Head closed at snapshot " <> show snapshotNumber)
      [ fld "Head ID" (prettyHeadId headId)
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
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Snapshot" (show snapshotNumber)
      , fld "Contestation deadline" (show contestationDeadline)
      ]
      raw
  ReadyToFanout{headId} ->
    mk
      Success
      time
      "Ready to fan out"
      [ fld "Head ID" (prettyHeadId headId)
      , "Contestation period has passed. You can now submit a fanout transaction."
      ]
      raw
  HeadIsFinalized{headId, utxo} ->
    mk
      Success
      time
      "Head finalized"
      [ fld "Head ID" (prettyHeadId headId)
      , utxoBlock "Distributed UTxO" utxo
      ]
      raw
  TxValid{headId, transactionId} ->
    mk
      Success
      time
      ("Transaction " <> prettyTxId transactionId <> " valid")
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Transaction ID" (prettyTxId transactionId)
      , "Transaction accepted and will be included in the next snapshot."
      ]
      raw
  TxInvalid{headId, transaction, validationError} ->
    mk
      Error
      time
      ("Transaction " <> prettyTxId (txId transaction) <> " invalid")
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Transaction ID" (prettyTxId (txId transaction))
      , fld "Validation error" (show validationError)
      ]
      raw
  SnapshotConfirmed{headId, snapshot = Snapshot{number, version, confirmed}} ->
    let txCount = length confirmed
        countLabel
          | txCount == 0 = " (empty)"
          | txCount == 1 = " (1 transaction)"
          | otherwise = " (" <> show txCount <> " transactions)"
     in mk
          Info
          time
          ("Snapshot #" <> show number <> "." <> show version <> " confirmed" <> countLabel)
          ( [ fld "Head ID" (prettyHeadId headId)
            , fld "Snapshot number" (show number)
            , fld "Version" (show version)
            , fld "Transactions" (if txCount == 0 then "none" else show txCount)
            ]
              <> ((\tx -> "  " <> prettyTxId (txId tx)) <$> confirmed)
          )
          raw
  IgnoredHeadInitializing{headId, contestationPeriod, parties, participants} ->
    mk
      Info
      time
      "Ignored head initializing"
      [ fld "Head ID" (prettyHeadId headId)
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
      ("Decommit requested: " <> prettyTxId (txId decommitTx))
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Decommit tx" (prettyTxId (txId decommitTx))
      , utxoBlock "UTxO to decommit" utxoToDecommit
      ]
      raw
  DecommitInvalid{headId, decommitTx, decommitInvalidReason} ->
    mk
      Error
      time
      ("Decommit invalid: " <> prettyTxId (txId decommitTx))
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Decommit tx" (prettyTxId (txId decommitTx))
      , fld "Reason" (renderDecommitInvalidReason decommitInvalidReason)
      ]
      raw
  DecommitApproved{headId, decommitTxId, utxoToDecommit} ->
    mk
      Success
      time
      ("Decommit approved: " <> prettyTxId decommitTxId)
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Decommit tx" (prettyTxId decommitTxId)
      , utxoBlock "UTxO to decommit" utxoToDecommit
      ]
      raw
  DecommitFinalized{headId, distributedUTxO} ->
    mk
      Success
      time
      "Decommit finalized"
      [ fld "Head ID" (prettyHeadId headId)
      , utxoBlock "Distributed UTxO" distributedUTxO
      ]
      raw
  CommitRecorded{headId, utxoToCommit, pendingDeposit, deadline} ->
    mk
      Success
      time
      ("Deposit recorded: " <> prettyTxId pendingDeposit)
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Deposit tx ID" (prettyTxId pendingDeposit)
      , fld "Deadline" (show deadline)
      , utxoBlock "UTxO to commit" utxoToCommit
      , "Waiting for approval before funds enter the head."
      ]
      raw
  DepositActivated{headId, depositTxId, deadline, chainTime} ->
    mk
      Info
      time
      ("Deposit activated: " <> prettyTxId depositTxId)
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Deposit tx ID" (prettyTxId depositTxId)
      , fld "Deadline" (show deadline)
      , fld "Chain time" (show chainTime)
      ]
      raw
  DepositExpired{headId, depositTxId, deadline, chainTime} ->
    mk
      Error
      time
      ("Deposit expired: " <> prettyTxId depositTxId)
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Deposit tx ID" (prettyTxId depositTxId)
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
      [ fld "Head ID" (prettyHeadId headId)
      , utxoBlock "UTxO committed" utxoToCommit
      ]
      raw
  CommitFinalized{headId, depositTxId} ->
    mk
      Success
      time
      ("Commit finalized: " <> prettyTxId depositTxId)
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Deposit tx ID" (prettyTxId depositTxId)
      , "Funds are now in the head."
      ]
      raw
  CommitRecovered{headId, recoveredUTxO, recoveredTxId} ->
    mk
      Info
      time
      ("Commit recovered: " <> prettyTxId recoveredTxId)
      [ fld "Head ID" (prettyHeadId headId)
      , fld "Recovered tx ID" (prettyTxId recoveredTxId)
      , utxoBlock "Recovered UTxO" recoveredUTxO
      , "The pending deposit was recovered back to L1."
      ]
      raw
  SnapshotSideLoaded{headId, snapshotNumber} ->
    mk
      Info
      time
      ("Snapshot #" <> show snapshotNumber <> " side-loaded")
      [ fld "Head ID" (prettyHeadId headId)
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
renderGreetings now Greetings{me, headStatus, hydraHeadId, hydraNodeVersion, chainSyncedStatus} =
  mk
    Info
    now
    ("Connected to hydra-node " <> toText hydraNodeVersion)
    ( [ fld "Node version" (toText hydraNodeVersion)
      , fld "Party" (show me)
      , fld "Head status" (show headStatus)
      , fld "Chain sync" (show chainSyncedStatus)
      ]
        <> maybe [] (\hid -> [fld "Head ID" (prettyHeadId hid)]) hydraHeadId
    )

-- ---------------------------------------------------------------------------
-- InvalidInput
-- ---------------------------------------------------------------------------

renderInvalidInput :: UTCTime -> InvalidInput -> Text -> RenderedMessage
renderInvalidInput now InvalidInput{reason, input} =
  mk
    Error
    now
    "Invalid input"
    [ fld "Reason" (toText reason)
    , fld "Input" input
    ]

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Render a UTxO map as a labelled block: "Label:\n  txin ↦ ₳ X.XXXXXX\n  ...".
-- Embeds newlines so it can be used as a single item in a detail-lines list.
utxoBlock :: Text -> UTxO -> Text
utxoBlock lbl u
  | null entries = lbl <> ": (none)"
  | otherwise = T.intercalate "\n" $ (lbl <> ":") : (("  " <>) . renderUTxOEntry <$> entries)
 where
  entries = UTxO.toList u

-- | Render a UTxO entry as "txin ↦ ₳ X.XXXXXX" (full txin, ADA-denominated).
renderUTxOEntry :: (TxIn, TxOut CtxUTxO) -> Text
renderUTxOEntry (txin, TxOut _ val _ _) =
  let Coin l = selectLovelace val
      (ada, frac) = abs l `divMod` 1_000_000
      fracStr = show frac
      padded = T.replicate (6 - length fracStr) "0" <> T.pack fracStr
      sign = if l < 0 then "-" else ""
   in renderTxIn txin <> " ↦ ₳ " <> sign <> T.pack (show ada) <> "." <> padded

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
  ScriptFailedInWallet{redeemerPtr, failureReason} ->
    [ "Script execution failed in wallet."
    , fld "Redeemer" redeemerPtr
    , fld "Reason" failureReason
    ]
  FailedToPostTx{failureReason} ->
    [ "Failed to submit transaction to the chain."
    , fld "Reason" failureReason
    ]
  FailedToConstructDepositTx{failureReason} ->
    [ "Failed to construct deposit transaction."
    , fld "Reason" failureReason
    ]
  FailedToConstructRecoverTx{failureReason} ->
    [ "Failed to construct recover transaction."
    , fld "Reason" failureReason
    ]
  FailedToConstructIncrementTx{failureReason} ->
    [ "Failed to construct increment transaction."
    , fld "Reason" failureReason
    ]
  FailedToConstructDecrementTx{failureReason} ->
    [ "Failed to construct decrement transaction."
    , fld "Reason" failureReason
    ]
  FailedToConstructCloseTx ->
    [ "Failed to construct close transaction."
    ]
  FailedToConstructContestTx ->
    [ "Failed to construct contest transaction."
    ]
  FailedToConstructFanoutTx ->
    [ "Failed to construct fanout transaction."
    ]
  err ->
    [ fld "On-chain error" (show err)
    ]
