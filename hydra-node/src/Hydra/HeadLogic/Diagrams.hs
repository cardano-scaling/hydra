{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate diagrams of HeadLogic machines and workflows.
module Hydra.HeadLogic.Diagrams (page) where

import Crem.Topology (Topology (..))
import Data.List qualified as List (lookup)
import Data.Text qualified as Text (
  concat,
  stripEnd,
  unlines,
 )
import Hydra.HeadLogic.Diagrams.TH (machineDiagrams, stateSources)
import Hydra.HeadLogic.Rules.Chain.Lifecycle qualified as Lifecycle (workflow)
import Hydra.HeadLogic.Rules.Chain.Settlement qualified as Settlement (
  workflow,
 )
import Hydra.HeadLogic.Rules.Client.Decommit qualified as Decommit (
  workflow,
 )
import Hydra.HeadLogic.Rules.Client.Fanout qualified as Fanout (workflow)
import Hydra.HeadLogic.Rules.Client.SideLoad qualified as SideLoad (workflow)
import Hydra.HeadLogic.Rules.Decommit qualified as DecommitAdmission (workflow)
import Hydra.HeadLogic.Rules.Input qualified as Input (workflow)
import Hydra.HeadLogic.Rules.Network.AckSn qualified as AckSn (
  workflow,
 )
import Hydra.HeadLogic.Rules.Network.ReqDec qualified as ReqDec (
  workflow,
 )
import Hydra.HeadLogic.Rules.Network.ReqSn qualified as ReqSn (
  validationWorkflow,
  workflow,
 )
import Hydra.HeadLogic.Rules.Network.ReqTx qualified as ReqTx (
  workflow,
 )
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  advance,
 )
import Hydra.HeadLogic.State.Coordinated.Commits qualified as Commits (
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots qualified as Snapshots (
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.Topology qualified as SnapshotsTopology (
  transitionLabels,
 )
import Hydra.HeadLogic.State.Coordinated.Transactions qualified as Transactions (
  machine,
  transitionLabels,
 )
import Hydra.HeadLogic.State.Deposits qualified as Deposits (
  machine,
 )
import Hydra.HeadLogic.State.Head qualified as Head (machine, transitionLabels)
import Hydra.HeadLogic.State.Node qualified as Node (step)
import Hydra.Prelude

renderTopology ::
  (Bounded vertex, Enum vertex, Eq vertex, Show vertex) =>
  String ->
  String ->
  String ->
  [((vertex, vertex), String)] ->
  Topology vertex ->
  Text
renderTopology title source description labels (Topology edges) =
  "## "
    <> toText title
    <> "\n\n"
    <> toText description
    <> "\n\n"
    <> toText source
    <> "\n\n"
    <> (if null selfTransitions then "" else "↻ marks a transition that keeps the same state.\n\n")
    <> fence (Text.unlines $ "stateDiagram-v2" : nodes <> selfTransitions <> arrows)
 where
  vertices = zip [minBound .. maxBound] [0 :: Int ..]
  identifier vertex = maybe (error "Topology vertex is absent from its enumeration") (\n -> "s" <> show n) $ List.lookup vertex vertices
  nodes = ["state \"" <> (if show vertex == ("()" :: Text) then "Owned data" else show vertex) <> "\" as s" <> show n | (vertex, n) <- vertices]
  selfTransitions =
    [ identifier from <> " : ↻ " <> toText transitionLabel
    | (from, destinations) <- edges
    , from `elem` destinations
    , Just transitionLabel <- [List.lookup (from, from) labels]
    ]
  arrows =
    [ identifier from <> " --> " <> identifier to <> maybe "" ((" : " <>) . toText) (List.lookup (from, to) labels)
    | (from, destinations) <- edges
    , to <- destinations
    , from /= to
    ]

fence :: Text -> Text
fence value = "```mermaid\n" <> value <> "\n```\n\n"

primitives :: [Text]
primitives =
  $( machineDiagrams
      'renderTopology
      [ ('Head.machine, 'Head.transitionLabels)
      , ('Snapshots.machine, 'SnapshotsTopology.transitionLabels)
      , ('Transactions.machine, 'Transactions.transitionLabels)
      ]
      [('ReqSn.workflow, 'ReqSn.validationWorkflow)]
      [
        ( "Head lifecycle"
        , 'Head.machine
        , "This is the lifecycle of the Head protocol."
        )
      ,
        ( "All inputs into the head logic"
        , 'Input.workflow
        , "Client commands, network messages, and chain events."
        )
      ,
        ( "ReqTx: Receive a transaction"
        , 'ReqTx.workflow
        , "Check duplicates, outputs and deposits before selecting a snapshot or deciding whether to retry."
        )
      ,
        ( "ReqDec: Receive a decommit request"
        , 'ReqDec.workflow
        , "Check competing incremental work and transaction validity before recording a pending decommit."
        )
      ,
        ( "ReqSn: Receive a snapshot request"
        , 'ReqSn.workflow
        , "The `ValidateRequest` stage runs `validationWorkflow` to check the request\
          \, resolve incremental work, apply transactions and check commitment capacity. \
          \The remaining stages sign and acknowledge the snapshot, prune local transactions \
          \and retain the snapshot."
        )
      ,
        ( "AckSn: Receive a snapshot acknowledgement"
        , 'AckSn.workflow
        , "Collect a signature for the owned candidate; a complete certificate permits confirmation and continuation."
        )
      ,
        ( "Decommit rules"
        , 'DecommitAdmission.workflow
        , "Check availability, ledger applicability, nonempty outputs and capacity."
        )
      ,
        ( "Client decommit"
        , 'Decommit.workflow
        , "Validate a client decommit before broadcasting it."
        )
      ,
        ( "Client fanout"
        , 'Fanout.workflow
        , "Validate a selection, retain its matching intent and post the planned transaction. Chain continuations and reposts do not create new intent."
        )
      ,
        ( "Side-load a snapshot"
        , 'SideLoad.workflow
        , "Check compatibility, verify a signed snapshot, adopt and clear local work. Refusals notify the client."
        )
      ,
        ( "Observe close or contest"
        , 'Lifecycle.workflow
        , "Attempt adoption, record the observed chain fact even when verification fails, then consider contesting with a newer certificate."
        )
      ,
        ( "Observe incremental settlement"
        , 'Settlement.workflow
        , "Record the chain observation, reconcile a matching candidate and consider requesting the next snapshot."
        )
      ,
        ( "Snapshot progressing logic"
        , 'Snapshots.machine
        , "Handles requesting, accepting, collecting and confirming snapshots."
        )
      ,
        ( "Transactions on the local ledger"
        , 'Transactions.machine
        , "Handles transactions and updates the ledger; also manages decommits."
        )
      ,
        ( "Commits"
        , 'Commits.machine
        , "Handles selection of commits for inclusion."
        )
      ,
        ( "Deposits"
        , 'Deposits.machine
        , "Handles recording a deposit, and their activation or expiry."
        )
      ]
   )

page :: Either Text Text
page =
  Right $
    Text.unlines
      [ "---"
      , "title: Head logic machines"
      , "sidebar_position: 4"
      , "---"
      , ""
      , "This page is generated from the Haskell machine signatures. The overview diagrams show state ownership; arrows in the individual machine diagrams show permitted phase transitions."
      , ""
      , "A validation workflow returns a typed result to its caller for the current input; these results are not persisted state."
      , ""
      , "## Node state updates"
      , ""
      , "The node owns the Head state, deposit registry, and observed chain time and synchronisation status. Deposits remain recoverable after their Head closes. Tick inputs trigger time-dependent work in the rules."
      , ""
      , $(stateSources 'Node.step ['Head.machine, 'Deposits.machine])
      , fence $
          Text.unlines
            [ "flowchart TD"
            , "  node[\"Node state\"] --> head[\"Head lifecycle\"]"
            , "  node --> deposits[\"Deposit registry\"]"
            , "  node --> clock[\"Chain time and synchronisation\"]"
            , "  head -->|when Open| coordinated[\"Off-chain state\"]"
            ]
      , "The node applies each state-change event to the relevant components and updates chain time and synchronisation directly in NodeState. Increment settlement reads the deposited UTxO before the registry marks it consumed and passes that value to the Head."
      , ""
      , "## Off-chain state updates"
      , ""
      , "An open Head owns snapshots, local transactions and decommit reservations, and commit selection. Ordinary transaction commands go directly to the transaction machine."
      , ""
      , $(stateSources 'Coordinated.advance ['Snapshots.machine, 'Transactions.machine, 'Commits.machine])
      , fence $
          Text.unlines
            [ "flowchart TD"
            , "  coordinated[\"Off-chain state\"] --> snapshots[\"Snapshots\"]"
            , "  coordinated --> transactions[\"Local transactions and decommit\"]"
            , "  coordinated --> commits[\"Commit selection\"]"
            ]
      , "Snapshot acceptance, confirmation and adoption update the snapshot first, reconcile local transactions and decommit reservations, then update commit selection. Settlement uses the same order. Retained events restore snapshot state without repeating live admission checks."
      , ""
      , Text.stripEnd $ Text.concat primitives
      ]
