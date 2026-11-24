{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Documentation generated from the same topology types and compositions that
-- execute HeadLogic. No machine input is required to render a primitive graph.
module Hydra.HeadLogic.Diagrams (page) where

import Crem.Render.Render (
  MachineLabel,
  Mermaid (..),
  renderErrorMessage,
 )
import Crem.Render.RenderFlow (TreeMetadata (..), renderFlow)
import Crem.StateMachine (StateMachineT)
import Crem.Topology (Topology (..))
import Data.List qualified as List (lookup)
import Data.Text qualified as Text (
  concat,
  replace,
  stripEnd,
  unlines,
 )
import Hydra.HeadLogic.Diagrams.TH (compositionSources, machineDiagrams)
import Hydra.HeadLogic.Outcome (StateChanged (HeadOpened))
import Hydra.HeadLogic.Rules.Chain.Settlement qualified as Settlement (
  workflow,
 )
import Hydra.HeadLogic.Rules.Client.Decommit qualified as Decommit (
  workflow,
 )
import Hydra.HeadLogic.Rules.Input qualified as Input (workflow)
import Hydra.HeadLogic.Rules.Network.AckSn qualified as AckSn (
  workflow,
 )
import Hydra.HeadLogic.Rules.Network.ReqDec qualified as ReqDec (
  workflow,
 )
import Hydra.HeadLogic.Rules.Network.ReqSn qualified as ReqSn (
  workflow,
 )
import Hydra.HeadLogic.Rules.Network.ReqTx qualified as ReqTx (
  workflow,
 )
import Hydra.HeadLogic.State qualified as State (HeadState (Open))
import Hydra.HeadLogic.State.Clock qualified as Clock (machine)
import Hydra.HeadLogic.State.Coordinated qualified as Coordinated (
  components,
 )
import Hydra.HeadLogic.State.Coordinated.Commits qualified as Commits (
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots qualified as Snapshots (
  machine,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.Topology qualified as SnapshotsTopology (transitionLabels)
import Hydra.HeadLogic.State.Coordinated.Transactions qualified as Transactions (
  machine,
  transitionLabels,
 )
import Hydra.HeadLogic.State.Deposits qualified as Deposits (
  machine,
 )
import Hydra.HeadLogic.State.Head qualified as Head (machine, transitionLabels)
import Hydra.HeadLogic.State.Node qualified as Node (
  apply,
  components,
 )
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.State (NodeState (headState), initNodeState)
import Hydra.Prelude
import Hydra.Tx (
  HeadId (UnsafeHeadId),
  HeadParameters (..),
  HeadSeed (UnsafeHeadSeed),
 )

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
    <> fence (Text.unlines $ "stateDiagram-v2" : nodes <> arrows)
 where
  vertices = zip [minBound .. maxBound] [0 :: Int ..]
  identifier vertex = maybe (error "Topology vertex is absent from its enumeration") (\n -> "s" <> show n) $ List.lookup vertex vertices
  nodes = ["state \"" <> (if show vertex == ("()" :: Text) then "Owned data" else show vertex) <> "\" as s" <> show n | (vertex, n) <- vertices]
  arrows =
    [ identifier from <> " --> " <> identifier to <> maybe "" ((" : " <>) . toText) (List.lookup (from, to) labels)
    | (from, destinations) <- edges
    , to <- destinations
    , from /= to || isJust (List.lookup (from, to) labels)
    ]

fence :: Text -> Text
fence value = "```mermaid\n" <> value <> "\n```\n\n"

primitives :: [Text]
primitives =
  $( machineDiagrams
      'renderTopology
      [('Head.machine, 'Head.transitionLabels), ('Snapshots.machine, 'SnapshotsTopology.transitionLabels), ('Transactions.machine, 'Transactions.transitionLabels)]
      [ ("Input admission and dispatch", 'Input.workflow, "Each input starts a fresh workflow. Admission checks precede dispatch by head state.")
      , ("Receive a transaction", 'ReqTx.workflow, "Check duplicates, outputs and deposits before selecting a snapshot or deciding whether to retry.")
      , ("Receive a decommit request", 'ReqDec.workflow, "Check competing incremental work and transaction validity before recording a pending decommit.")
      , ("Receive a snapshot request", 'ReqSn.workflow, "Resolve the requested incremental work and transactions before building, signing and retaining the candidate.")
      , ("Receive a snapshot acknowledgement", 'AckSn.workflow, "Collect a signature for the owned candidate; a complete certificate permits confirmation and continuation.")
      , ("Client decommit", 'Decommit.workflow, "Validate a client decommit before broadcasting it. Recording and approval occur later.")
      , ("Observe incremental settlement", 'Settlement.workflow, "Record the chain observation, reconcile a matching candidate and consider requesting the next snapshot.")
      , ("Head lifecycle", 'Head.machine, "This is the lifecycle of one Head identity. Contest updates a Closed Head's snapshot and deadline while staying Closed. A local fanout request and its cancellation also leave that protocol phase unchanged; only an observed partial distribution enters PartialFanout. Further partial distributions stay there. Full or final partial fanout reaches Finalized, which is terminal and survives checkpoint recovery. Starting a different Head creates a new machine at the node boundary. Named self-loops identify increment, decrement, contest and repeated partial distribution.")
      , ("Snapshot ownership", 'Snapshots.machine, "Normal request processing cannot restart a round while collecting signatures. Confirmation and certificate adoption finish it; administrative side-loading can instead discard the candidate and reset the seen number to the confirmed number. This administrative escape hatch is outside the Agda signing-discipline theorem. Retained-event restoration is separate from both operations.")
      , ("Local transactions and decommit ownership", 'Transactions.machine, "Local transactions and reserved decommit outputs share an owner. An approved decommit awaiting settlement cannot be replaced by a pending one. NoDecommit can move directly to AwaitingDecommitSettlement when a confirmed or adopted snapshot carries a decommit whose ReqDec this node has not received.")
      , ("Commit selection owner", 'Commits.machine, "A single Crem state contains Unselected, Selected or Approved data. Approval and the optional selected deposit are independent. Every pair of these data states is reachable, so a phase graph would add no restriction.")
      , ("Deposit registry owner", 'Deposits.machine, "A single Crem state owns the registry. Each entry has independent eligibility, recording and consumption fields. Ordinary reducers handle observation, status updates, consumption and rollback. This is an ownership graph, not the lifecycle of one deposit; see the [deposit lifecycle](../deposit-lifecycle.svg).")
      , ("Node clock owner", 'Clock.machine, "A single Crem state owns synchronization status and the observed chain clock; the diagram expresses ownership, not a protocol lifecycle.")
      ]
   )

page :: Either Text Text
page = do
  let initial = initNodeState @SimpleTx 0
      opened = Node.apply initial $ HeadOpened (HeadParameters 1 1 []) 0 (UnsafeHeadId mempty) (UnsafeHeadSeed mempty) []
  open <- case opened.headState of
    State.Open value -> Right value
    _ -> Left "Diagram fixture did not open the Head"
  let labels :: a -> a -> a -> TreeMetadata a
      labels a b c = BinaryLabel (BinaryLabel (LeafLabel a) (LeafLabel b)) (LeafLabel c)
      render :: Text -> String -> TreeMetadata MachineLabel -> StateMachineT m input output -> Either Text Text
      render title sources metadata machine = do
        (Mermaid graph, _, _) <- first (toText . renderErrorMessage) $ renderFlow metadata machine
        pure $ "## " <> title <> "\n\n" <> toText sources <> "\n\n" <> fence ("stateDiagram-v2\n" <> Text.replace "_()" "_State" graph)
  node <- render "Retained-event composition" $(compositionSources 'Node.components ['Deposits.machine, 'Head.machine, 'Clock.machine]) (labels "Deposits" "Head" "Clock") $ Node.components initial
  coordinated <- render "Open-head composition" $(compositionSources 'Coordinated.components ['Snapshots.machine, 'Transactions.machine, 'Commits.machine]) (labels "Snapshots" "Transactions" "Commits") $ Coordinated.components open
  pure $
    Text.unlines
      [ "---"
      , "title: Head logic machines"
      , "sidebar_position: 4"
      , "---"
      , ""
      , "This page is generated during the documentation build from the Haskell machine signatures and compositions."
      , node
      , coordinated
      , Text.stripEnd $ Text.concat primitives
      ]
