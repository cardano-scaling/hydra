{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Generate diagrams of all the Crem machines.
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
      [ ('Head.machine, 'Head.transitionLabels)
      , ('Snapshots.machine, 'SnapshotsTopology.transitionLabels)
      , ('Transactions.machine, 'Transactions.transitionLabels)
      ]
      [
        ( "Head lifecycle"
        , 'Head.machine
        , "This is the lifecycle of the Head protocol."
        )
      ,
        ( "All inputs into the head logic"
        , 'Input.workflow
        , "Client commands, network messages, and chain events.
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
        , "Resolve any incremental work and transactions before building, signing and retaining the snapshot."
        )
      ,
        ( "AckSn: Receive a snapshot acknowledgement"
        , 'AckSn.workflow
        , "Collect a signature for the owned candidate; a complete certificate permits confirmation and continuation."
        )
      , ("Client decommit"
        , 'Decommit.workflow
        , "Validate a client decommit before broadcasting it."
        )
      , ("Observe incremental settlement"
        , 'Settlement.workflow
        , "Record the chain observation, reconcile a matching candidate and consider requesting the next snapshot."
        )
      ,
        ( "Snapshot progressing logic"
        , 'Snapshots.machine
        , "Handles requesting, accepting, collecting and confirming snapshots."
        )
      ,
        ( "Transactions on the local ledger."
        , 'Transactions.machine
        , "Handles transactions and updates the ledger; also manages decommits."
        )
      ,
        ( "Commit selection owner"
        , 'Commits.machine
        , "Handles selection of commits for inclusion."
        )
      ,
        ( "Deposit registry owner"
        , 'Deposits.machine
        , "Handles recording a deposit, and their activation or expiry."
        )
      ,
        ( "Node clock owner"
        , 'Clock.machine
        , "Handles what happens on Tick events."
        )
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
  node <-
    render
      "Retained-event composition"
      $(compositionSources 'Node.components ['Deposits.machine, 'Head.machine, 'Clock.machine])
      (labels "Deposits" "Head" "Clock")
      $ Node.components initial
  coordinated <-
    render
      "Open-head composition"
      $(compositionSources 'Coordinated.components ['Snapshots.machine, 'Transactions.machine, 'Commits.machine])
      (labels "Snapshots" "Transactions" "Commits")
      $ Coordinated.components open
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
