{-# LANGUAGE OverloadedRecordDot #-}

-- | Development-only rendering of the actual crem definitions.
module Test.Hydra.HeadLogic.MachineDiagrams (writeDiagrams) where

import Hydra.Prelude

import Crem.Render.Render (Mermaid (..), baseMachineAsGraph, renderStateDiagram)
import Crem.Render.RenderFlow (TreeMetadata (..), renderFlow)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hydra.HeadLogic.Machine qualified as Machine
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Machine.Deposit qualified as Deposit
import Hydra.HeadLogic.Machine.Lifecycle qualified as Lifecycle
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.State (NodeState (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hydra.HeadLogic.Scenario (initialNode, openState)

writeDiagrams :: FilePath -> IO ()
writeDiagrams directory = do
  createDirectoryIfMissing True directory
  (flow, _, _) <- either fail pure $ renderFlow labels (Machine.components initialNode)
  (coordinated, _, _) <- either fail pure $ renderFlow (BinaryLabel (LeafLabel "Snapshots") (LeafLabel "Transactions")) (Coordinated.components $ openState initialNode)
  let diagrams =
        [ ("coordinated", "stateDiagram-v2\n" <> getText coordinated)
        , ("composition", annotateLifecycle "Lifecycle_" $ nameDataOwners $ "stateDiagram-v2\n" <> getText flow)
        , ("lifecycle", annotateLifecycle "" $ getText $ renderStateDiagram $ baseMachineAsGraph $ Lifecycle.machine @SimpleTx @Identity initialNode.headState)
        , ("deposits", getText $ renderStateDiagram $ baseMachineAsGraph $ Deposit.machine @Identity @SimpleTx Nothing)
        ]
  forM_ diagrams $ \(name, diagram) -> Text.writeFile (directory </> name <> ".mmd") (Text.stripEnd diagram <> "\n")
  Text.writeFile (directory </> "README.md") $
    "# Head state machines\n\nGenerated from crem's executable definitions with:\n\n```sh\ncabal run hydra-node:head-logic-lab -- machines "
      <> toText directory
      <> "\n```\n\n"
      <> "Coordinated contains the snapshot and transaction owners used by live staging and replay. Its Reserved phase records a pending decommit; it does not prove that rebuilding preserved the reservation. Composition shows DepositRegistry → Lifecycle → Clock. The open lifecycle state holds the Coordinated continuation; its nested composition is shown separately. These are allowed replay transitions, including resets and events outside their usual phase. Edges to Idle mean observed completion (HeadFannedOut), including at parties that did not initiate fanout. Identity transitions are implicit. They do not show admission guards or prove that every edge is reachable from network input. See [the implementation guide](../head-logic-crem.md) for event meanings and limitations.\n"
      <> foldMap (\(name, diagram) -> "\n## " <> toText name <> "\n\n```mermaid\n" <> diagram <> "\n```\n") diagrams
 where
  labels = BinaryLabel (BinaryLabel (LeafLabel "DepositRegistry") (LeafLabel "Lifecycle")) (LeafLabel "Clock")

-- Crem renders a trivial topology's vertex as (). Use readable Mermaid ids;
-- these owners carry data, with no additional phase distinction.
nameDataOwners :: Text -> Text
nameDataOwners = Text.replace "DepositRegistry_()" "DepositRegistry_TrackedDeposits" . Text.replace "Clock_()" "Clock_ObservedTime"

-- Event captions explain lifecycle edges already supplied by crem; they never
-- add or remove edges. The other graphs describe phase constraints only.
annotateLifecycle :: Text -> Text -> Text
annotateLifecycle prefix = Text.unlines . map annotate . Text.lines
 where
  annotate line = case Text.words line of
    [source, "-->", target]
      | Just from <- Text.stripPrefix prefix source
      , Just to <- Text.stripPrefix prefix target
      , Just caption <- eventCaption from to ->
          line <> ": " <> caption
    _ -> line
  eventCaption :: Text -> Text -> Maybe Text
  eventCaption from to = case (from, to) of
    (_, "Idle") -> Just "HeadFannedOut (observed on chain)"
    ("Idle", "Open") -> Just "HeadOpened (Init observed)"
    (_, "Open") -> Just "HeadOpened (replay reset)"
    ("Open", "Closed") -> Just "HeadClosed"
    ("Closed", "ReadyToFanout") -> Just "HeadIsReadyToFanout"
    (_, "ReadyToFanout") -> Just "HeadFanoutReverted"
    (_, "FanoutProgress") -> Just "HeadPartialFannedOut"
    ("FanoutProgress", "PendingFanout") -> Just "HeadPartialFannedOut (empty total)"
    (_, "PendingFanout") -> Just "fanout starts or empty partial observation"
    _ -> Nothing
