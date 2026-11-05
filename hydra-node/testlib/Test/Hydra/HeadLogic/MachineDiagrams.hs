{-# LANGUAGE OverloadedRecordDot #-}

-- | Development-only rendering of the actual crem definitions.
module Test.Hydra.HeadLogic.MachineDiagrams (writeDiagrams) where

import Hydra.Prelude

import Crem.Render.Render (Mermaid (..), baseMachineAsGraph, renderStateDiagram)
import Crem.Render.RenderFlow (TreeMetadata (..), renderFlow)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Hydra.HeadLogic.Machine qualified as Machine
import Hydra.HeadLogic.Machine.Deposit qualified as Deposit
import Hydra.HeadLogic.Machine.Incremental qualified as Incremental
import Hydra.HeadLogic.Machine.Lifecycle qualified as Lifecycle
import Hydra.HeadLogic.Machine.OffChain qualified as OffChain
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node.State (NodeState (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hydra.HeadLogic.Scenario (initialNode)

writeDiagrams :: FilePath -> IO ()
writeDiagrams directory = do
  createDirectoryIfMissing True directory
  (flow, _, _) <- either fail pure $ renderFlow labels (Machine.components initialNode)
  let diagrams =
        [ ("composition", annotateLifecycle "Lifecycle_" $ "stateDiagram-v2\n" <> getText flow)
        , ("lifecycle", annotateLifecycle "" $ getText $ renderStateDiagram $ baseMachineAsGraph $ Lifecycle.machine @SimpleTx @Identity initialNode.headState)
        , ("deposits", getText $ renderStateDiagram $ baseMachineAsGraph $ Deposit.machine @Identity @SimpleTx Nothing)
        , ("incremental", getText $ renderStateDiagram $ baseMachineAsGraph $ Incremental.machine @SimpleTx @Identity initialNode.headState)
        , ("off-chain", getText $ renderStateDiagram $ baseMachineAsGraph $ OffChain.machine @SimpleTx @Identity initialNode.headState)
        ]
  forM_ diagrams $ \(name, diagram) -> Text.writeFile (directory </> name <> ".mmd") (Text.stripEnd diagram <> "\n")
  Text.writeFile (directory </> "README.md") $
    "# Head state machines\n\nGenerated from crem's executable definitions with:\n\n```sh\ncabal run hydra-node:head-logic-lab -- machines "
      <> toText directory
      <> "\n```\n\n"
      <> "These are allowed replay transitions, including resets and events outside their usual phase. Edges to Idle mean observed completion (HeadFannedOut), including at parties that did not initiate fanout. Identity transitions are implicit. They do not show admission guards or prove that every edge is reachable from network input. See [the implementation guide](../head-logic-crem.md) for event meanings and limitations.\n"
      <> foldMap (\(name, diagram) -> "\n## " <> toText name <> "\n\n```mermaid\n" <> diagram <> "\n```\n") diagrams
 where
  labels = BinaryLabel (BinaryLabel (BinaryLabel (LeafLabel "Lifecycle") (LeafLabel "Incremental")) (LeafLabel "OffChain")) (LeafLabel "NodeMetadata")

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
