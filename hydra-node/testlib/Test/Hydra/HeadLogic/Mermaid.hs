-- | Development diagrams. Lanes describe responsibilities within a node;
-- arrows retain instruction order and do not imply persistence or delivery.
module Test.Hydra.HeadLogic.Mermaid (renderSteps, renderTraces, renderTimeline) where

import Data.Text qualified as Text
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.Prelude hiding (label)
import Test.Hydra.HeadLogic.Debugger (Step (..), StepKind (..))

renderSteps :: [Step] -> Text
renderSteps = renderSequence . map (\Step{kind, description} -> (kind, description))

-- | Each title introduces a handler invocation or an observed outcome. Empty
-- sections are useful when a frozen handler has no instruction trace.
renderTraces :: [(Text, [Trace.TraceStep])] -> Text
renderTraces = renderSequence . concatMap (\(title, steps) -> (InputStep, title) : map observation steps)
 where
  observation step = (kind step, Trace.traceLabel step)
  kind = \case
    Trace.LedgerApplied{} -> LedgerStep
    Trace.PrimitiveCalled{} -> CryptoStep
    Trace.Changed{} -> ChangeStep
    Trace.Emitted{} -> EffectStep
    Trace.Decided{} -> DecisionStep
    Trace.Finished -> TerminalStep
    Trace.Suspended -> TerminalStep
    Trace.Rejected -> TerminalStep

renderSequence :: [(StepKind, Text)] -> Text
renderSequence steps = Text.unlines $ header <> participants <> sections steps
 where
  lanes =
    ("logic", "Head logic")
      :| [(name, label) | (kind, name, label) <- [(LedgerStep, "ledger", "Ledger"), (CryptoStep, "crypto", "Cryptography"), (ChangeStep, "changes", "Staged changes"), (EffectStep, "effects", "Buffered effects")], any ((== kind) . fst) steps]
  participants = ["  participant " <> name <> " as " <> label | (name, label) <- toList lanes]
  laneSpan = Text.intercalate "," $ ordNub ["logic", fst $ last lanes]
  sections [] = []
  sections ((InputStep, title) : rest) = section title rest
  sections rest = section "Execution" rest
  section title rest =
    let (body, remaining) = break ((== InputStep) . fst) rest
     in ["  rect rgb(245, 247, 250)", "    Note over " <> laneSpan <> ": " <> escape title]
          <> map render body
          <> ["  end"]
          <> sections remaining
  render (kind, label) = "    " <> address kind <> ": " <> escape label
  address = \case
    InputStep -> "Note over " <> laneSpan
    DecisionStep -> "Note over logic"
    LedgerStep -> "ledger-->>logic"
    CryptoStep -> "crypto-->>logic"
    ChangeStep -> "logic->>changes"
    EffectStep -> "logic->>effects"
    TerminalStep -> "Note over " <> laneSpan

-- | Whole-update observations, grouped by the party that processes them.
-- These are notes, since the observations do not imply inter-party messages.
renderTimeline :: [(Text, Text)] -> Text
renderTimeline events = Text.unlines $ header <> participants <> observations
 where
  lanes = zip (ordNub $ map fst events) [0 :: Int ..]
  name :: Int -> Text
  name n = "party" <> show n
  participants = ["  participant " <> name n <> " as " <> escape label | (label, n) <- lanes]
  observations = ["  Note over " <> name n <> ": " <> escape event | (party, event) <- events, (label, n) <- lanes, label == party]

header :: [Text]
header = ["%%{init: {\"sequence\": {\"wrap\": true}}}%%", "sequenceDiagram"]

-- | Mermaid numeric entities protect both its grammar and HTML labels. Escape
-- each input character once so inserted entities are not escaped again.
escape :: Text -> Text
escape = Text.concatMap $ \case
  '\n' -> " "
  '\r' -> " "
  '&' -> "#38;"
  '"' -> "#34;"
  '<' -> "#60;"
  '>' -> "#62;"
  ';' -> "#59;"
  '#' -> "#35;"
  c -> Text.singleton c
