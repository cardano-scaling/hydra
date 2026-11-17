-- | Stage a checked coordinated update in the current input program.
module Hydra.HeadLogic.Rules.Coordinated (stage) where

import Hydra.HeadLogic.Error (LogicError (AssertionFailed))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  change,
  reject,
 )
import Hydra.HeadLogic.State.Coordinated (
  Command,
  Runtime,
  advance,
 )
import Hydra.Prelude

stage :: Runtime tx -> Command tx -> Program tx (Runtime tx)
stage runtime command = case advance runtime command of
  Left message -> Program.reject $ AssertionFailed message
  Right (events, next) -> traverse_ Program.change events $> next
