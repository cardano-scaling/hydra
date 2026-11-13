{-# LANGUAGE DuplicateRecordFields #-}

-- | Cardano runtime wiring and compatibility entry points for the pure core.
module Hydra.Node.HeadLogic (
  module Hydra.Node.HeadLogic,
  module Hydra.HeadLogic,
  module Hydra.Node.HeadLogic.Configuration,
) where

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.Fanout qualified as Fanout
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment)
import Hydra.Node.HeadLogic.Configuration
import Hydra.Node.State (NodeState)
import Hydra.Prelude
import Hydra.Tx (IsTx, UTxOType)

-- | The node and tools execute the same admission checks and rule programs.
update :: IsChainState tx => Environment -> Ledger tx -> UTCTime -> NodeState tx -> Input tx -> Outcome tx
update env ledger now st input =
  Interpreter.runProgram (interpreterConfig env ledger) $ updateProgram (ruleEnvironment env) now st input

onClosedChainPartialFanoutTx ::
  IsTx tx =>
  Interpreter.InterpreterConfig tx ->
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Outcome tx
onClosedChainPartialFanoutTx config st cs utxo =
  Interpreter.runProgram config $
    Fanout.onClosedChainPartialFanoutTx st cs utxo

repostFanoutStep :: IsTx tx => Interpreter.InterpreterConfig tx -> PartialFanoutState tx -> Outcome tx
repostFanoutStep config st =
  Interpreter.runProgram config $
    Fanout.repostFanoutStep st
