{-# LANGUAGE RecordWildCards #-}

module Hydra.Node.HeadLogic.Configuration where

import Hydra.HeadLogic.Cardano (cardanoSnapshotLimits)
import Hydra.HeadLogic.Interpreter
import Hydra.HeadLogic.Rules.Environment
import Hydra.HeadLogic.Snapshot.Request (SettlingCommit, settlingCommitAtPeriod)
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.Environment qualified as Node
import Hydra.Node.State (PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (IsTx)
import Hydra.Tx.Crypto (getVerificationKey, sign)
import Hydra.Tx.Snapshot (Snapshot, SnapshotVersion)

ruleEnvironment ::
  Environment ->
  RuleEnvironment
ruleEnvironment Node.Environment{..} =
  let snapshotLimits = cardanoSnapshotLimits
   in RuleEnvironment{..}

interpreterConfig ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  InterpreterConfig tx
interpreterConfig Environment{signingKey} ledger =
  mkInterpreterConfig ledger $ SnapshotSigner (sign signingKey) (getVerificationKey signingKey)

settlingCommitAt ::
  IsTx tx =>
  Environment ->
  SnapshotVersion ->
  UTCTime ->
  PendingDeposits tx ->
  Snapshot tx ->
  Maybe (SettlingCommit tx)
settlingCommitAt Environment{depositPeriod} = settlingCommitAtPeriod depositPeriod
