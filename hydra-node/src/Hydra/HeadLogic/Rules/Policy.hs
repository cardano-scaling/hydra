{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.Rules.Policy where

import Hydra.HeadLogic.Program (Program, getUserValidation)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), SeenSnapshot (..))
import Hydra.Prelude
import Hydra.Tx (IsTx, Snapshot (..), UTxOType, applyTxTo, getSnapshot, utxoFromTx, withoutUTxO)
import Hydra.UserValidationRules (PolicyContext (..), PolicyViolation, UserValidation (..), localPolicyContextFor)

checkTransactionPolicy :: tx -> Program tx (Either PolicyViolation ())
checkTransactionPolicy tx = do
  mvalidation <- getUserValidation
  case mvalidation of
    Nothing -> pure (Right ())
    Just UserValidation{validateTransaction} -> pure $ validateTransaction tx

-- | Check the candidate UTxO without changing the local policy context.
checkTransactionUTxO :: IsTx tx => CoordinatedHeadState tx -> tx -> Program tx (Either PolicyViolation ())
checkTransactionUTxO chs tx = do
  mvalidation <- getUserValidation
  case mvalidation of
    Nothing -> pure (Right ())
    Just validation ->
      let current = localPolicyContextFor validation $ localPolicyUTxO chs
          candidate = current.applyPolicyTx tx
       in pure candidate.policyResult

-- | Reserve incoming deposits, but exclude outgoing outputs from the count limit.
-- Other UTxO policies still check the outgoing outputs until settlement.
localPolicyUTxO :: IsTx tx => CoordinatedHeadState tx -> (UTxOType tx, UTxOType tx)
localPolicyUTxO chs =
  (active <> incoming, outgoing)
 where
  snapshot = case chs.seenSnapshot of
    SeenSnapshot{snapshot = seen} -> seen
    _ -> getSnapshot chs.confirmedSnapshot
  incoming
    | chs.version == snapshot.version = fold snapshot.utxoToCommit
    | otherwise = mempty
  -- A signed decommit takes precedence over a locally queued one.
  (active, outgoing)
    | chs.version == snapshot.version, Just withdrawn <- snapshot.utxoToDecommit = (chs.localUTxO, withdrawn)
    | Just tx <- chs.decommitTx = (applyTxTo tx chs.localUTxO `withoutUTxO` utxoFromTx tx, utxoFromTx tx)
    | otherwise = (chs.localUTxO, mempty)
