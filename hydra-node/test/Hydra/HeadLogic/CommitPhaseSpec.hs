{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.CommitPhaseSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude (Expectation, Spec, describe, it, shouldBe)

import Cardano.Binary (decodeFull', serialize')
import Control.Monad (foldM)
import Crem.Inspect (Component (RightComponent), inspectStates)
import Crem.StateMachine qualified as Crem
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Program (Instruction (VerifySnapshot), Program, foldProgram)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot, verifyCandidate, verifySignedSnapshot)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (LastSeenSnapshot))
import Hydra.HeadLogicSpec (inOpenState, testSnapshot, withAccumulators)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..))
import Hydra.Node.State (NodeState (headState))
import Hydra.Tx (UTxOType)
import Hydra.Tx.Crypto (aggregate, getVerificationKey, sign, verifyMultiSignatureBytes)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..))
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk)

spec :: Spec
spec = describe "commit ownership phases" $ do
  it "restores selection and signed authority independently, including legacy certificates without a deposit id" $
    forM_ [(selected, version, signedVersion, claim, depositIdentity) | selected <- [Nothing, Just 7], version <- [0 .. 2], signedVersion <- [0 .. 2], claim <- [Nothing, Just mempty, Just deposited], depositIdentity <- [Nothing, Just 7]] $ \(selected, version, signedVersion, claim, depositIdentity) -> do
      let signed = snapshot{version = fromInteger signedVersion, utxoToCommit = claim, depositTxId = depositIdentity}
          before = open{coordinatedHeadState = open.coordinatedHeadState{version = fromInteger version, currentDepositTxId = selected, confirmedSnapshot = ConfirmedSnapshot signed mempty}}
          expected = Agda.commitPhaseRef (isJust selected) version signedVersion (isJust claim)
      commitPhase (Coordinated.components before) `shouldBe` Just (show expected)

  forM_ [False, True] $ \settleFirst ->
    it (if settleFirst then "retires authority when settlement precedes confirmation" else "confirms selection and retires authority on settlement") $ do
      let accept = Coordinated.acceptSnapshot snapshot mempty (Just 7)
          (_, accepted) = Coordinated.step (Coordinated.start open) accept
      owned <- maybe (fail "missing candidate") pure $ Coordinated.candidate accepted
      evidence <- verifyOnly (verifyCandidate owned $ aggregate [sign aliceSk snapshot]) >>= either (fail . show) pure
      let confirm = Coordinated.confirmSnapshot evidence
          settle = Coordinated.settleCommit (SimpleChainState 3) 1 7 deposited
          ending
            | settleFirst = [(settle, Nothing, "NoCommit", deposited), (confirm, Nothing, "NoCommit", deposited)]
            | otherwise = [(confirm, Just 7, "AwaitingCommitSettlement", mempty), (settle, Nothing, "NoCommit", deposited)]
          spend = SimpleTx 100 deposited (utxoRef 100)
      checkSteps open $
        [(accept, Just 7, "CommitSelected", mempty)]
          <> ending
          <> [ (Coordinated.applyTransaction spend deposited (utxoRef 100), Nothing, "NoCommit", utxoRef 100)
             , (settle, Nothing, "NoCommit", utxoRef 100)
             ]

  it "keeps adopted authority without selection across clearing and ordinary transactions" $ do
    evidence <- verified snapshot
    let tx = SimpleTx 100 mempty (utxoRef 100)
    checkSteps
      open
      [ (Coordinated.adoptSnapshot evidence, Nothing, "AwaitingCommitSettlement", mempty)
      , (Coordinated.clearLocalState 1, Nothing, "AwaitingCommitSettlement", mempty)
      , (Coordinated.applyTransaction tx mempty (utxoRef 100), Nothing, "AwaitingCommitSettlement", utxoRef 100)
      , (Coordinated.settleCommit (SimpleChainState 3) 1 7 deposited, Nothing, "NoCommit", deposited <> utxoRef 100)
      ]

  it "retires a dropped claim only when the replacement snapshot is confirmed" $ do
    let before = open{coordinatedHeadState = open.coordinatedHeadState{currentDepositTxId = Just 7, confirmedSnapshot = ConfirmedSnapshot snapshot mempty, seenSnapshot = LastSeenSnapshot 1}}
        replacement = testSnapshot 2 0 [] mempty
    evidence <- verified replacement
    checkSteps
      before
      [ (Coordinated.acceptSnapshot replacement mempty Nothing, Nothing, "AwaitingCommitSettlement", mempty)
      , (Coordinated.adoptSnapshot evidence, Nothing, "NoCommit", mempty)
      ]

  it "preserves another selection when adoption retires an older claim" $ do
    let before = open{coordinatedHeadState = open.coordinatedHeadState{currentDepositTxId = Just 8, confirmedSnapshot = ConfirmedSnapshot snapshot mempty}}
    evidence <- verified $ testSnapshot 2 0 [] mempty
    checkSteps before [(Coordinated.adoptSnapshot evidence, Just 8, "CommitSelected", mempty)]

  it "retires old commit authority on a newer decrement while preserving selection" $ do
    let before = open{coordinatedHeadState = open.coordinatedHeadState{currentDepositTxId = Just 8, confirmedSnapshot = ConfirmedSnapshot snapshot mempty}}
    checkSteps before [(Coordinated.settleDecommit (SimpleChainState 3) 1 (utxoRef 9), Just 8, "CommitSelected", mempty)]

  it "preserves a newer selection on duplicate and older increment observations" $ do
    let before = open{coordinatedHeadState = open.coordinatedHeadState{currentDepositTxId = Just 7, confirmedSnapshot = ConfirmedSnapshot snapshot mempty, seenSnapshot = LastSeenSnapshot 1}}
        proposal = withAccumulators $ (testSnapshot 2 1 [] deposited){utxoToCommit = Just (utxoRef 8), depositTxId = Just 8}
    checkSteps
      before
      [ (Coordinated.settleCommit (SimpleChainState 3) 1 7 deposited, Nothing, "NoCommit", deposited)
      , (Coordinated.acceptSnapshot proposal mempty (Just 8), Just 8, "CommitSelected", deposited)
      , (Coordinated.settleCommit (SimpleChainState 4) 1 7 deposited, Just 8, "CommitSelected", deposited)
      , (Coordinated.settleCommit (SimpleChainState 2) 0 7 deposited, Just 8, "CommitSelected", deposited)
      ]

  it "does not force the transaction ledger to advance or inspect commit selection" $ do
    let (_, next) = runIdentity $ Crem.run (Coordinated.components open) $ Coordinated.acceptSnapshot snapshot (Seq.singleton $ error "commit phase demands transactions") (Just 7)
    commitPhase next `shouldBe` Just "CommitSelected"

-- Compare the live continuation, retained events and checkpoint reconstruction
-- after every command, including the selected pointer and spendable ledger.
checkSteps :: OpenState SimpleTx -> [(Coordinated.Command SimpleTx, Maybe Integer, Text, UTxOType SimpleTx)] -> Expectation
checkSteps initial = void . foldM step (Coordinated.components initial, Coordinated.start initial)
 where
  step (machine, runtime) (command, selected, phase, utxo) = do
    let (_, next) = runIdentity $ Crem.run machine command
        (events, updated) = Coordinated.step runtime command
        current = Coordinated.currentState updated
        replayed = foldl' (flip $ Coordinated.replayEvent deposited) runtime events
    current.currentDepositTxId `shouldBe` selected
    current.localUTxO `shouldBe` utxo
    Coordinated.currentState replayed `shouldBe` current
    restored <- either (fail . show) pure $ decodeFull' $ serialize' current
    inspectStates next `shouldBe` inspectStates (Coordinated.components initial{coordinatedHeadState = restored})
    commitPhase next `shouldBe` Just phase
    pure (next, updated)

commitPhase :: Crem.StateMachineT m input output -> Maybe Text
commitPhase = join . List.lookup [RightComponent] . inspectStates

verified :: Snapshot SimpleTx -> IO (VerifiedSnapshot SimpleTx)
verified value =
  verifyOnly (verifySignedSnapshot [getVerificationKey aliceSk] value $ aggregate [sign aliceSk value]) >>= either (fail . show) pure

-- Exercise the real verification primitive while retaining its opaque result.
verifyOnly :: Program SimpleTx a -> IO a
verifyOnly = foldProgram $ \case
  VerifySnapshot parties signatures bytes -> pure $ verifyMultiSignatureBytes parties signatures bytes
  _ -> fail "unexpected primitive while verifying a test certificate"

open :: OpenState SimpleTx
open = case (inOpenState [alice]).headState of
  Open value -> value
  _ -> error "fixture does not open a Head"

deposited :: UTxOType SimpleTx
deposited = utxoRef 7

snapshot :: Snapshot SimpleTx
snapshot = withAccumulators $ (testSnapshot 1 0 [] mempty){utxoToCommit = Just deposited, depositTxId = Just 7}
