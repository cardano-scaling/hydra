{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Review witnesses, loaded separately in the test REPL. The assertions
-- reproduce defects and API limitations; they do not specify desired behavior.
module ReviewExamples (runExamples, writeConflictTrace) where

import Hydra.Prelude

import Data.List ((!!))
import Data.Text.IO qualified as Text
import Hydra.API.ClientInput (ClientInput (SideLoadSnapshot))
import Hydra.API.ServerOutput (DecommitInvalidReason (DecommitAlreadyInFlight))
import Hydra.HeadLogic qualified as Head
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotVerification (verifyCandidate)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), OpenState (..), mkSeenSnapshot)
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (NodeState, initialChainTime)
import Hydra.Tx.Crypto (Verified (KeyNumberMismatch), verifyMultiSignature)
import Hydra.Tx.Party (vkey)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.CoordinatedAudit (reservationHeld)
import Test.Hydra.HeadLogic.Explorer qualified as Explorer
import Test.Hydra.HeadLogic.InputComparison (compareInput)
import Test.Hydra.HeadLogic.LegacyState (legacyAggregateNodeState)
import Test.Hydra.HeadLogic.Mermaid (renderTraces)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)

decommit :: SimpleTx
decommit = SimpleTx 10 (utxoRef 1) (utxoRef 10)

initial :: Scenario.World
initial =
  Scenario.initialWorld
    { Scenario.pending =
        [ Scenario.Delivery sender recipient 2 message
        | (sender, message) <- [(0, ReqTx Scenario.transactionOne), (1, ReqDec decommit)]
        , recipient <- [0, 1, 2]
        ]
    }

prefix :: [Scenario.Action]
prefix =
  [ Scenario.Deliver (Scenario.Delivery sender recipient 2 message)
  | (sender, recipient, message) <-
      [ (0, 0, ReqTx Scenario.transactionOne)
      , (1, 1, ReqDec decommit)
      , (0, 1, ReqTx Scenario.transactionOne)
      , (0, 2, ReqTx Scenario.transactionOne)
      , (1, 0, ReqDec decommit)
      , (1, 2, ReqDec decommit)
      ]
  ]

type Trace = [(Text, [TraceStep])]

-- ReqDec is deliberately outside the general explorer's validated scenario
-- domain. This focused schedule reuses its delivery and production retry logic,
-- and checks every input against the existing compatibility/replay harness.
conflict :: Either Text (Scenario.World, Trace)
conflict = foldlM deliver (initial, []) prefix >>= drain 100
 where
  drain :: Int -> (Scenario.World, Trace) -> Either Text (Scenario.World, Trace)
  drain 0 _ = Left "review example exceeds its delivery bound"
  drain remaining current@(world, _) = case world.pending of
    d : _ -> deliver current (Scenario.Deliver d) >>= drain (remaining - 1)
    [] -> case world.retries of
      d : _ -> deliver current (Scenario.Retry d) >>= drain (remaining - 1)
      [] -> Right current

deliver :: (Scenario.World, Trace) -> Scenario.Action -> Either Text (Scenario.World, Trace)
deliver (world, sections) action = do
  delivery <- case action of
    Scenario.Deliver d -> Right d
    Scenario.Retry d -> Right d
    _ -> Left "review example schedules deliveries and retries only"
  let node = world.nodes !! delivery.recipientIndex
      env = Scenario.environment delivery.recipientIndex
      input = Head.NetworkInput delivery.remainingTTL $ ReceivedMessage (Scenario.scenarioParties !! delivery.senderIndex) delivery.message
      title = case delivery.message of
        ReqDec tx -> "ReqDec " <> show tx.txSimpleId <> " to party " <> show delivery.recipientIndex <> " (TTL " <> show delivery.remainingTTL <> ")"
        _ -> Scenario.actionLabel action
  (outcome, observations) <- compareInput env initialChainTime node input
  let events = case outcome of Head.Continue es _ -> es; Head.Wait _ es -> es; Head.Error{} -> []
  unless (Head.aggregateState node outcome == foldl' legacyAggregateNodeState node events) $
    Left "state differs from the original frozen reducer"
  next <- Explorer.advance Scenario.VerifySignatures world action
  pure (next, sections <> [(title, observations)])

coordinated :: NodeState SimpleTx -> CoordinatedHeadState SimpleTx
coordinated = (.coordinatedHeadState) . Scenario.openState

runExamples :: IO ()
runExamples = do
  (world, sections) <- either (fail . toString) pure conflict
  ensure "24 deliveries drain all messages and retries" $
    length sections == 24 && null world.pending && null world.retries
  ensure "all parties confirm the ordinary transaction" $
    all ((== 1) . (.number) . getSnapshot . (.confirmedSnapshot) . coordinated) world.nodes
  ensure "all parties confirm output 2 and have no pending ordinary transactions" $
    all (\node -> let current = coordinated node in (getSnapshot current.confirmedSnapshot).utxo == utxoRef 2 && null current.localTxs) world.nodes
  ensure "only Bob retains the conflicting decommit" $
    map ((.decommitTx) . coordinated) world.nodes == [Nothing, Just decommit, Nothing]
  ensure "the existing reservation invariant misses the conflict" $
    all (reservationHeld . coordinated) world.nodes
  let bob = world.nodes !! 1
      current = coordinated bob
      next = SimpleTx 11 (utxoRef 2) (utxoRef 11)
      outcome =
        Head.update (Scenario.environment 1) simpleLedger initialChainTime bob $
          Head.NetworkInput 0 $
            ReceivedMessage (Scenario.scenarioParties !! 1) (ReqDec next)
  ensure "Bob has the input for the new decommit" $ current.localUTxO == utxoRef 2
  ensure "the orphaned reservation rejects a new decommit" $ case outcome of
    Head.Continue [Head.DecommitInvalid{Head.decommitInvalidReason = DecommitAlreadyInFlight 10}] [] -> True
    _ -> False
  let reload =
        Head.update (Scenario.environment 1) simpleLedger initialChainTime bob $
          Head.ClientInput $
            SideLoadSnapshot current.confirmedSnapshot
  ensure "side-loading the confirmed snapshot clears the orphan" $
    isNothing (coordinated $ Head.aggregateState bob reload).decommitTx
  putStrLn ("Conflicting decommit: reproduced in 24 deliveries; every compatibility/replay comparison passes. Side-load recovery works." :: String)

  ensure "unbound verification evidence permits empty signatures" $ case emptyKeyConfirmation of
    Head.Continue [Head.SnapshotConfirmed _ Nothing signatures] [] ->
      verifyMultiSignature (vkey <$> Scenario.scenarioParties) signatures Scenario.initialSnapshot{number = 1} == KeyNumberMismatch
    _ -> False
  putStrLn ("Verification API: empty key list creates evidence accepted by the owner. This is caller misuse, not an input accepted by AckSn." :: String)
 where
  ensure :: String -> Bool -> IO ()
  ensure message condition = unless condition $ fail message

-- The real verifier is used. No primitive substitution or unsafe operation is
-- needed, but production AckSn supplies the actual Head's keys instead of [].
emptyKeyConfirmation :: Head.Outcome SimpleTx
emptyKeyConfirmation = Interpreter.runProgramWith Interpreter.basicConfig $ do
  evidence <- verifyCandidate [] seen mempty >>= either Program.reject pure
  void $ Coordinated.stage (Coordinated.start open) $ Coordinated.confirmSnapshot evidence
 where
  seen = mkSeenSnapshot Scenario.initialSnapshot{number = 1} mempty
  original = Scenario.openState Scenario.initialNode
  open = original{coordinatedHeadState = original.coordinatedHeadState{seenSnapshot = seen}}

writeConflictTrace :: FilePath -> IO ()
writeConflictTrace path = do
  (_, sections) <- either (fail . toString) pure conflict
  Text.writeFile path $ renderTraces sections
