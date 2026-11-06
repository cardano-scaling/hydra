{-# LANGUAGE OverloadedRecordDot #-}

-- | Competing ordinary and decommit spends, using the actual delivery and retry
-- machinery. Every prefix checks independently corrected replay and CBOR.
module Test.Hydra.HeadLogic.ConflictingDecommit (conflict, schedules) where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.List ((!!))
import Hydra.HeadLogic qualified as Head
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (initialChainTime)
import Test.Hydra.HeadLogic.Explorer qualified as Explorer
import Test.Hydra.HeadLogic.ReplayReference (replayWithReservationFix)
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
-- and checks every retained prefix against independently corrected replay.
conflict :: Either Text (Scenario.World, Trace)
conflict = conflictWith prefix

conflictWith :: [Scenario.Action] -> Either Text (Scenario.World, Trace)
conflictWith route = foldlM deliver (initial, []) route >>= drain 100
 where
  drain :: Int -> (Scenario.World, Trace) -> Either Text (Scenario.World, Trace)
  drain 0 _ = Left "decommit conflict exceeds its delivery bound"
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
    _ -> Left "decommit conflict schedules deliveries and retries only"
  let node = world.nodes !! delivery.recipientIndex
      env = Scenario.environment delivery.recipientIndex
      input = Head.NetworkInput delivery.remainingTTL $ ReceivedMessage (Scenario.scenarioParties !! delivery.senderIndex) delivery.message
      title = case delivery.message of
        ReqDec tx -> "ReqDec " <> show tx.txSimpleId <> " to party " <> show delivery.recipientIndex <> " (TTL " <> show delivery.remainingTTL <> ")"
        _ -> Scenario.actionLabel action
  let (outcome, observations) = Interpreter.traceProgramWith (Interpreter.snapshotConfig env simpleLedger) $ Head.updateProgram (Head.ruleEnvironment env) initialChainTime node input
      events = case outcome of Head.Continue es _ -> es; Head.Wait _ es -> es; Head.Error{} -> []
      after = Head.aggregateState node outcome
      expected = foldl' replayWithReservationFix node events
  unless (after == expected) $ Left "state differs from corrected independent replay"
  unless (decodeFull' (serialize' after) == Right expected) $ Left "checkpoint round trip differs"
  decoded <- first show $ decodeFull' @[Head.StateChanged SimpleTx] (serialize' events)
  unless (foldl' Head.aggregateNodeState node decoded == expected) $ Left "event round trip differs"
  next <- Explorer.advance Scenario.VerifySignatures world action
  pure (next, sections <> [(title, observations)])

-- Alice proposes the ordinary spend; Bob reserves the competing decommit.
-- Vary the remaining four initial deliveries, then drain messages and retries.
schedules :: [Either Text (Scenario.World, Trace)]
schedules = [conflictWith (take 2 prefix <> rest) | rest <- permutations (drop 2 prefix)]
