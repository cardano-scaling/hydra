{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Bounded, deterministic development scheduler over the production programs.
module Test.Hydra.HeadLogic.Explorer where

import Cardano.Binary (serialize')
import Data.List (delete, (!!))
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Hydra.HeadLogic hiding (error)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Retry
import Hydra.Ledger.Simple
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude hiding (delete, local, state)
import Hydra.Tx (txId)
import Hydra.Tx.Crypto (Verified (..), sign, verifyMultiSignature)
import Hydra.Tx.Party (vkey)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.Scenario
import Test.Hydra.Node.Fixture (aliceSk)

programFor :: World -> Delivery -> Interpreter.Execution SimpleTx
programFor world Delivery{senderIndex, recipientIndex, remainingTTL, message} =
  case message of
    ReqTx tx -> Interpreter.Execution (Interpreter.ledgerConfig simpleLedger) $ onOpenNetworkReqTxProgram (ruleEnvironment env) st.chainPointTime os remainingTTL mempty tx
    ReqSn sv sn txs decommit deposit -> Interpreter.Execution (Interpreter.snapshotConfig env simpleLedger) $ onOpenNetworkReqSnProgram (ruleEnvironment env) mempty st.chainPointTime os remainingTTL sender sv sn txs decommit deposit
    AckSn signature sn -> Interpreter.Execution Interpreter.acknowledgementConfig $ onOpenNetworkAckSnProgram (ruleEnvironment env) st.chainPointTime mempty os sender signature sn
    _ -> error "message outside explorer domain"
 where
  env = environment recipientIndex
  sender = scenarioParties !! senderIndex
  st = nodes world !! recipientIndex
  os = openState st

applyExecutionMode :: ExecutionMode -> Interpreter.Execution tx -> Interpreter.Execution tx
applyExecutionMode mode (Interpreter.Execution config program) =
  Interpreter.Execution config $ case mode of
    VerifySignatures -> program
    BypassVerification -> Debug.bypassVerification program

-- | Ordering is stable. A delivery can stay pending while any other message is
-- delivered. Explicit delay and retry actions make disturbances reproducible.
actions :: Disturbances -> World -> [Action]
actions disturbances world =
  [Corrupt d | disturbances == AllowInvalidAcknowledgement, not world.corruptionUsed, d <- world.pending, AckSn{} <- [d.message]]
    <> fmap Deliver world.pending
    <> fmap Retry world.retries
    <> fmap Release world.delayed
    <> [Duplicate d | not world.duplicateUsed, d <- world.pending]
    <> [Delay d | not world.delayUsed, d <- world.pending]

isDelivery :: Action -> Bool
isDelivery = \case
  Deliver{} -> True
  Retry{} -> True
  _ -> False

-- | Fail on actions no longer enabled: minimization cannot invent deliveries or
-- retries after removing their prerequisites.
advance :: ExecutionMode -> World -> Action -> Either Text World
advance mode world action =
  if action `notElem` actions AllowInvalidAcknowledgement world
    then Left "scheduled action is not enabled"
    else case action of
      Duplicate d -> Right world{pending = world.pending <> [d], duplicateUsed = True}
      Delay d -> Right world{pending = delete d world.pending, delayed = world.delayed <> [d], delayUsed = True}
      Release d -> Right world{pending = world.pending <> [d], delayed = delete d world.delayed}
      Corrupt d -> case d.message of
        AckSn _ sn -> Right world{pending = replaceFirst d (d{message = AckSn (sign aliceSk world.originSnapshot) sn} :: Delivery) world.pending, corruptionUsed = True}
        _ -> Left "only acknowledgements can be corrupted"
      Deliver d -> Right $ deliver mode world{pending = delete d world.pending} d
      Retry d -> Right $ deliver mode world{retries = delete d world.retries} d

deliver :: ExecutionMode -> World -> Delivery -> World
deliver mode world d = installOutcome world d $ Interpreter.runExecution $ applyExecutionMode mode $ programFor world d

installOutcome :: World -> Delivery -> Outcome SimpleTx -> World
installOutcome world d outcome =
  world
    { nodes = take d.recipientIndex world.nodes <> [next] <> drop (d.recipientIndex + 1) world.nodes
    , pending = world.pending <> emitted
    , retries = world.retries <> rescheduled
    , confirmations = if snapshot.number > oldSnapshot.number then world.confirmations <> [snapshot] else world.confirmations
    }
 where
  st = world.nodes !! d.recipientIndex
  next = aggregateState st outcome
  snapshot = getSnapshot (openState next).coordinatedHeadState.confirmedSnapshot
  oldSnapshot = getSnapshot (openState st).coordinatedHeadState.confirmedSnapshot
  emitted = case outcome of
    Continue{effects} -> [Delivery d.recipientIndex recipient 2 message | NetworkEffect message <- effects, recipient <- [0 .. 2]]
    _ -> []
  rescheduled = case outcome of
    Wait{reason} -> case retryDecision reason (NetworkInput d.remainingTTL $ ReceivedMessage (scenarioParties !! d.senderIndex) d.message) of
      RetryInput (NetworkInput ttl _) -> [d{remainingTTL = ttl}]
      _ -> []
    _ -> []

-- | Independent checks: real verification, complete snapshot equality at equal
-- version/number across history, input availability in transaction order, and
-- resolvability of transactions still queued locally for a future snapshot.
-- A separate progress check applies only to this fixed honest scenario: when
-- every queue is empty, valid pending work must not be stranded at followers.
invariantFailure :: World -> Maybe Text
invariantFailure world = safetyFailure world <|> progressFailure world

-- | Safety is checked at every prefix, independently of eventual delivery.
safetyFailure :: World -> Maybe Text
safetyFailure world
  | any invalidSignature world.nodes = Just "confirmed snapshot has invalid signatures"
  | or [a /= b | a <- world.confirmations, b <- world.confirmations, (a.version, a.number) == (b.version, b.number)] = Just "confirmed snapshots disagree at the same version and number"
  | not (all respectsDependencies world.confirmations) = Just "confirmed transactions violate dependency order"
  | any missingLocalTransaction world.nodes = Just "local pending transaction differs from the registry payload"
  | any inconsistentLocalUTxO world.nodes = Just "local UTxO does not reconcile with snapshot and pending transactions"
  | otherwise = Nothing
 where
  missingLocalTransaction st =
    let c = (openState st).coordinatedHeadState
     in any (\tx -> Map.lookup (txId tx) c.allTxs /= Just tx) c.localTxs
  inconsistentLocalUTxO st =
    let c = (openState st).coordinatedHeadState
        base = case c.seenSnapshot of
          SeenSnapshot{snapshot} -> snapshot
          _ -> getSnapshot c.confirmedSnapshot
     in foldlM apply base.utxo c.localTxs /= Just c.localUTxO
  invalidSignature st = case (openState st).coordinatedHeadState.confirmedSnapshot of
    InitialSnapshot{} -> False
    ConfirmedSnapshot snapshot signatures -> verifyMultiSignature (vkey <$> scenarioParties) signatures snapshot /= Verified
  respectsDependencies snapshot =
    let previous = if snapshot.number == world.originSnapshot.number + 1 then Just world.originSnapshot else find (\s -> s.version == snapshot.version && s.number + 1 == snapshot.number) world.confirmations
     in case previous of
          Nothing -> False
          Just predecessor -> case foldlM apply predecessor.utxo snapshot.confirmed of
            Nothing -> False
            Just result -> result == snapshot.utxo
  apply :: Set SimpleTxOut -> SimpleTx -> Maybe (Set SimpleTxOut)
  apply available tx
    | tx.txInputs `Set.isSubsetOf` available = Just $ (available Set.\\ tx.txInputs) <> tx.txOutputs
    | otherwise = Nothing

-- | Finite retry expiry can violate this progress condition. It is separate
-- from safety: this scenario expects re-submission if a dependency expires.
progressFailure :: World -> Maybe Text
progressFailure world
  | not world.corruptionUsed
  , null world.pending && null world.retries && null world.delayed
  , not $ all (\node -> null (openState node).coordinatedHeadState.localTxs) world.nodes =
      Just "quiescent scenario has unconfirmed transactions"
  | otherwise = Nothing

explore :: Limits -> Exploration -> SearchReport
explore bounds settings@Exploration{searchOrder = RetryWindows} = exploreRetryWindows bounds settings
explore bounds settings = go Set.empty 0 0 (Seq.singleton (initialFor settings.family, [], 0))
 where
  report count cut capped = SearchReport bounds count cut (capped || cut > 0)
  go _ count cut Seq.Empty = report count cut False Nothing []
  go seen count cut ((world, reversed, depth) Seq.:<| rest)
    | count >= bounds.maxStates = report count cut True Nothing []
    | key `Set.member` seen = go seen count cut rest
    | Just err <- invariantFailure world = report (count + 1) cut (not $ Seq.null rest) (Just err) (reverse reversed)
    | depth >= bounds.maxDeliveries = go (Set.insert key seen) (count + 1) (cut + fromEnum (not $ null enabled)) rest
    | otherwise = go (Set.insert key seen) (count + 1) cut (case settings.searchOrder of BreadthFirst -> rest <> children; _ -> children <> rest)
   where
    key = serialize' (depth, world)
    enabled = orderActions settings.searchOrder $ actions settings.disturbances world
    children = Seq.fromList [(next, action : reversed, depth + fromEnum (isDelivery action)) | action <- enabled, Right next <- [advance settings.executionMode world action]]

-- | Focused finite schedules: receive an initial prefix, allow each recipient
-- an independently bounded retry window, then drain deliveries before retries.
-- This varies asymmetric expiry without enumerating unrelated permutations.
-- It excludes optional disturbances and makes no claim about other schedules or
-- production wall-clock timing. Eligibility still comes from 'advance'.
exploreRetryWindows :: Limits -> Exploration -> SearchReport
exploreRetryWindows bounds settings = go Set.empty 0 0 schedules
 where
  start = initialFor settings.family
  retryBound = foldl' max (0 :: Int) $ map (fromIntegral . remainingTTL) start.pending
  schedules = [(start, [], 0, prefix, budgets) | prefix <- [0 .. length start.pending], budgets <- replicateM (length start.nodes) [0 .. retryBound]]
  report count cut capped = SearchReport bounds count cut (capped || cut > 0)
  go _ count cut [] = report count cut False Nothing []
  go seen count cut ((world, route, depth, prefix, budgets) : rest)
    | count >= bounds.maxStates = report count cut True Nothing []
    | key `Set.member` seen = go seen count cut rest
    | Just err <- invariantFailure world = report (count + 1) cut (not $ null rest) (Just err) (reverse route)
    | Nothing <- next = go seen' (count + 1) cut rest
    | depth >= bounds.maxDeliveries = go seen' (count + 1) (cut + 1) rest
    | Just (action, prefix', budgets') <- next
    , Right world' <- advance settings.executionMode world action =
        go seen' (count + 1) cut ((world', action : route, depth + 1, prefix', budgets') : rest)
    | otherwise = error "retry-window scheduler selected a disabled action"
   where
    -- Remaining window budgets affect future scheduling, so belong in the key.
    key = serialize' (depth, world, prefix, budgets)
    seen' = Set.insert key seen
    next
      | prefix > 0, d : _ <- world.pending = Just (Deliver d, prefix - 1, budgets)
      | Just (recipient, d) <- listToMaybe [(recipient, d) | (recipient, budget) <- zip [0 ..] budgets, budget > 0, d <- world.retries, d.recipientIndex == recipient] =
          Just (Retry d, 0, zipWith (\i budget -> if i == recipient then budget - 1 else budget) [0 ..] budgets)
      | d : _ <- world.pending = Just (Deliver d, 0, replicate (length budgets) 0)
      | d : _ <- world.retries = Just (Retry d, 0, replicate (length budgets) 0)
      | otherwise = Nothing

runPath :: ExecutionMode -> World -> [Action] -> Either Text World
runPath mode start route = validateWorld start >> foldlM (advance mode) start route

-- | Greedy deletion to a fixed point. This is minimized under single-action
-- removal, including optional disturbances, not globally minimal.
minimize :: ExecutionMode -> World -> [Action] -> [Action]
minimize mode start original = go original
 where
  expectedFailure = either (const Nothing) invariantFailure $ runPath mode start original
  go route = maybe route go $ find reproduces [take n route <> drop (n + 1) route | n <- [0 .. length route - 1]]
  reproduces candidate = case runPath mode start candidate of
    Right world -> isJust expectedFailure && invariantFailure world == expectedFailure
    Left _ -> False

replaceFirst :: Eq a => a -> a -> [a] -> [a]
replaceFirst old new items = case break (== old) items of
  (before, _ : after) -> before <> [new] <> after
  _ -> items

-- | Outcomes are retained only for inspection, never in the deduplication key.
pathOutcomes :: ExecutionMode -> World -> [Action] -> Either Text [Outcome SimpleTx]
pathOutcomes mode = go
 where
  go _ [] = Right []
  go world (action : rest) = do
    next <- advance mode world action
    let result = case action of
          Deliver d -> [Interpreter.runExecution $ applyExecutionMode mode $ programFor world d]
          Retry d -> [Interpreter.runExecution $ applyExecutionMode mode $ programFor world d]
          _ -> []
    (result <>) <$> go next rest

-- | Retry-focused search reaches budget-sensitive paths before unrelated traffic.
-- Breadth-first search changes frontier discipline, retaining stable action order.
orderActions :: SearchOrder -> [Action] -> [Action]
orderActions RetriesFirst choices = filter urgent choices <> filter (not . urgent) choices
 where
  urgent = \case Retry{} -> True; Duplicate{} -> True; Corrupt{} -> True; _ -> False
orderActions _ choices = choices
