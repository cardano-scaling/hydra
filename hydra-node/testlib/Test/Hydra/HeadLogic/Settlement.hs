{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Bounded deposit/rollback sequences through the complete production update.
-- Chain observations are trusted inputs here; no Cardano node or validator runs.
module Test.Hydra.HeadLogic.Settlement (SettlementStep (..), settlementSequence) where

import Cardano.Binary (decodeFull', serialize')
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Chain (ChainEvent (..), OnChainTx (..), PostChainTx (..))
import Hydra.HeadLogic hiding (changes, error)
import Hydra.Ledger.Simple
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Prelude hiding (label, state)
import Hydra.Tx.Crypto (MultiSignature, Verified (..), verifyMultiSignature)
import Hydra.Tx.Party (vkey)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)
import Test.Hydra.HeadLogic.Scenario (Delivery (..), environment, initialNode, openState, scenarioParties)
import Test.Hydra.Node.Fixture (alice, testHeadId)

data SettlementStep = SettlementStep
  { label :: Text
  , recipient :: Int
  , input :: Input SimpleTx
  , outcome :: Outcome SimpleTx
  , before :: NodeState SimpleTx
  , after :: NodeState SimpleTx
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Run = Run
  { nodes :: [NodeState SimpleTx]
  , messages :: [Delivery]
  , steps :: [SettlementStep]
  }

-- | Vary whether the L2 spend confirms before rollback, the rollback boundary
-- (2 erases the increment; 3 retains it), and re-observation order at the parties.
-- Every transition checks event replay and checkpoint CBOR round trips.
settlementSequence :: Bool -> Int -> [Int] -> Either Text [SettlementStep]
settlementSequence confirmBeforeRollback rollbackSlot recipients = do
  unless (sort recipients == [0, 1, 2] && rollbackSlot `elem` [2, 3]) $ Left "unsupported settlement schedule"
  deposited <- chainAll "observe deposit" (observation 1 OnDepositTx{headId = testHeadId, depositTxId = 42, deposited = Set.singleton 42, created = initialChainTime, deadline = addUTCTime 600 initialChainTime}) start
  activated <- chainAll "activate deposit" (ChainInput $ Tick{chainTime = now, chainPoint = 2}) deposited
  confirmed <- drain 40 activated
  commit <- maybe (Left "settlement fixture has no nodes") Right $ listToMaybe $ confirmedSnapshots confirmed
  let increment slot = observation slot OnIncrementTx{headId = testHeadId, newVersion = 1, depositTxId = 42, snapshotNumber = 1, signatures = signaturesOf commit}
  checkConfirmed 1 (Set.singleton 1) confirmed
  unless ((getSnapshot commit).utxoToCommit == Just (Set.singleton 42)) $ Left "deposit snapshot did not claim the deposit"
  settled <- chainAll "observe increment" (increment 3) confirmed
  spent <- foldlM (\run recipient -> step "spend deposit" recipient (NetworkInput 2 $ ReceivedMessage alice $ ReqTx spend) run) settled [0 .. 2]
  beforeRollback <- if confirmBeforeRollback then drain 40 spent else Right spent
  rolled <- chainAll "rollback" (ChainInput $ Rollback{rolledBackChainState = SimpleChainState $ fromIntegral rollbackSlot, chainTime = now}) beforeRollback
  preserveL2 beforeRollback rolled
  let rollbackSteps = take 3 rolled.steps
  forM_ rollbackSteps $ \entry -> do
    let reposts = [snapshot | Continue{effects} <- [entry.outcome], OnChainEffect{postChainTx = IncrementTx{incrementingSnapshot = snapshot}} <- effects]
    unless (reposts == [commit | rollbackSlot == 2]) $ Left "rollback did not repost exactly the retained signed increment"
  reobserved <-
    foldlM
      ( \run recipient -> do
          next <- step "re-observe increment" recipient (increment 5) run
          preserveL2 run next
          deliverOne next
      )
      rolled
      recipients
  final <- drain 40 reobserved
  checkConfirmed 2 (Set.fromList [1, 43]) final
  forM_ final.nodes $ \node -> do
    let c = (openState node).coordinatedHeadState
    unless (c.localUTxO == Set.fromList [1, 43] && null c.localTxs && c.version == 1) $ Left "spent deposit resurrected or L2 work was lost"
    unless (Map.notMember 42 node.pendingDeposits) $ Left "settled deposit remains pending"
    unless (fmap (getSnapshot . (.snapshot)) c.finalizedCommit == Just (getSnapshot commit)) $ Left "signed increment history was lost"
    unless (fmap (.observedAtSlot) c.finalizedCommit == Just 5) $ Left "increment re-observation did not refresh its retained slot"
  pure $ reverse final.steps
 where
  now = addUTCTime 62 initialChainTime
  start = Run (replicate 3 initialNode) [] []
  spend = SimpleTx 43 (Set.singleton 42) (Set.singleton 43)
  observation slot observedTx = ChainInput $ Observation observedTx (SimpleChainState slot)
  chainAll label input run = foldlM (\r recipient -> step label recipient input r) run [0 .. 2]
  step :: Text -> Int -> Input SimpleTx -> Run -> Either Text Run
  step label recipient input run = do
    let before = run.nodes !! recipient
        env = (environment recipient){depositPeriod = 60, depositActivation = 60}
        outcome = update env simpleLedger now before input
        after = aggregateState before outcome
        changes = case outcome of Continue events _ -> events; Wait _ events -> events; Error{} -> []
        emitted = [Delivery recipient target 2 msg | Continue{effects} <- [outcome], NetworkEffect msg <- effects, target <- [0 .. 2]]
    case outcome of Error err -> Left $ "settlement sequence rejected " <> label <> ": " <> show err; Wait reason _ -> Left $ "settlement sequence waited " <> label <> ": " <> show reason; _ -> pure ()
    decoded <- first show $ decodeFull' @[StateChanged SimpleTx] $ serialize' changes
    unless (foldl' aggregateNodeState before decoded == after) $ Left "settlement event replay differs"
    checkpoint <- first show $ decodeFull' $ serialize' after
    unless (checkpoint == after) $ Left "settlement checkpoint round trip differs"
    pure Run{nodes = take recipient run.nodes <> [after] <> drop (recipient + 1) run.nodes, messages = run.messages <> emitted, steps = SettlementStep{label, recipient, input, outcome, before, after} : run.steps}
  drain remaining run = case run.messages of
    [] -> Right run
    _
      | remaining > (0 :: Int) -> deliverOne run >>= drain (remaining - 1)
      | otherwise -> Left "settlement delivery limit reached"
  deliverOne :: Run -> Either Text Run
  deliverOne run = case run.messages of
    [] -> Right run
    d : rest -> step "deliver snapshot message" d.recipientIndex (NetworkInput d.remainingTTL $ ReceivedMessage (scenarioParties !! d.senderIndex) d.message) run{messages = rest}
  preserveL2 :: Run -> Run -> Either Text ()
  preserveL2 before after = forM_ (zip before.nodes after.nodes) $ \(a, b) -> do
    let old = (openState a).coordinatedHeadState
        new = (openState b).coordinatedHeadState
    unless ((old.localUTxO, old.localTxs, old.allTxs, old.confirmedSnapshot, old.seenSnapshot, old.version) == (new.localUTxO, new.localTxs, new.allTxs, new.confirmedSnapshot, new.seenSnapshot, new.version)) $ Left "chain convergence changed retained L2 state"
  checkConfirmed :: SnapshotNumber -> Set SimpleTxOut -> Run -> Either Text ()
  checkConfirmed number utxo run = forM_ run.nodes $ \node -> do
    let confirmed = (openState node).coordinatedHeadState.confirmedSnapshot
        snapshot = getSnapshot confirmed
    unless (snapshot.number == number && snapshot.utxo == utxo) $ Left "unexpected confirmed snapshot"
    unless (snapshot.version == (if number == 1 then 0 else 1) && snapshot.confirmed == [spend | number == 2]) $ Left "unexpected confirmed version or transactions"
    unless (all ((== snapshot) . getSnapshot) (confirmedSnapshots run)) $ Left "parties disagree on the complete confirmed snapshot"
    unless (verifyMultiSignature (vkey <$> scenarioParties) (signaturesOf confirmed) snapshot == Verified) $ Left "invalid confirmed signatures"
  signaturesOf :: ConfirmedSnapshot SimpleTx -> MultiSignature (Snapshot SimpleTx)
  signaturesOf = \case ConfirmedSnapshot _ signatures -> signatures; InitialSnapshot{} -> mempty
  confirmedSnapshots :: Run -> [ConfirmedSnapshot SimpleTx]
  confirmedSnapshots run = map ((.confirmedSnapshot) . (.coordinatedHeadState) . openState) run.nodes
