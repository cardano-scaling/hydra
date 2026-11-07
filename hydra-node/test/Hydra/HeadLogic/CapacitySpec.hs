{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.CapacitySpec (spec) where

import Hydra.Prelude hiding (local)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Hydra.API.ClientInput qualified as Client
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.HeadLogic (aggregateState, update)
import Hydra.HeadLogic.Input (Input (ClientInput, NetworkInput))
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (NetworkEffect), Outcome (Continue), StateChanged (DecommitInvalid, DecommitRecorded, SnapshotRequestDecided, SnapshotRequested))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotCapacity (checkSnapshotCapacity)
import Hydra.HeadLogic.Rules.SnapshotRequest (DecommitStatus (CarryDecommit, NoDecommit), SettlingCommit (ContinueCommit), nextSnapshotRequest, requestDepositTxId, requestMessage, requestTxs)
import Hydra.HeadLogic.Rules.Transactions (applyTransactionsWithoutCollision, pruneTransactions)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (RequestedSnapshot), mkSeenSnapshot)
import Hydra.Ledger.Simple (SimpleTx (..), SimpleTxOut (..), simpleLedger)
import Hydra.Network.Message (Message (AckSn, ReqDec, ReqSn, ReqTx), NetworkEvent (ReceivedMessage))
import Hydra.Node.State (Deposit (..), DepositStatus (Active), NodeState (headState), PendingDeposits, initialChainTime)
import Hydra.Tx (HeadId (UnsafeHeadId), applyTxTo, txId)
import Hydra.Tx.Accumulator (maxAccumulatorSize)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..))
import Test.Hydra.HeadLogic.Capacity qualified as Capacity
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef, utxoRefs)
import Test.Hydra.Tx.Fixture (alice, testHeadId)

spec :: Spec
spec = describe "snapshot capacity" $ do
  it "reproduces the historical stall and confirms the independent merge after pruning" $ do
    Capacity.demonstration True `shouldSatisfy` isRight
    Capacity.demonstration False `shouldSatisfy` isRight

  it "checks both fanout alternatives, counting overlapping references once" $ do
    Agda.accumulatorCapacityRef `shouldBe` toInteger maxAccumulatorSize
    forM_ [(a, c, d) | a <- subsequences [1, 2], c <- subsequences [2, 3], d <- subsequences [3, 4]] $ \(a, c, d) ->
      isRight (checkSnapshotCapacity @SimpleTx (padding 3 <> refs a) (Just $ refs c) (Just $ refs d))
        `shouldBe` Agda.capacityFitsRef 3 a c d
    forM_ [(a, b) | a <- [maxAccumulatorSize - 1 .. maxAccumulatorSize + 1], b <- [maxAccumulatorSize - 1 .. maxAccumulatorSize + 1]] $ \(a, b) ->
      isRight (checkSnapshotCapacity @SimpleTx mempty (Just $ refs [1 .. toInteger a]) (Just $ refs [1 .. toInteger b]))
        `shouldBe` Agda.snapshotCapacityRef (toInteger a) (toInteger b)

  it "agrees with small-capacity Agda selection and pruning at the deployed limit" $
    forM_ [(limit, commit, txs) | limit <- [3, 4], commit <- [[], [8]], 3 + length commit <= limit, txs <- concatMap (`replicateM` palette) [0 .. 3]] $ \(limit, commit, txs) -> do
      let active = padding limit <> refs [1, 2, 3]
          base = Scenario.initialSnapshot{utxo = active}
          held = refs commit
          deposit = Deposit testHeadId held initialChainTime (addUTCTime 100 initialChainTime) Active
          settling = if null commit then Nothing else Just $ ContinueCommit (9, deposit)
          spentInputs = map (identifiers . (.txInputs)) txs
          outputs = map (identifiers . (.txOutputs)) txs
          request = nextSnapshotRequest 0 base (Seq.fromList txs) mempty Nothing NoDecommit settling
          pruning = do
            kept <- pruneTransactions 0 (Just held) Nothing active (Seq.fromList txs)
            Program.change $ SnapshotRequested base kept Nothing
          expected = Agda.capacityPruneRef (toInteger limit) [1, 2, 3] commit [] spentInputs outputs
      -- Selection consumes an already admitted sequence. Pruning also covers
      -- invalid dependencies and repeated/colliding outputs after a rebase.
      when (isRight $ applyTransactionsWithoutCollision simpleLedger 0 active txs) $
        fmap (toInteger . length . requestTxs) request
          `shouldBe` Just (Agda.capacityPrefixRef (toInteger limit) [1, 2, 3] commit spentInputs outputs)
      case Interpreter.runProgramWith (Interpreter.snapshotConfig (Scenario.environment 0) simpleLedger) pruning of
        Continue [SnapshotRequested _ kept _] [] ->
          toList kept `shouldBe` [tx | (index, tx) <- zip [1 ..] txs, index `elem` expected]
        _ -> fail "pruning does not produce its retained sequence"

  it "lets a new deposit use space freed by a merge and defers one that does not fit" $ do
    let base = Scenario.initialSnapshot{utxo = padding 3 <> refs [1, 2, 3]}
        merge = SimpleTx 2 (refs [2, 3]) (utxoRef 6)
        deposit :: Set SimpleTxOut -> PendingDeposits SimpleTx
        deposit u = Map.singleton 9 $ Deposit testHeadId u initialChainTime (addUTCTime 100 initialChainTime) Active
        select pending txs = nextSnapshotRequest 0 base (Seq.fromList txs) pending Nothing NoDecommit Nothing
    fmap requestDepositTxId (select (deposit $ utxoRef 8) [merge]) `shouldBe` Just (Just 9)
    fmap requestDepositTxId (select (deposit $ refs [8, 9]) [merge]) `shouldBe` Just Nothing
    fmap requestTxs (select (deposit $ refs [8, 9]) [merge]) `shouldBe` Just [merge]
    fmap requestDepositTxId (select (deposit $ utxoRef 8) []) `shouldBe` Just Nothing

  it "reserves fanout capacity for a carried decommit" $ do
    let base = Scenario.initialSnapshot{utxo = padding 3 <> refs [1, 2], utxoToDecommit = Just $ utxoRef 9}
        decommit = SimpleTx 3 (utxoRef 8) (utxoRef 9)
        split = SimpleTx 1 (utxoRef 1) (refs [4, 5])
    fmap requestTxs (nextSnapshotRequest 0 base (Seq.singleton split) mempty Nothing (CarryDecommit decommit) Nothing) `shouldBe` Just []

  it "keeps competing optimistic transactions for a delayed valid snapshot at TTL zero" $ do
    let active = padding 3 <> refs [1, 2]
        split = SimpleTx 1 (utxoRef 1) (refs [4, 5])
        competing = SimpleTx 2 (utxoRef 2) (refs [6, 7])
        open = Scenario.openState Scenario.initialNode
        initial = Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{localUTxO = active, confirmedSnapshot = ConfirmedSnapshot Scenario.initialSnapshot{utxo = active} mempty}}}
        run node message = update (Scenario.environment 1) simpleLedger initialChainTime node (NetworkInput 0 $ ReceivedMessage alice message)
        receive node message = aggregateState node $ run node message
        optimistic = receive (receive initial $ ReqTx split) $ ReqTx competing
        c = (Scenario.openState optimistic).coordinatedHeadState
        signed = run optimistic $ ReqSn 0 1 [txId competing] Nothing Nothing
        after = (Scenario.openState $ aggregateState optimistic signed).coordinatedHeadState
    Set.size c.localUTxO `shouldBe` maxAccumulatorSize + 1
    Map.lookup (txId split) c.allTxs `shouldBe` Just split
    Map.lookup (txId competing) c.allTxs `shouldBe` Just competing
    signed `shouldSatisfy` \case Continue _ effects -> any (\case NetworkEffect AckSn{} -> True; _ -> False) effects; _ -> False
    after.localUTxO `shouldBe` applyTxTo competing active
    after.localTxs `shouldBe` mempty

  it "rejects a stale or in-flight proposal without recording or broadcasting it" $ do
    request <- maybe (fail "no proposal") pure $ nextSnapshotRequest 0 Scenario.initialSnapshot mempty mempty Nothing NoDecommit Nothing
    let open = Scenario.openState Scenario.initialNode
        current = open.coordinatedHeadState
        run before = Interpreter.runProgramWith Interpreter.basicConfig $ sendSnapshot (Coordinated.start before) request
        changedSnapshot :: Snapshot SimpleTx -> OpenState SimpleTx
        changedSnapshot snapshot = open{coordinatedHeadState = current{confirmedSnapshot = ConfirmedSnapshot snapshot mempty}}
        otherHead :: OpenState SimpleTx
        otherHead = open{headId = UnsafeHeadId "another head", coordinatedHeadState = current{confirmedSnapshot = ConfirmedSnapshot Scenario.initialSnapshot{headId = UnsafeHeadId "another head"} mempty}}
        stale :: [OpenState SimpleTx]
        stale =
          [ open{coordinatedHeadState = current{version = 1}}
          , changedSnapshot Scenario.initialSnapshot{number = 1}
          , otherHead
          , open{coordinatedHeadState = current{seenSnapshot = RequestedSnapshot 0 1}}
          , open{coordinatedHeadState = current{seenSnapshot = mkSeenSnapshot Scenario.initialSnapshot{number = 1} mempty}}
          ]
    run open `shouldBe` Continue [SnapshotRequestDecided 1] [NetworkEffect $ requestMessage request]
    forM_ stale $ \before -> do
      run before `shouldBe` Continue [] []
      fst (Coordinated.step (Coordinated.start before) $ Coordinated.requestSnapshot request) `shouldBe` []

  it "checks decommit capacity against both local and confirmed UTxOs on both entry paths" $ do
    let large = padding 3 <> refs [1, 2, 3]
        small = padding 3 <> refs [1, 6]
        split = SimpleTx 1 (utxoRef 1) (refs [4, 5])
        noOutputs = SimpleTx 1 (utxoRef 1) mempty
    forM_ [(large, small), (small, large), (large, large)] $ \(local, confirmed) ->
      forM_ (inputs split) $ \input -> runAt local confirmed input `shouldSatisfy` rejectedDecommit
    forM_ (inputs noOutputs) $ \input -> runAt small small input `shouldSatisfy` rejectedDecommit
    runAt small small (ClientInput $ Client.Decommit split) `shouldBe` Continue [] [NetworkEffect $ ReqDec split]
    runAt small small (NetworkInput 0 $ ReceivedMessage alice $ ReqDec split)
      `shouldSatisfy` \case Continue [DecommitRecorded{}] _ -> True; _ -> False
    forM_ [(local, confirmed, tx) | local <- [large, small], confirmed <- [large, small], tx <- [noOutputs, split]] $ \(local, confirmed, tx) ->
      not (rejectedDecommit $ runAt local confirmed $ ClientInput $ Client.Decommit tx)
        `shouldBe` Agda.decommitCapacityRef (toInteger $ Set.size tx.txOutputs) (toInteger $ Set.size $ applyTxTo tx local) (toInteger $ Set.size $ applyTxTo tx confirmed)

  it "matches the extracted pruning predicate for each rejection cause" $
    forM_ [(valid, disjoint, fits) | valid <- [False, True], disjoint <- [False, True], fits <- [False, True]] $ \(valid, disjoint, fits) -> do
      let active = padding 3 <> refs [1, 2]
          tx = SimpleTx 1 (utxoRef $ if valid then 1 else 9) (refs $ if fits then [4] else [4, 5, 6])
          commit = if disjoint then mempty else utxoRef 4
          after = applyTxTo tx active
          expected = Agda.pendingTxFitsRef valid disjoint (toInteger $ Set.size after) (toInteger $ Set.size $ after <> commit)
          program = do
            kept <- pruneTransactions 0 (Just commit) Nothing active (Seq.singleton tx)
            Program.change $ SnapshotRequested Scenario.initialSnapshot kept Nothing
      case Interpreter.runProgramWith (Interpreter.snapshotConfig (Scenario.environment 0) simpleLedger) program of
        Continue [SnapshotRequested _ kept _] [] -> not (null kept) `shouldBe` expected
        _ -> fail "pruning does not complete"
 where
  -- The small model never names these outputs. They reduce the free capacity
  -- without changing the production bound or constructing fake commitments.
  padding limit = Set.fromList $ SimpleTxOut . fromIntegral <$> [100 .. 100 + maxAccumulatorSize - limit - 1]
  refs = Set.fromList . map fromInteger
  identifiers = map unSimpleTxOut . Set.toList
  palette = [SimpleTx 1 (utxoRef 1) (utxoRefs [4, 5]), SimpleTx 2 (utxoRefs [2, 3]) (utxoRef 6), SimpleTx 3 (utxoRefs [4, 5]) (utxoRef 7)]
  inputs :: SimpleTx -> [Input SimpleTx]
  inputs tx = [ClientInput $ Client.Decommit tx, NetworkInput 0 $ ReceivedMessage alice $ ReqDec tx]
  runAt local confirmed input =
    let open = Scenario.openState Scenario.initialNode
        current = open.coordinatedHeadState{localUTxO = local, confirmedSnapshot = ConfirmedSnapshot Scenario.initialSnapshot{utxo = confirmed} mempty}
     in update (Scenario.environment 0) simpleLedger initialChainTime Scenario.initialNode{headState = Open open{coordinatedHeadState = current}} input
  rejectedDecommit :: Outcome SimpleTx -> Bool
  rejectedDecommit = \case
    Continue [DecommitInvalid{}] [] -> True
    _ -> False
