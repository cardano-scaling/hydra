{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.SnapshotDecommitSpec (spec) where

import Hydra.HeadLogic.Cardano (cardanoSnapshotLimits)
import Hydra.Prelude
import Test.Hydra.Prelude (Spec, describe, it, shouldBe, shouldSatisfy)

import Cardano.Binary (decodeFull', serialize')
import Data.Map.Strict qualified as Map
import Data.Secret (Secret)
import Hydra.API.ClientInput qualified as Client
import Hydra.Agda.OffChainReference qualified as Agda
import Hydra.Chain (ChainEvent (Observation), OnChainTx (OnDecrementTx))
import Hydra.HeadLogic.Error (LogicError (RequireFailed), RequirementFailure (InvalidMultisignature))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Rules.SnapshotRequest (DecommitStatus (..), nextSnapshotRequest)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (Open), OpenState (..), SeenSnapshot (SeenSnapshot))
import Hydra.Ledger.Simple (SimpleChainState (SimpleChainState), SimpleTx (..), simpleLedger)
import Hydra.Network.Message (Message (..), NetworkEvent (ReceivedMessage))
import Hydra.Node.HeadLogic qualified as Head
import Hydra.Node.State (NodeState (..), initialChainTime)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (HydraKey, SigningKey, aggregateInOrder, sign)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, testHeadId)

spec :: Spec
spec = describe "snapshot decommit retention" $ do
  it "carries the decommit into the next round when ReqSn overtakes ReqDec" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    confirmed <- confirm requested
    let outcome = run confirmed bob $ ReqTx ordinary
        afterTx = Head.aggregateState confirmed outcome
        expected = ReqSn 0 2 [20] (Just decommit) Nothing
    requests outcome `shouldBe` [expected]
    requestedAgain <- deliver afterTx bob expected
    confirmedAgain <- confirm requestedAgain
    let current = (Scenario.openState confirmedAgain).coordinatedHeadState
    current.decommitTx `shouldBe` Just decommit
    (getSnapshot current.confirmedSnapshot).number `shouldBe` 2
    (getSnapshot current.confirmedSnapshot).utxo `shouldBe` utxoRef 20

  it "retains the candidate separately until confirmation, including delayed ReqDec" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    let current = (Scenario.openState requested).coordinatedHeadState
    current.decommitTx `shouldBe` Nothing
    Map.lookup 10 current.allTxs `shouldBe` Just decommit
    Head.aggregateState requested (run requested alice $ ReqDec decommit) `shouldBe` requested
    confirmed <- confirm requested
    let confirmedState = (Scenario.openState confirmed).coordinatedHeadState
    confirmedState.decommitTx `shouldBe` Just decommit
    Map.lookup 10 confirmedState.allTxs `shouldBe` Nothing
    Head.aggregateState confirmed (run confirmed alice $ ReqDec decommit) `shouldBe` confirmed

  it "keeps a different reservation until confirmation then promotes the candidate" $ do
    let other = SimpleTx 11 (utxoRef 2) (utxoRef 11)
    reserved <- deliver initial bob $ ReqDec other
    requested <- deliver reserved alice $ ReqSn 0 1 [] (Just decommit) Nothing
    (Scenario.openState requested).coordinatedHeadState.decommitTx `shouldBe` Just other
    (Scenario.openState requested).coordinatedHeadState.localUTxO `shouldBe` mempty
    confirmed <- confirm requested
    (Scenario.openState confirmed).coordinatedHeadState.decommitTx `shouldBe` Just decommit
    (Scenario.openState confirmed).coordinatedHeadState.localUTxO `shouldBe` utxoRef 2

  it "a rejected final signature cannot promote the payload or release a reservation" $ do
    let other = SimpleTx 11 (utxoRef 2) (utxoRef 11)
    reserved <- deliver initial bob $ ReqDec other
    requested <- deliver reserved alice $ ReqSn 0 1 [] (Just decommit) Nothing
    snapshot <- candidateOf requested
    withAlice <- deliver requested alice $ AckSn (sign aliceSk snapshot) 1
    withBob <- deliver withAlice bob $ AckSn (sign bobSk snapshot) 1
    let rejected = run withBob carol $ AckSn (sign aliceSk snapshot) 1
    rejected `shouldSatisfy` \case Head.Error (RequireFailed InvalidMultisignature{}) -> True; _ -> False
    Head.aggregateState withBob rejected `shouldBe` withBob
    let current = (Scenario.openState withBob).coordinatedHeadState
    current.decommitTx `shouldBe` Just other
    Map.lookup 10 current.allTxs `shouldBe` Just decommit

  it "duplicate and expired ReqTx cannot evict the candidate's transaction" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    forM_ [0, 2] $ \ttl -> do
      let duplicate = Head.update (Scenario.environment 1) simpleLedger initialChainTime requested $ Head.NetworkInput ttl $ ReceivedMessage alice $ ReqTx decommit
      duplicate `shouldBe` Head.Continue [] []
      Head.aggregateState requested duplicate `shouldBe` requested
    void $ confirm requested

  it "settlement before the final signature retires the retained transaction" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    let observation = Head.ChainInput $ Observation (OnDecrementTx testHeadId 1 (utxoRef 10) 1 mempty) (SimpleChainState 1)
        outcome = Head.update (Scenario.environment 1) simpleLedger initialChainTime requested observation
    observed <- accept requested outcome
    (Scenario.openState observed).coordinatedHeadState.version `shouldBe` 1
    confirmed <- confirm observed
    let current = (Scenario.openState confirmed).coordinatedHeadState
    current.decommitTx `shouldBe` Nothing
    Map.lookup 10 current.allTxs `shouldBe` Nothing
    current.localUTxO `shouldBe` utxoRef 2
    let next = run confirmed bob $ ReqTx ordinary
    requests next `shouldBe` [ReqSn 1 2 [20] Nothing Nothing]

  it "side-loading and clearing local work preserve the authorised payload" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    confirmed <- confirm requested
    let signed = (Scenario.openState confirmed).coordinatedHeadState.confirmedSnapshot
        outcome = Head.update (Scenario.environment 1) simpleLedger initialChainTime confirmed $ Head.ClientInput $ Client.SideLoadSnapshot signed
    cleared <- accept confirmed outcome
    (Scenario.openState cleared).coordinatedHeadState.decommitTx `shouldBe` Just decommit
    (Scenario.openState cleared).coordinatedHeadState.allTxs `shouldBe` mempty

  it "does not request a round when a legacy checkpoint lacks the authorised payload" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    confirmed <- confirm requested
    let open = Scenario.openState confirmed
        old = confirmed{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{decommitTx = Nothing}}}
        outcome = run old bob $ ReqTx ordinary
        after = Head.aggregateState old outcome
    requests outcome `shouldBe` []
    (Scenario.openState after).coordinatedHeadState.seenSnapshot `shouldBe` open.coordinatedHeadState.seenSnapshot

  it "rejects missing or mismatched carry proposals before retaining a request marker" $ do
    requested <- deliver initial alice $ ReqSn 0 1 [] (Just decommit) Nothing
    confirmed <- confirm requested
    let snapshot = getSnapshot (Scenario.openState confirmed).coordinatedHeadState.confirmedSnapshot
        other = SimpleTx 11 (utxoRef 2) (utxoRef 11)
    forM_ [NoDecommit, AwaitingInputs other, ReadyToDecommit other, CarryDecommit other] $ \status ->
      isJust (nextSnapshotRequest cardanoSnapshotLimits 0 snapshot mempty mempty Nothing status Nothing) `shouldBe` False
    isJust (nextSnapshotRequest cardanoSnapshotLimits 0 snapshot mempty mempty Nothing (CarryDecommit decommit) Nothing) `shouldBe` True
    isJust (nextSnapshotRequest cardanoSnapshotLimits 1 snapshot mempty mempty Nothing NoDecommit Nothing) `shouldBe` True
    stale <- maybe (fail "no stale proposal") pure $ nextSnapshotRequest cardanoSnapshotLimits 0 snapshot{utxoToDecommit = Nothing} mempty mempty Nothing NoDecommit Nothing
    let (events, runtime) = Coordinated.step (Coordinated.start $ Scenario.openState confirmed) $ Coordinated.requestSnapshot stale
    events `shouldBe` []
    Coordinated.currentState runtime `shouldBe` (Scenario.openState confirmed).coordinatedHeadState

  it "agrees with Agda on bounded payload selection and the carry obligation" $
    forM_ [False, True] $ \sameVersion ->
      forM_ [[], [1], [2], [1, 2]] $ \outputs ->
        forM_ [[], [1], [2], [1, 2]] $ \reservedOutputs ->
          forM_ [[], [[1]], [[2], [1]], [[1, 2], [2]]] $ \registryOutputs -> do
            let toUTxO = fromList . fmap fromInteger
                snapshot :: Snapshot SimpleTx
                snapshot = Scenario.initialSnapshot{utxoToDecommit = Just $ toUTxO outputs}
                reserved = SimpleTx 0 mempty (toUTxO reservedOutputs)
                registry = [SimpleTx index mempty (toUTxO refs) | (index, refs) <- zip [1 ..] registryOutputs]
                open = Scenario.openState initial
                base = open{coordinatedHeadState = open.coordinatedHeadState{version = if sameVersion then 0 else 1, decommitTx = reserved <$ guard sameVersion, allTxs = Map.fromList [(tx.txSimpleId, tx) | tx <- registry]}}
                runtime = Coordinated.replayEvent mempty (Head.SnapshotConfirmed testHeadId (Just snapshot) mempty) $ Coordinated.start base
                actual = Coordinated.currentState runtime
                expected = Agda.carriedDecommitPayloadRef sameVersion outputs reservedOutputs registryOutputs
            ((.txSimpleId) <$> actual.decommitTx) `shouldBe` expected
            forM_ [NoDecommit, CarryDecommit reserved] $ \status -> do
              let matching = case status of CarryDecommit{} -> not (null outputs) && outputs == reservedOutputs; _ -> False
                  proposal = nextSnapshotRequest cardanoSnapshotLimits (if sameVersion then 0 else 1) snapshot mempty mempty Nothing status Nothing
              isJust proposal `shouldBe` Agda.carriedDecommitRequestRef sameVersion True matching

initial :: NodeState SimpleTx
initial =
  let utxo = utxoRef 1 <> utxoRef 2
      base :: Snapshot SimpleTx
      base = Scenario.initialSnapshot{utxo, accumulator = Accumulator.buildFromUTxO @SimpleTx utxo, appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx utxo}
      signed = aggregateInOrder (Map.fromList [(p, sign sk base) | (p, sk) <- signers]) Scenario.scenarioParties
      open = Scenario.openState Scenario.initialNode
   in Scenario.initialNode{headState = Open open{coordinatedHeadState = open.coordinatedHeadState{localUTxO = utxo, confirmedSnapshot = ConfirmedSnapshot base signed}}}

decommit, ordinary :: SimpleTx
decommit = SimpleTx 10 (utxoRef 1) (utxoRef 10)
ordinary = SimpleTx 20 (utxoRef 2) (utxoRef 20)

signers :: [(Party, Secret (SigningKey HydraKey))]
signers = [(alice, aliceSk), (bob, bobSk), (carol, carolSk)]

run :: NodeState SimpleTx -> Party -> Message SimpleTx -> Head.Outcome SimpleTx
run node sender msg = Head.update (Scenario.environment 1) simpleLedger initialChainTime node $ Head.NetworkInput 2 $ ReceivedMessage sender msg

deliver :: NodeState SimpleTx -> Party -> Message SimpleTx -> IO (NodeState SimpleTx)
deliver node sender msg = accept node $ run node sender msg

-- Replay serialized events and restore a checkpoint at every retained prefix.
accept :: NodeState SimpleTx -> Head.Outcome SimpleTx -> IO (NodeState SimpleTx)
accept node = \case
  outcome@(Head.Continue events _) -> do
    next <- foldlM replay node events
    next `shouldBe` Head.aggregateState node outcome
    pure next
  outcome -> fail $ show outcome
 where
  replay :: NodeState SimpleTx -> Head.StateChanged SimpleTx -> IO (NodeState SimpleTx)
  replay before event = do
    decoded <- either (fail . show) pure $ decodeFull' (serialize' event)
    decoded `shouldBe` event
    let next = Head.aggregateNodeState before decoded
    restored <- either (fail . show) pure $ decodeFull' (serialize' next)
    restored `shouldBe` next
    pure restored

candidateOf :: NodeState SimpleTx -> IO (Snapshot SimpleTx)
candidateOf node = case (Scenario.openState node).coordinatedHeadState.seenSnapshot of
  SeenSnapshot candidate _ _ -> pure candidate
  _ -> fail "no candidate snapshot"

confirm :: NodeState SimpleTx -> IO (NodeState SimpleTx)
confirm node = do
  candidate <- candidateOf node
  foldlM (\current (p, sk) -> deliver current p $ AckSn (sign sk candidate) candidate.number) node signers

requests :: Head.Outcome SimpleTx -> [Message SimpleTx]
requests = \case
  Head.Continue _ effects -> [message | Head.NetworkEffect message@ReqSn{} <- effects]
  _ -> []
