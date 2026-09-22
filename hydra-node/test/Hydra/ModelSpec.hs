{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Model-Based testing of Hydra Head protocol implementation.
--
-- * Troubleshooting
--
-- ** Deadlocks
--
-- One of the most annoying problems one can face with those very high level properties involving multithreading and a lot
-- of complex moving parts is when the test execution deadlocks. Here is a short guide on what one can do to troubleshoort
-- this kind of issue:
--
-- * **Check generators**: `suchThat` combinator from QuickCheck is useful when one wants to refine another `Gen`erator's behaviour
--   but it can lead to deadlock if the filtering leads to no value being generated. Avoid it.
--
-- * **Dump nodes' logs**: In case of a "normal" failure of the tests, the logs from the nodes are dumped. However, if the test does
--   not even complete then no logs are produced because they are kept in memory. In this case. replacing `traceInIOSim` with
--   `traceInIOSim <> traceDebug` will ensure the logs are dumped on the `stderr`. It could be a good idea to store them in a file
--   as they can be quite large.
--
-- * **Use** `Debug.Trace.trace` liberally: Because getting a proper stack trace is hard in Haskell, esp. in pure code, sprinkling
--   `trace` statements at key points might help understand what's going on and zoom in on the culprits
--
-- * **Dump IOSim trace**: In case the deadlock (or race condition) is caused by having two or more concurrent threads competing
--   to access a resource, dumping the trace of IOSim's runtime scheduleer execution can help. io-sim generate its trace lazily which
--   means that even when it deadlocks, one can capture at least a significant prefix of the trace and dump it to `stderr`. One can
--   `map (\ t -> trace (ppEvents t) t) . traceEvents` over the `SimTrace` returned by `runSimTrace` to get some pretty-printed
--   output similar to:
--
--   @@
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventThrow AsyncCancelled
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventMask MaskedInterruptible
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventMask MaskedInterruptible
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventDeschedule Interruptible
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventTxCommitted [Labelled (TVarId 25) (Just "async-ThreadId [4]")] [] Nothing
--   Time 380.1s - ThreadId []   main          - EventTxWakeup [Labelled (TVarId 25) (Just "async-ThreadId [4]")]
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventUnblocked [ThreadId []]
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventDeschedule Yield
--   Time 380.1s - ThreadId []   main          - EventTxCommitted [] [] Nothing
--   Time 380.1s - ThreadId []   main          - EventUnblocked []
--   Time 380.1s - ThreadId []   main          - EventDeschedule Yield
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventThreadFinished
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventDeschedule Terminated
--   Time 380.1s - ThreadId []   main          - EventThreadFinished
--   @@
--
-- ** Recording trace failures
--
-- When a property fails it will dump the sequence of actions leading to the
-- failure:
--
-- @@
--   do action $ Seed {seedKeys = [("8bbc9f32e4faff669ed1561025f243649f1332902aa79ad7e6e6bbae663f332d",CardanoSigningKey {signingKey = "0400020803030302070808060405040001050408070401040604000005010603"})], seedContestationPeriod = 46s, seedDepositDeadline = 50s}
--      var2 <- action $ Init (Party {vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"})
--      action $ Deposit {headIdVar = var2, utxoToDeposit = [(CardanoSigningKey {signingKey = "0400020803030302070808060405040001050408070401040604000005010603"},valueFromList [(AdaAssetId,54862683)])], deadline = 1864-06-06 08:24:08.669152896211 UTC}
--      pure ()
-- @@
--
-- Which can be turned into a unit test after resolving most of the imports.
-- Common pitfalls are incorrect show instances (e.g. the UTCTime in deadline
-- above). Should the variables not be bound correctly, double check
-- HasVariables instances. A working example of the above output would be:
--
-- @@
--   it "troubleshoot" . withMaxSuccess 1 . flip forAllDL propHydraModel $ do
--     action $ Seed{seedKeys = [("8bbc9f32e4faff669ed1561025f243649f1332902aa79ad7e6e6bbae663f332d", CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"})], seedContestationPeriod = UnsafeContestationPeriod 46, seedDepositDeadline = UnsafeDepositDeadline 50}
--     var2 <- action $ Init (Party{vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"})
--     action $ Deposit{headIdVar = var2, utxoToDeposit = [(CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"}, valueFromList [(AdaAssetId, 54862683)])], deadline = read "1864-06-06 08:24:08.669152896211 UTC"}
--     pure ()
-- @@
module Hydra.ModelSpec where

import Hydra.Cardano.Api hiding (CardanoSigningKey (..))
import Hydra.Prelude
import Test.Hydra.Prelude hiding (after)

import Cardano.Api.UTxO qualified as UTxO
import Control.Monad.Class.MonadTimer ()
import Control.Monad.IOSim (Failure (FailureException), IOSim, SimTrace, runSimTrace, traceResult)
import Data.List (nub, (\\))
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Typeable (cast)
import Hydra.API.ServerOutput (ServerOutput (SnapshotConfirmed, snapshot))
import Hydra.BehaviorSpec (RequeueMode (..), TestHydraClient (..), dummySimulatedChainNetwork)
import Hydra.Logging.Messages (HydraLog)
import Hydra.Model (
  Action (..),
  GlobalState (..),
  Nodes (Nodes, nodes),
  OffChainState (..),
  RunMonad,
  RunState (..),
  WorldState (..),
  genFaultySeed,
  genPartyKeysExactly,
  genPayment,
  genSeedWith,
  headUTxO,
  runMonad,
  toRealUTxO,
  toTxOuts,
 )
import Hydra.Model qualified as Model
import Hydra.Model.MockChain (FaultMode (..), bogusDepositId, deviateFrom)
import Hydra.Model.Payment (Payment (..))
import Hydra.Model.Payment qualified as Payment
import Hydra.Network.Message (Message (..))
import Hydra.Tx (HeadId)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.IsTx (UTxOType)
import Hydra.Tx.IsTx qualified as IsTx
import Hydra.Tx.Party (Party (..), deriveParty)
import Hydra.Tx.Snapshot (Snapshot (..))
import System.IO.Temp (writeSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (formatFailureReason)
import Test.Hydra.Node.Fixture (alice, aliceSk)
import Test.Hydra.Tx.Fixture (fanoutOutputThreshold)
import Test.QuickCheck (Property, Testable, conjoin, counterexample, forAllShrink, mapSize, noShrinking, property, suchThat, vectorOf, withMaxShrinks, withMaxSuccess, within)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.DynamicLogic (
  DL,
  Quantification,
  action,
  anyActions_,
  forAllDL,
  forAllNonVariableQ,
  forAllQ,
  getModelStateDL,
  whereQ,
  withGenQ,
 )
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic', run, stop)
import Test.QuickCheck.Property ((===))
import Test.QuickCheck.StateModel (
  ActionWithPolarity (..),
  Actions,
  Annotated (..),
  HasVariables (..),
  Step ((:=)),
  Var,
  precondition,
  runActions,
  pattern Actions,
 )
import Test.Util (printTrace, traceInIOSim)
import Text.Printf (printf)

instance HasVariables Payment.CardanoSigningKey where
  getAllVariables = mempty

spec :: Spec
spec = do
  context "modeling" $ do
    reportWalkActionMix
    prop "not generate actions with 0 Ada" $ withMaxSuccess 10000 propDoesNotGenerate0AdaUTxO
    prop "toRealUTxO is distributive" $ propIsDistributive toRealUTxO
    prop "toTxOuts is distributive" $ propIsDistributive toTxOuts
  -- The default random walk settles every deposit and decommit before the
  -- next action, see 'concurrentSettlements'.
  prop "check model" propHydraModel
  prop "check model balances" propCheckModelBalances
  -- This scenario seeds a head with a single party and an UTxO set of elements.
  -- See https://github.com/cardano-scaling/hydra/issues/2270
  context "fanout limit" $ do
    prop "succeeds fanout with many outputs" $ propFanoutLimit (fanoutOutputThreshold + 1)
    prop "succeeds fanout with few outputs" $ propFanoutLimit fanoutOutputThreshold
  context "logic" $ do
    prop "check conflict-free liveness" $ propDL conflictFreeLiveness
    prop "fanout contains whole confirmed UTxO" $ propDL fanoutContainsWholeConfirmedUTxO
    prop "parties contest to wrong closed snapshot" $ propDL partyContestsToWrongClosedSnapshot
  -- Scripted settlement/rollback interleavings. Each drives an open head into
  -- a specific settlement race, forks the chain and then requires the head to
  -- still close and fan out its whole confirmed UTxO. Only the keys are
  -- random, so a handful of runs each is enough.
  context "settlements under divergent forks" $ do
    prop "two finalized decrements are both erased by a fork" $
      propScripted twoFinalizedDecrementsErased
    prop "a fork erases the deposit transaction and its increment" $
      propScripted depositAndIncrementErasedThenRelanded
    prop "a second fork erases the re-posted increment" $
      propScripted rePostedIncrementErasedAgain
  -- Scripted fanouts of a head holding more outputs than one fanout
  -- transaction can distribute, so the node fans out in steps: driven
  -- automatically by 'Fanout' or one selection at a time by 'PartialFanout',
  -- with forks erasing steps along the way.
  context "partial fanout" $ do
    prop "a manual fanout distributes the selections in turn" $
      propScripted manualPartialFanout
  -- A crash and restart at the moment a settlement landed, see
  -- 'partyRestartedTwiceAfterSettlement'.
  context "fail-recovery" $ do
    prop "a party restarted twice right after a settlement still confirms snapshots" $
      propScripted partyRestartedTwiceAfterSettlement
    prop "a party misbehaving on the network does not stop the head" $
      propScripted headSurvivesFaultyParty
    prop "check model with a party misbehaving on the network" $
      forAllDL faultyWalk propHydraModel
    prop "a replaced proposal is never the one an honest leader would send" propReplacingDeviates
    -- The same party, now sending those deviations in place of the honest
    -- proposal rather than alongside it, so the head is allowed to stop and
    -- only what the parties confirmed before it did is judged. See
    -- 'checkSnapshotsAreSound'.
    prop "a party replacing its proposals cannot make the head lose track of funds" $
      within 60000000 $
        withMaxSuccess 50 $
          forAllDL soundnessWalk checkSnapshotsAreSound
  -- The concurrent random walk lets several deposits and decommits settle at
  -- the same time as L2 traffic and divergent forks, and the scripted
  -- settlement replays pin down the races it found. In the order it found
  -- them, and now guards against:
  --
  --   * A deposit that activated while a snapshot was in flight was never
  --     committed. It was parked in 'currentDepositTxId', the tick refused to
  --     act while anything was queued, and the request chained on a confirmed
  --     snapshot needed local transactions. It expired.
  --   * A ReqSn that reached a party after that party had seen the settlement
  --     it raced was parked on 'WaitOnSnapshotVersion' until its TTL dropped
  --     it, while the leader kept collecting signatures, which deadlocked the
  --     head. Such a party now signs at the proposed version, one behind its
  --     own (see 'waitOnSnapshotVersion').
  --   * A ReqDec was held back on a deposit queued locally and rejected once
  --     its TTL ran out. Whether a deposit is queued depends on the node's own
  --     tick, so the same request was refused by some parties and recorded by
  --     others, whose decommit was then never proposed.
  --   * After a rollback erased a settled increment while the snapshot
  --     claiming it was still being signed, the leader proposed that deposit
  --     again and every party refused the proposal.
  --
  -- Each one only showed up once the previous was fixed, so keep the walks
  -- enabled. A new counterexample here is a new bug, not a flake to retry.
  --
  -- The scripted fanout scenarios pin down the same kind of gap for a fanout
  -- in progress. After a fork erased a landed step, the node's bookkeeping was
  -- ahead of the chain. Automatic mode posted the next step instead of the
  -- erased one, which could not land, and manual mode posted nothing at all
  -- since it waited for the client, so the head was never fully fanned out.
  -- Each landed step is now recorded with its slot, and the progress is
  -- rewound to the steps still on chain ('rewindFanoutProgress').
  context "settlement and fanout rollback stress" $ do
    prop "check model with concurrent settlements" $
      forAllDL concurrentWalk propHydraModel
    prop "check model balances with concurrent settlements" $
      within 30000000 $
        forAllDL concurrentWalk checkModelBalances
    -- Heavy: run the deep-stress version only on nightly, where it does not
    -- compete with the rest of the suite for CPU (a starved io-sim schedule
    -- makes the driver's waits time out for no reason, cf. ServerSpec). It is
    -- the same generator as the walks above, only deeper.
    around_ onlyNightly $
      prop "check model balances under load with divergent forks @nightly" propStressModelBalances
    prop "two finalized increments are both erased by a fork" $
      propScripted twoFinalizedIncrementsErased
    prop "a finalized increment is erased while the next increment is in flight" $
      propScripted finalizedIncrementErasedWithNextInFlight
    prop "a finalized increment and decrement are both erased by a fork" $
      propScripted finalizedIncrementAndDecrementErased
    prop "new settlements requested during a replay settle in order" $
      propScripted newSettlementsDuringReplay
    prop "a fork erases a step of an automatic fanout" $
      propScripted autoFanoutStepErased
    prop "a fork erases two steps of an automatic fanout" $
      propScripted autoFanoutTwoStepsErased
    prop "a fork erases a step of a manual fanout" $
      propScripted manualFanoutStepErased

propFanoutLimit :: Int -> Property
propFanoutLimit limit =
  within 30000000 $ propDL $ do
    signingKeys <- forAllQ $ withGenQ (vectorOf limit (arbitrary @Payment.CardanoSigningKey)) (const True) (const [])
    let aliceCardanoSks = fromMaybe (error "propFanoutLimit: limit must be > 0") (nonEmpty signingKeys)
    let utxo = fmap (,lovelaceToValue 1_000_000) signingKeys
    void $
      action $
        Seed
          { seedKeys = [(aliceSk, head aliceCardanoSks)]
          , contestationPeriod = UnsafeContestationPeriod 10
          , additionalUTxO = utxo
          , concurrentSettlements = False
          , faultyParty = Nothing
          }
    headId <- action $ Init alice
    void $ action $ Deposit{headIdVar = headId, utxoToDeposit = utxo}
    void $ action Close{party = alice}
    void $ action $ Wait 3600
    void $ action $ Fanout alice

propDL :: DL WorldState () -> Property
propDL d = forAllDL d propHydraModel

-- | Like 'propDL' for scripted scenarios where only the keys are random.
-- Shrinking is off: it cannot simplify a fixed script, it only reruns it
-- hundreds of times with varied values (a failing run takes ~0.3s, a shrunk
-- one took minutes).
-- | Run a fixed script a few times. Only its keys are random, so the size just
-- bounds the script's length. The generator gives up ("Looping") past
-- 2 * size + 20 steps, and the first run has size 0.
propScripted :: DL WorldState () -> Property
propScripted d = withMaxSuccess 5 $ noShrinking $ mapSize (max 60) $ forAllDL d propHydraModel

-- * Settlement races under divergent forks

-- | Open a head of @n@ parties, each owning one UTxO that can be deposited,
-- and return the head id variable together with each party's deposit fuel.
--
-- Funds only enter the head through deposits (it opens empty), so every
-- scenario starts from here. The 'Wait' leaves room for deep forks that must
-- stay clear of the head-opening transactions.
openHeadWithDepositFuel :: Int -> DL WorldState (Var HeadId, [(Party, UTxOType Payment)])
openHeadWithDepositFuel = openHeadWith (const Nothing)

-- | 'openHeadWithDepositFuel' with the last party misbehaving on the network
-- in the given way, see 'FaultMode'. Not the first, which leads the head's
-- opening.
openHeadWithFaultyParty :: FaultMode -> Int -> DL WorldState (Var HeadId, [(Party, UTxOType Payment)])
openHeadWithFaultyParty mode = openHeadWith $ \fuel -> case reverse fuel of
  (party, _) : _ -> Just (party, mode)
  [] -> Nothing

openHeadWith ::
  ([(Party, UTxOType Payment)] -> Maybe (Party, FaultMode)) ->
  Int ->
  DL WorldState (Var HeadId, [(Party, UTxOType Payment)])
openHeadWith pickFaulty n = do
  seedKeys <- forAllNonVariableQ $ withGenQ (genPartyKeysExactly n) (const True) (const [])
  let fuel = [(deriveParty hk, [(ck, lovelaceToValue 10_000_000)]) | (hk, ck) <- seedKeys]
  action_ $
    Seed
      { seedKeys
      , contestationPeriod = UnsafeContestationPeriod 10
      , additionalUTxO = concatMap snd fuel
      , concurrentSettlements = True
      , faultyParty = pickFaulty fuel
      }
  leader <- case fuel of
    (party, _) : _ -> pure party
    [] -> error "openHeadWith: no parties"
  headId <- action $ Init leader
  action_ $ Model.Wait 200
  pure (headId, fuel)

-- | The fuel of a two-party head, see 'openHeadWithDepositFuel'.
twoParties :: [(Party, UTxOType Payment)] -> ((Party, UTxOType Payment), (Party, UTxOType Payment))
twoParties = \case
  [a, b] -> (a, b)
  other -> error $ "expected two parties, got " <> show (length other)

-- | The fuel of a three-party head, see 'openHeadWithDepositFuel'.
threeParties :: [(Party, UTxOType Payment)] -> ((Party, UTxOType Payment), (Party, UTxOType Payment), (Party, UTxOType Payment))
threeParties = \case
  [a, b, c] -> (a, b, c)
  other -> error $ "expected three parties, got " <> show (length other)

-- | Payment decommitting a party's deposited fuel back to itself.
decommitFuel :: UTxOType Payment -> Payment
decommitFuel fuel = case fuel of
  (ck, value) : _ -> Payment{from = ck, to = ck, value}
  [] -> error "decommitFuel: no fuel"

-- | The head must still settle after the fork: confirm an L2 transaction,
-- close and fan out the whole confirmed UTxO (checked by the 'Fanout'
-- postcondition). A head wedged on an erased settlement fails here, either
-- because the close cannot land (its snapshot is ahead of the on-chain
-- version) or because the fanout does not match.
headStillSettles :: DL WorldState ()
headStillSettles = do
  st <- getModelStateDL
  case st of
    WorldState{hydraState = Open{}} -> do
      (party, payment) <- forAllNonVariableQ (nonConflictingTx st)
      tx <- action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx tx)
      action_ $ Model.Close party
      void $ action $ Model.Fanout party
    _ -> pure ()
  action_ Model.StopTheWorld

-- | Scenario 1: two deposits settle back to back, then a fork erases both
-- increments for good (no mempool re-inclusion). Both must be re-posted.
twoFinalizedIncrementsErased :: DL WorldState ()
twoFinalizedIncrementsErased = do
  (headId, fuel) <- openHeadWithDepositFuel 2
  let ((_, fuelA), (_, fuelB)) = twoParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  b <- action $ Model.SubmitDeposit headId fuelB
  action_ $ Model.ObserveCommitFinalized a
  action_ $ Model.ObserveCommitFinalized b
  -- Deep enough to erase both increments (a couple of blocks apart), shallow
  -- enough to keep both deposit transactions: they land back to back, at
  -- least 5 blocks (the activation period) before A's increment.
  action_ Model.RollbackAndFork{numberOfBlocks = 4, requeueErased = RequeueNone}
  headStillSettles

-- | Scenario 2: deposit B's transaction is on chain before A's increment
-- lands. Right after A settles, B's snapshot is approved and its increment is
-- in flight; a shallow fork then erases A's increment but keeps B's deposit.
-- Timing dependent by nature; the random walk covers the rest of this space.
finalizedIncrementErasedWithNextInFlight :: DL WorldState ()
finalizedIncrementErasedWithNextInFlight = do
  (headId, fuel) <- openHeadWithDepositFuel 2
  let ((_, fuelA), (_, fuelB)) = twoParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  b <- action $ Model.SubmitDeposit headId fuelB
  action_ $ Model.ObserveCommitFinalized a
  -- Fork as soon as B's snapshot is confirmed, while its increment is in
  -- flight. Deposit transactions re-land (B is still pending), the erased
  -- increment does not. B's snapshot confirms about one block after A's
  -- increment, so 2 reaches A's increment; the deposits are 8 blocks older.
  action_ $ Model.ObserveCommitApproved b
  action_ Model.RollbackAndFork{numberOfBlocks = 2, requeueErased = RequeueDeposits}
  action_ $ Model.ObserveCommitFinalized b
  headStillSettles

-- | Scenario 3: two decommits settle back to back (a second decommit is only
-- accepted once the first is finalized), then a fork erases both decrements.
twoFinalizedDecrementsErased :: DL WorldState ()
twoFinalizedDecrementsErased = do
  (headId, fuel) <- openHeadWithDepositFuel 3
  let ((partyA, fuelA), (partyB, fuelB), (_, fuelC)) = threeParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  b <- action $ Model.SubmitDeposit headId fuelB
  c <- action $ Model.SubmitDeposit headId fuelC
  action_ $ Model.ObserveCommitFinalized a
  action_ $ Model.ObserveCommitFinalized b
  action_ $ Model.ObserveCommitFinalized c
  dA <- action $ Model.SubmitDecommit partyA (decommitFuel fuelA)
  action_ $ Model.ObserveDecommitFinalized dA
  dB <- action $ Model.SubmitDecommit partyB (decommitFuel fuelB)
  action_ $ Model.ObserveDecommitFinalized dB
  action_ Model.RollbackAndFork{numberOfBlocks = 3, requeueErased = RequeueNone}
  headStillSettles

-- | A party misbehaving on the network must not stop the head. It stays
-- honest in the role where a fault wedges the head by design, so what is left
-- is noise the head is meant to survive: every message twice, an
-- acknowledgement for the round just finished, and proposals for the numbers
-- either side of each round it leads. See
-- 'Hydra.Model.MockChain.faultyBroadcast'.
--
-- The duplicate has to be ignored rather than counted twice, and the
-- neighbouring proposals refused without recording a snapshot in flight. A
-- receiver that recorded one would then refuse the real leader's request at
-- that number as one it had already seen, and the head would stop confirming.
--
-- What this pins and what it does not, both measured rather than assumed.
-- Dropping the check on a proposal's number in 'requireReqSn' fails here.
-- Dropping the check on who leads that number does not, because every party
-- then takes the injected proposal, they all agree on it, and the head keeps
-- confirming with the real leader's proposal refused instead. That check
-- decides who sets a snapshot's content, which no property about the head
-- staying alive can see.
headSurvivesFaultyParty :: DL WorldState ()
headSurvivesFaultyParty = do
  (headId, fuel) <- openHeadWithFaultyParty AddsMessages 3
  let ((partyA, fuelA), (_, fuelB), (_, fuelC)) = threeParties fuel
  -- Rounds of ordinary traffic between the settlements. Nothing is injected
  -- except during a round, and the faulty party only proposes for the rounds
  -- it leads, which with three parties is every third. A handful of
  -- settlements on their own leave it leading once or twice, so most of what
  -- it can send is never sent. These carry the head far enough for each
  -- injection to go out several times, against a different state each time:
  -- with a deposit in flight, with one settled, with a decommit in flight and
  -- with the head quiet.
  --
  -- Worth the extra second it costs. Dropping the check on a proposal's
  -- number in 'requireReqSn' is caught here and is not caught without these,
  -- since without them the injected proposals for the numbers either side of
  -- a round never meet a party far enough along to act on them.
  someTraffic
  a <- action $ Model.SubmitDeposit headId fuelA
  someTraffic
  action_ $ Model.ObserveCommitFinalized a
  someTraffic
  b <- action $ Model.SubmitDeposit headId fuelB
  action_ $ Model.ObserveCommitFinalized b
  someTraffic
  -- Whatever the head holds by now, rather than the fuel deposited earlier:
  -- the traffic above has moved that on, and a decommit of something the head
  -- no longer holds is refused before it is ever proposed.
  whatTheHeadHolds >>= \case
    Nothing -> pure ()
    Just held -> do
      dA <- action $ Model.SubmitDecommit partyA held
      someTraffic
      action_ $ Model.ObserveDecommitFinalized dA
  someTraffic
  c <- action $ Model.SubmitDeposit headId fuelC
  action_ $ Model.ObserveCommitFinalized c
  someTraffic
  headStillSettles

-- | A few snapshot rounds of ordinary layer-two traffic, to carry a scripted
-- scenario past the handful of rounds its settlements alone would produce.
someTraffic :: DL WorldState ()
someTraffic = replicateM_ 3 $ do
  st <- getModelStateDL
  case st of
    WorldState{hydraState = Open{offChainState = OffChainState{confirmedUTxO}}}
      | not (null confirmedUTxO) -> do
          (party, payment) <- forAllNonVariableQ (nonConflictingTx st)
          tx <- action $ Model.NewTx party payment
          eventually (ObserveConfirmedTx tx)
    _ -> pure ()

-- | Something the head confirmed it holds, as a payment sending it back to
-- whoever holds it, which is the shape a decommit takes.
whatTheHeadHolds :: DL WorldState (Maybe Payment)
whatTheHeadHolds =
  getModelStateDL <&> \case
    WorldState{hydraState = Open{offChainState = OffChainState{confirmedUTxO = (ck, value) : _}}} ->
      Just Payment{from = ck, to = ck, value}
    _ -> Nothing

-- | Scenario 4: an increment and then a decrement settle in consecutive
-- versions; a fork erases both. The decrement can only re-land after the
-- increment did.
finalizedIncrementAndDecrementErased :: DL WorldState ()
finalizedIncrementAndDecrementErased = do
  (headId, fuel) <- openHeadWithDepositFuel 2
  let ((partyA, fuelA), (_, fuelB)) = twoParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  action_ $ Model.ObserveCommitFinalized a
  b <- action $ Model.SubmitDeposit headId fuelB
  action_ $ Model.ObserveCommitFinalized b
  dA <- action $ Model.SubmitDecommit partyA (decommitFuel fuelA)
  action_ $ Model.ObserveDecommitFinalized dA
  action_ Model.RollbackAndFork{numberOfBlocks = 3, requeueErased = RequeueNone}
  headStillSettles

-- | While erased settlements are being re-posted, L2 keeps going: a new
-- decommit and a new deposit are requested right after the fork. Their
-- snapshots are signed at the local version, ahead of the chain, so their
-- settlements must queue behind the replayed ones and land in version order.
newSettlementsDuringReplay :: DL WorldState ()
newSettlementsDuringReplay = do
  (headId, fuel) <- openHeadWithDepositFuel 3
  let ((partyA, fuelA), (_, fuelB), (_, fuelC)) = threeParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  b <- action $ Model.SubmitDeposit headId fuelB
  action_ $ Model.ObserveCommitFinalized a
  action_ $ Model.ObserveCommitFinalized b
  action_ Model.RollbackAndFork{numberOfBlocks = 4, requeueErased = RequeueNone}
  -- New work while the two increments are being re-posted.
  dA <- action $ Model.SubmitDecommit partyA (decommitFuel fuelA)
  c <- action $ Model.SubmitDeposit headId fuelC
  -- The erased increments re-land first, then the new settlements.
  action_ $ Model.ObserveCommitFinalized a
  action_ $ Model.ObserveCommitFinalized b
  action_ $ Model.ObserveDecommitFinalized dA
  action_ $ Model.ObserveCommitFinalized c
  headStillSettles

-- | A party is crashed and restarted twice in a row right after a settlement
-- landed. A settlement lands as soon as one party holds all signatures of its
-- snapshot, so for a slower party some signatures can still be on their way
-- when the next action runs. A restarted party picks those up from the network
-- log, and the second restart must not lose them, or that party never confirms
-- the snapshot and the head never agrees on another one. Which party lags
-- depends on network timing, so every party is restarted, after
-- decommit rounds started off the block boundaries.
partyRestartedTwiceAfterSettlement :: DL WorldState ()
partyRestartedTwiceAfterSettlement = do
  (headId, fuel) <- openHeadWithDepositFuel 4
  -- The walk's own actions return as soon as the settlement landed, unlike
  -- 'SubmitDecommit', which first waits for every party to confirm.
  forM_ fuel $ \(_, fuelP) ->
    action_ $ Model.Deposit{headIdVar = headId, utxoToDeposit = fuelP}
  -- Three parties decommit their fuel; the last party's stays in the head for
  -- 'headStillSettles'.
  forM_ (zip [7, 11, 13] (take 3 fuel)) $ \(delay, (party, fuelP)) -> do
    -- Off the block boundary, so the round's signatures spread around the
    -- block in which the decrement lands.
    action_ $ Model.Wait delay
    action_ $ Model.Decommit{party, decommitTx = decommitFuel fuelP}
    forM_ fuel $ \(p, _) -> do
      action_ $ Model.RestartNode p
      action_ $ Model.RestartNode p
  headStillSettles

-- | Scenario 5: a deep fork erases the deposit transaction itself along with
-- its increment; the mempool re-includes the deposit, so the increment must
-- be re-posted once the deposit is observed again.
depositAndIncrementErasedThenRelanded :: DL WorldState ()
depositAndIncrementErasedThenRelanded = do
  (headId, fuel) <- openHeadWithDepositFuel 2
  let ((_, fuelA), _) = twoParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  action_ $ Model.ObserveCommitFinalized a
  -- The deposit lands ~8 blocks before its increment (up to half a deposit
  -- period of grace, 5 blocks of activation, snapshotting); 11 reaches past
  -- it, and the fork never goes past the head-opening transactions anyway.
  action_ Model.RollbackAndFork{numberOfBlocks = 11, requeueErased = RequeueAll}
  action_ $ Model.ObserveCommitFinalized a
  headStillSettles

-- * Partial fanout

-- | Open a single-party head holding @n@ outputs of 1 ADA, each owned by a
-- different key, and close it. More than 'fanoutOutputThreshold' outputs do
-- not fit one fanout transaction, so the fanout takes several steps; how many
-- outputs each step carries is decided by the node from the script budget.
closedHeadWithManyOutputs :: Int -> DL WorldState (UTxOType Payment)
closedHeadWithManyOutputs n = do
  ownerKeys <- forAllNonVariableQ $ withGenQ (vectorOf n arbitrary `suchThat` ((== n) . length . nub)) (const True) (const [])
  aliceCardanoSk <- case ownerKeys of
    k : _ -> pure k
    [] -> error "closedHeadWithManyOutputs: n must be > 0"
  let utxo = (,lovelaceToValue 1_000_000) <$> ownerKeys
  action_ $
    Seed
      { seedKeys = [(aliceSk, aliceCardanoSk)]
      , contestationPeriod = UnsafeContestationPeriod 10
      , additionalUTxO = utxo
      , concurrentSettlements = False
      , faultyParty = Nothing
      }
  headId <- action $ Init alice
  action_ $ Deposit{headIdVar = headId, utxoToDeposit = utxo}
  action_ Close{party = alice}
  pure utxo

-- NOTE: The fork scenarios use @numberOfBlocks = 1@: it erases exactly the tip
-- block, which holds the step observed just before.

-- | The automatic fanout is in progress and a fork erases its latest step.
-- The node must re-post it (the step is expected to be reported a second
-- time) and the head must still be fully fanned out.
autoFanoutStepErased :: DL WorldState ()
autoFanoutStepErased = do
  -- The node sizes each step by the script budget, about 23 outputs here, so
  -- this takes one partial step before the final one.
  void $ closedHeadWithManyOutputs (3 * fanoutOutputThreshold)
  action_ $ Model.StartFanout alice
  action_ $ Model.ObservePartialFanoutSteps 1
  action_ Model.RollbackAndFork{numberOfBlocks = 1, requeueErased = RequeueNone}
  action_ $ Model.ObservePartialFanoutSteps 2
  void $ action $ Model.ObserveFanoutFinalized alice
  action_ Model.StopTheWorld

-- | Like 'autoFanoutStepErased' but with two partial steps landed, of which
-- the fork erases the second: the node has to post that step again while its
-- bookkeeping is already at the final one.
autoFanoutTwoStepsErased :: DL WorldState ()
autoFanoutTwoStepsErased = do
  void $ closedHeadWithManyOutputs (6 * fanoutOutputThreshold)
  action_ $ Model.StartFanout alice
  action_ $ Model.ObservePartialFanoutSteps 2
  action_ Model.RollbackAndFork{numberOfBlocks = 1, requeueErased = RequeueNone}
  action_ $ Model.ObservePartialFanoutSteps 3
  void $ action $ Model.ObserveFanoutFinalized alice
  action_ Model.StopTheWorld

-- | Manual mode: the client hands the node two selections in turn. The
-- second one drains the head, so it ends in the final fanout.
manualPartialFanout :: DL WorldState ()
manualPartialFanout = do
  utxo <- closedHeadWithManyOutputs (2 * fanoutOutputThreshold + 5)
  let (firstSelection, rest) = splitAt fanoutOutputThreshold utxo
  action_ $ Model.PartialFanoutStep alice firstSelection
  action_ $ Model.PartialFanoutStep alice rest
  void $ action $ Model.ObserveFanoutFinalized alice
  action_ Model.StopTheWorld

-- | Manual mode with a fork erasing the first selection's step before the
-- client hands over the next selection. The node must post the erased step
-- again (the step is expected to be reported a second time) before the head
-- can be drained.
manualFanoutStepErased :: DL WorldState ()
manualFanoutStepErased = do
  utxo <- closedHeadWithManyOutputs (2 * fanoutOutputThreshold + 5)
  let (firstSelection, rest) = splitAt fanoutOutputThreshold utxo
  action_ $ Model.PartialFanoutStep alice firstSelection
  action_ Model.RollbackAndFork{numberOfBlocks = 1, requeueErased = RequeueNone}
  action_ $ Model.ObservePartialFanoutSteps 2
  action_ $ Model.PartialFanoutStep alice rest
  void $ action $ Model.ObserveFanoutFinalized alice
  action_ Model.StopTheWorld

-- | Scenario 6: a fork erases a finalized increment, the re-post lands, and
-- a second fork erases the re-posted increment as well.
rePostedIncrementErasedAgain :: DL WorldState ()
rePostedIncrementErasedAgain = do
  (headId, fuel) <- openHeadWithDepositFuel 2
  let ((_, fuelA), _) = twoParties fuel
  a <- action $ Model.SubmitDeposit headId fuelA
  action_ $ Model.ObserveCommitFinalized a
  -- The fork helper lets the chain run on for three blocks afterwards, so the
  -- re-posted increment is already a few blocks deep when the second fork
  -- hits: 3 reaches it while staying clear of the deposit (5+ blocks back).
  action_ Model.RollbackAndFork{numberOfBlocks = 3, requeueErased = RequeueNone}
  action_ $ Model.ObserveCommitFinalized a
  action_ Model.RollbackAndFork{numberOfBlocks = 3, requeueErased = RequeueNone}
  action_ $ Model.ObserveCommitFinalized a
  headStillSettles

propHydraModel :: Actions WorldState -> Property
propHydraModel actions =
  runIOSimProp $ do
    _ <- runActions actions
    assert True

-- XXX: This is very similar to propHydraModel, where the assertion is
-- basically a post condition!?
--
-- NOTE: this runs under io-sim, so a wedged head is caught by the simulated
-- deadlines inside the actions ('waitUntilMatch'), not by the wall clock here.
-- 'within' is only a backstop against a simulation that burns real CPU without
-- advancing virtual time, so it is deliberately far above the cost of the
-- heaviest generated case: measured at ~10s for size 100 on an 8-core box, so
-- a 30s budget failed on nothing worse than ordinary CPU contention (a full
-- `just test` run, or anything else busy on the machine).
--
-- That misfire is expensive out of proportion to itself: a timeout failure
-- shrinks, every shrink candidate overruns the same budget, and QuickCheck
-- prints nothing until shrinking ends - so a spurious failure presents as a
-- test frozen at a percentage for hours. Hence the shrink cap as well: a
-- genuine assertion failure converges well inside it, while a pathological
-- one still reports in bounded time.
propCheckModelBalances :: Property
propCheckModelBalances =
  within 120000000 $
    withMaxShrinks 25 $
      forAllShrink arbitrary shrink checkModelBalances

-- | Same balance consistency assertion as 'propCheckModelBalances', but over
-- longer random action sequences: heavier L2 traffic with deposits, decommits,
-- benign rollbacks and divergent-fork rollbacks interleaved. This is the
-- property meant to shake out races between settlement, re-posting and
-- rollbacks: a wedged head surfaces as a 'waitUntilMatch' timeout inside the
-- failing action, together with the shrunk action sequence and an io-sim
-- trace to diagnose from.
propStressModelBalances :: Property
propStressModelBalances =
  within 600000000 $
    withMaxSuccess 20 $
      mapSize (const 100) $
        forAllDL concurrentWalk checkModelBalances

-- | The walk behind the plain @check model@ property, which settles each
-- deposit and decommit before the next action.
defaultWalk :: DL WorldState ()
defaultWalk = do
  seed <- forAllNonVariableQ $ withGenQ (genSeedWith False) (const True) (const [])
  action_ seed
  anyActions_

-- | A random walk with 'concurrentSettlements': deposits and decommits may
-- overlap each other, L2 traffic and forks of every 'RequeueMode'.
concurrentWalk :: DL WorldState ()
concurrentWalk = do
  seed <- forAllNonVariableQ $ withGenQ (genSeedWith True) (const True) (const [])
  action_ seed
  anyActions_

-- | 'concurrentWalk' with one party misbehaving on the network, see
-- 'Hydra.Model.MockChain.faultyBroadcast'. The interleavings the walk produces
-- are what decides when its duplicates and out-of-turn requests land, and so
-- which guard has to refuse them.
faultyWalk :: DL WorldState ()
faultyWalk = do
  seed <- forAllNonVariableQ $ withGenQ (genFaultySeed True AddsMessages) (const True) (const [])
  action_ seed
  anyActions_

-- | 'concurrentWalk' with one party proposing something other than what an
-- honest leader would, in place of the honest proposal, see
-- 'Hydra.Model.MockChain.replacingBroadcast'. The head may stop, so this walk
-- is only ever run under 'checkSnapshotsAreSound'.
soundnessWalk :: DL WorldState ()
soundnessWalk = do
  seed <- forAllNonVariableQ $ withGenQ (genFaultySeed True ReplacesMessages) (const True) (const [])
  action_ seed
  anyActions_

-- | Whatever an honest leader would have proposed, the faulty party proposes
-- something else, see 'deviateFrom'.
--
-- 'checkSnapshotsAreSound' only means anything while this holds. A deviation
-- which quietly came out equal to its input would leave that walk running an
-- honest party under a name which says otherwise, and passing for it.
propReplacingDeviates :: Property
propReplacingDeviates =
  conjoin
    [ counterexample ("Proposal:     " <> show honest) $
      counterexample ("Sent instead: " <> show (deviateFrom honest)) $
        deviateFrom honest QC.=/= honest
    | honest <- proposals
    ]
 where
  -- Every shape a proposal comes in, all of them rather than a sample: which
  -- deviation runs is decided by the number, and what it has to work with by
  -- the rest, so the combinations are what matter and there are few enough to
  -- take the lot. Ten numbers reach each of the five deviations twice.
  --
  -- A proposal carrying nothing is the case that used to slip through, and a
  -- proposal already claiming the deposit a deviation would otherwise swap in
  -- is the other. No honest leader sends that second one, since nothing on
  -- chain answers to that id, but a deviation which quietly does nothing for
  -- some input is not one worth having.
  proposals =
    [ ReqSn{snapshotVersion, snapshotNumber, transactionIds, decommitTx, depositTxId}
    | snapshotVersion <- [0 .. 2]
    , snapshotNumber <- [0 .. 9]
    , transactionIds <- [[], [IsTx.txId aValidTx]]
    , decommitTx <- [Nothing, Just aValidTx]
    , depositTxId <- [Nothing, Just (IsTx.txId aValidTx), Just bogusDepositId]
    ]

  aValidTx :: Tx
  aValidTx = generateWith arbitrary 42

-- | Every snapshot the parties confirmed has to account for the value the head
-- moved, whether or not the run reached the end of its actions.
--
-- This is the one property here that tolerates a head which stops. A party
-- that withholds or corrupts a signing message stops the round it is in, and a
-- round cannot be abandoned and reissued, so that stall is permanent and is
-- the protocol working as written rather than a bug. A run against such a
-- party therefore has nothing to say about liveness, and the verdict of the
-- actions themselves is dropped. What it does have to say is whether the
-- parties, before they stopped, ever confirmed a snapshot that does not add
-- up, and that is judged here.
checkSnapshotsAreSound :: Actions WorldState -> Property
checkSnapshotsAreSound actions =
  property $ runRunMonadIOSimGen $ do
    runThem <- monadic' (void $ runActions actions)
    pure $ do
      outcome <- try @_ @SomeException runThem
      Nodes{nodes} <- get
      confirmed <- forM (Map.toList nodes) $ \(party, node) ->
        (party,) . mapMaybe confirmationOf <$> lift (serverOutputs node)
      let judged = map (uncurry partyConfirmedSoundSnapshots) confirmed
          counted = map (length . snd) confirmed
      pure $
        -- Not thresholds to meet, just a record of how often each happened
        -- when this went in, so a run which stops doing either is visible
        -- rather than silently passing. The deviations themselves are pinned
        -- by 'propReplacingDeviates', which does not depend on how the walk
        -- happens to fall.
        QC.cover 2 (isLeft outcome) "the run was stopped by the faulty party (12% then)" $
          QC.cover 5 (foldr max 0 counted > (3 :: Int)) "more than three snapshots confirmed (22% then)" $
            conjoin judged
 where
  confirmationOf :: ServerOutput Tx -> Maybe (Snapshot Tx)
  confirmationOf = \case
    SnapshotConfirmed{snapshot} -> Just snapshot
    _ -> Nothing

-- | Check one party's confirmed snapshots against each other, see
-- 'checkSnapshotsAreSound'.
partyConfirmedSoundSnapshots :: Party -> [Snapshot Tx] -> Property
partyConfirmedSoundSnapshots party confirmed =
  counterexample ("Party " <> show party <> " confirmed " <> show (length confirmed) <> " snapshot(s)") $
    conjoin (zipWith step confirmed (drop 1 confirmed))
 where
  -- A node restarted mid-run starts a fresh output history, so two neighbours
  -- here are not always two neighbours in the protocol. Only consecutive
  -- numbers say anything, the rest are skipped.
  step :: Snapshot Tx -> Snapshot Tx -> Property
  step earlier later
    | numberAfter /= numberBefore + 1 = property True
    | versionAfter == versionBefore =
        accountsFor "no settlement landed" (accountedValue earlier)
    | versionAfter == versionBefore + 1 =
        case (utxoToCommit earlier, utxoToDecommit earlier) of
          (Just committed, Nothing) ->
            accountsFor "an increment landed" (accountedValue earlier <> UTxO.totalValue committed)
          (Nothing, Just decommitted) ->
            accountsFor "a decrement landed" (accountedValue earlier <> negateValue (UTxO.totalValue decommitted))
          _ ->
            failing "a settlement landed for an action the snapshot preceding it did not carry"
    | otherwise = failing "the version moved by more than one settlement"
   where
    Snapshot{number = numberBefore, version = versionBefore} = earlier
    Snapshot{number = numberAfter, version = versionAfter} = later

    accountsFor what expected =
      counterexample ("Between them: " <> what) $
        counterexample (describePair earlier later) $
          accountedValue later === expected

    failing why = counterexample (describePair earlier later) $ counterexample why False

  describePair :: Snapshot Tx -> Snapshot Tx -> String
  describePair earlier later =
    "Snapshot "
      <> show (number earlier)
      <> " at version "
      <> show (version earlier)
      <> " accounting for "
      <> toString (renderValue (accountedValue earlier))
      <> ", carrying commit "
      <> show (UTxO.totalValue <$> utxoToCommit earlier)
      <> " and decommit "
      <> show (UTxO.totalValue <$> utxoToDecommit earlier)
      <> "\nSnapshot "
      <> show (number later)
      <> " at version "
      <> show (version later)
      <> " accounting for "
      <> toString (renderValue (accountedValue later))

-- | The value a snapshot accounts for: what the head holds under it, plus what
-- it has agreed to let go of and not yet seen leave.
--
-- What it has agreed to take in is deliberately not counted. A deposit sits in
-- its own output until the increment lands, so it is not the head's to account
-- for yet, and a claim on one can expire and be taken back by whoever made it,
-- which counting it would report as value gone missing. Nothing is lost by
-- leaving it out: the amount an increment brings in is still checked, on the
-- version bump which absorbs it. A decommit cannot be taken back the same way,
-- since a proposal dropping one is refused, so counting that is safe and
-- necessary, otherwise declaring a decommit would look like value vanishing.
accountedValue :: Snapshot Tx -> Value
accountedValue Snapshot{utxo, utxoToDecommit} =
  UTxO.totalValue utxo <> foldMap UTxO.totalValue utxoToDecommit

checkModelBalances :: Actions WorldState -> Property
checkModelBalances actions =
  runIOSimProp $ do
    (metadata, _symEnv) <- runActions actions
    let WorldState{hydraParties, hydraState, pendingCommits} = underlyingState metadata
    -- XXX: This wait time is arbitrary and corresponds to 3 "blocks" from
    -- the underlying simulated chain which produces a block every 20s. It
    -- should be enough to ensure all nodes' threads terminate their actions
    -- and those gets picked up by the chain
    run $ lift waitForAMinute
    let parties = Set.fromList $ deriveParty . fst <$> hydraParties
    nodes <- run $ gets nodes
    assert (parties == Map.keysSet nodes)
    forM_ parties $ \p -> do
      run $ lift $ threadDelay 1
      assertBalancesInOpenHeadAreConsistent hydraState (concatMap snd pendingCommits) nodes p
 where
  waitForAMinute :: MonadDelay m => m ()
  waitForAMinute = threadDelay 60

-- | The node's head UTxO must contain everything the model has as confirmed,
-- and nothing else except commits still pending in the model: those were
-- submitted but not observed as finalized ('SubmitDeposit'), so the node may
-- or may not have absorbed them yet.
assertBalancesInOpenHeadAreConsistent ::
  GlobalState ->
  -- | Pending (unobserved) commits, see 'pendingCommits'.
  UTxOType Payment ->
  Map Party (TestHydraClient Tx (IOSim s)) ->
  Party ->
  PropertyM (RunMonad (IOSim s)) ()
assertBalancesInOpenHeadAreConsistent world pendingCommitted nodes p = do
  assert (p `member` nodes)
  let node = nodes ! p
  case world of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      utxo <- run $ lift $ headUTxO node
      let sorted :: [TxOut x] -> [TxOut x]
          sorted = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o)))
      let expected = sorted (toTxOuts confirmedUTxO)
      let pendingOuts = sorted (toTxOuts pendingCommitted)
      let actual = sorted (UTxO.txOutputs utxo)
      let missing = expected \\ actual
          unexpected = (actual \\ expected) \\ pendingOuts
      stop $
        (null missing && null unexpected)
          & counterexample ("actual: \n  " <> intercalate "\n  " (map renderTxOut actual))
          & counterexample ("expected: \n  " <> intercalate "\n  " (map renderTxOut expected))
          & counterexample ("pending commits: \n  " <> intercalate "\n  " (map renderTxOut pendingOuts))
          & counterexample ("missing: \n  " <> intercalate "\n  " (map renderTxOut missing))
          & counterexample ("unexpected: \n  " <> intercalate "\n  " (map renderTxOut unexpected))
          & counterexample ("Incorrect balance for party " <> show p)
    _ -> do
      pure ()
 where
  renderTxOut :: TxOut x -> String
  renderTxOut o =
    toString $
      serialiseAddress (txOutAddress o) <> ": " <> renderValue (txOutValue o)

propIsDistributive :: (Show b, Eq b, Semigroup a, Semigroup b) => (a -> b) -> a -> a -> Property
propIsDistributive f x y =
  f x <> f y === f (x <> y)
    & counterexample ("f (x <> y)   " <> show (f (x <> y)))
    & counterexample ("f x <> f y: " <> show (f x <> f y))

-- | Expect to see contestations when trying to close with
-- an old snapshot
--
-- XXX: Since heads open empty (funds only enter via version-bumping
-- increments), a head with funds always has 'onChainVersion' > 0 and closing
-- with the initial snapshot (open version 0) is invalid on-chain — so this
-- scenario is effectively vacuous under random actions. To stay meaningful it
-- needs the mock to close with an old /confirmed/ snapshot at the current
-- version instead of 'CloseWithInitialSnapshot'.
partyContestsToWrongClosedSnapshot :: DL WorldState ()
partyContestsToWrongClosedSnapshot = do
  anyActions_
  settlePending
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{offChainState = OffChainState{confirmedUTxO}, onChainVersion = 0}} | not (null confirmedUTxO) -> do
      (party, payment) <- forAllNonVariableQ (nonConflictingTx st)
      tx <- action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx tx)
      action_ $ Model.CloseWithInitialSnapshot party
      void $ action $ Model.Fanout party
    _ -> pure ()
  action_ Model.StopTheWorld

-- | Given any random walk of the model, if the Head is open a NewTx getting
-- confirmed must be part of the UTxO after finalization.
fanoutContainsWholeConfirmedUTxO :: DL WorldState ()
fanoutContainsWholeConfirmedUTxO = do
  anyActions_
  settlePending
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{offChainState = OffChainState{confirmedUTxO}}} | not (null confirmedUTxO) -> do
      (party, payment) <- forAllNonVariableQ (nonConflictingTx st)
      tx <- action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx tx)
      action_ $ Model.Close party
      -- NOTE: The check is actually in the Model postcondition for 'Fanout'
      void $ action $ Model.Fanout party
    _ -> pure ()
  action_ Model.StopTheWorld

-- | Observe every settlement the random walk left pending, so that the steps
-- after it ('NewTx', 'Close', ...) are not blocked by their preconditions.
settlePending :: DL WorldState ()
settlePending = do
  WorldState{hydraState, pendingCommits, pendingDecommits} <- getModelStateDL
  case hydraState of
    Open{} -> do
      forM_ (fst <$> pendingCommits) $ action_ . Model.ObserveCommitFinalized
      forM_ (fst <$> pendingDecommits) $ action_ . Model.ObserveDecommitFinalized
    _ -> pure ()

nonConflictingTx :: WorldState -> Quantification (Party, Payment.Payment)
nonConflictingTx st =
  withGenQ (genPayment st) (const True) (const [])
    `whereQ` \(party, tx) -> precondition st (Model.NewTx party tx)

-- • Conflict-Free Liveness (Head):
--
-- In presence of a network adversary, a conflict-free execution satisfies the following condition:
-- For any transaction tx input via (new,tx), tx ∈ T i∈[n] Ci eventually holds.
--
-- NOTE: The model network is adversarial in delivery timing: each node
-- receives messages with a random per-node delay (see 'maxNetworkLatency' in
-- 'Hydra.Model.MockChain'), so nodes fall behind each other and behind their
-- own chain observations. Delivery order per node is preserved, matching the
-- production etcd network's total order — per-node reordering is deliberately
-- not modelled as it cannot happen there.
conflictFreeLiveness :: DL WorldState ()
conflictFreeLiveness = do
  anyActions_
  settlePending
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{offChainState = OffChainState{confirmedUTxO}}} | not (null confirmedUTxO) -> do
      (party, payment) <- forAllNonVariableQ (nonConflictingTx st)
      tx <- action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx tx)
    _ -> pure ()
  action_ Model.StopTheWorld

-- There cannot be a UTxO with no ADAs
-- See https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-mary.rst
propDoesNotGenerate0AdaUTxO :: Actions WorldState -> Property
propDoesNotGenerate0AdaUTxO (Actions actions) =
  property $ not (any contains0AdaUTxO actions)
 where
  contains0AdaUTxO :: Step WorldState -> Bool
  contains0AdaUTxO = \case
    _anyVar := (ActionWithPolarity (Model.Deposit _ utxo) _) -> any contains0Ada utxo
    _anyVar := (ActionWithPolarity (Model.SubmitDeposit _ utxo) _) -> any contains0Ada utxo
    _anyVar := (ActionWithPolarity (Model.NewTx _anyParty Payment.Payment{value}) _) -> value == lovelaceToValue 0
    _anyOtherStep -> False

  contains0Ada :: (a, Value) -> Bool
  contains0Ada = (== lovelaceToValue 0) . snd

-- | Print what each random walk actually generates, and fail if the mix has
-- gone degenerate.
--
-- What a walk exercises is decided by its generator and the preconditions
-- together, and neither is readable from the other. A walk which quietly
-- stopped producing deposits, or spent its whole length waiting, would keep
-- passing and still look like the coverage its name claims. QuickCheck's own
-- tables cannot be used for this: hspec keeps them to itself unless the
-- property fails.
--
-- Only the actions are generated here, none are run, so this costs seconds
-- rather than the minutes the walks themselves take.
reportWalkActionMix :: Spec
reportWalkActionMix =
  it "generates a usable mix of actions in every random walk" $ do
    measured <- traverse measure walks
    putTextLn . toText $ intercalate "\n" (render <$> measured)
    traverse_ requireEnoughOfEach measured
 where
  walks =
    [ ("default walk", defaultWalk)
    , ("concurrent walk", concurrentWalk)
    , ("faulty-party walk", faultyWalk)
    , ("replacing-party walk", soundnessWalk)
    ]

  -- Enough runs that a percentage means something, and not the same number
  -- every time, so a mix which only holds up at one sample size does not go
  -- unnoticed.
  measure (walkName, walk) = do
    runs <- QC.generate (QC.choose (300, 500))
    ActionMix walkName runs <$> walkActionMix runs walk

  -- Floors well under what was measured when this went in, so ordinary drift
  -- stays quiet and a walk which stops doing one of these does not. The head
  -- cannot be driven anywhere without all four. By what an action does rather
  -- than what it is called, because a walk settling each deposit before the
  -- next uses 'Model.Deposit' where a concurrent one uses 'Model.SubmitDeposit'.
  requireEnoughOfEach ActionMix{walkName, generated} =
    for_ needed $ \(what, belongs) -> do
      let share = percentOf (length (filter belongs generated)) (length generated)
      when (share < 1) $
        expectationFailure (printf "%s is only %s of the %s" what (showPercent share) walkName)

  needed :: [(String, String -> Bool)]
  needed =
    [ ("a deposit", (`elem` ["Deposit", "SubmitDeposit"]))
    , ("a decommit", (`elem` ["Decommit", "SubmitDecommit"]))
    , ("a transaction", (== "NewTx"))
    , ("a divergent fork", (== "RollbackAndFork"))
    ]

  render ActionMix{walkName, runs, generated} =
    intercalate "\n" $
      printf "\n  %s, %d actions over %d runs:" walkName total runs
        : [ printf "    %-7s %s" (showPercent (percentOf count total)) name
          | (name, count) <- sortOn (Down . snd) (Map.toList (tally generated))
          ]
   where
    total = length generated

  tally :: Ord a => [a] -> Map a Int
  tally xs = Map.fromListWith (+) [(x, 1) | x <- xs]

  percentOf :: Int -> Int -> Double
  percentOf n total = 100 * fromIntegral n / fromIntegral total

  showPercent :: Double -> String
  showPercent = printf "%.1f%%"

-- | What one walk generated, see 'reportWalkActionMix'.
data ActionMix = ActionMix
  { walkName :: String
  , runs :: Int
  , generated :: [String]
  -- ^ One entry per action generated, named after its constructor.
  }

-- | The names of the actions a walk generates, over the given number of runs.
-- Nothing is executed, see 'reportWalkActionMix'.
walkActionMix :: Int -> DL WorldState () -> IO [String]
walkActionMix runs walk = do
  collected <- newIORef []
  result <-
    QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = runs, QC.chatty = False} $
      forAllDL walk $ \(Actions steps) ->
        QC.ioProperty $ do
          modifyIORef' collected (map nameOf steps <>)
          pure True
  -- The property above cannot fail, so anything other than success means the
  -- walk would not produce actions at all, which every other test using it
  -- would then fail on for reasons much harder to read than this.
  unless (QC.isSuccess result) $
    fail ("a walk generated no actions: " <> QC.output result)
  readIORef collected
 where
  -- An action's constructor, taken off the front of its 'Show'. 'Action
  -- WorldState' is a GADT, so there is no generic name to ask for, and a
  -- case with an arm per constructor would be a second list to keep in step
  -- with the first.
  nameOf :: Step WorldState -> String
  nameOf (_var := ActionWithPolarity a _) =
    takeWhile (\c -> c /= ' ' && c /= '{') (show a)

-- * Utilities

-- | Specialised runner similar to <monadicST https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html#v:monadicST>.
runIOSimProp :: Testable a => (forall s. PropertyM (RunMonad (IOSim s)) a) -> Property
runIOSimProp p = property (runRunMonadIOSimGen (monadic' p))

-- | Similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck-Monadic.html#v:runSTGen>
--
-- It returns `Property` rather than `Gen a`, what allows to enhance the logging
-- in case of failures.
runRunMonadIOSimGen ::
  forall a.
  Testable a =>
  (forall s. Gen (RunMonad (IOSim s) a)) ->
  Gen Property
runRunMonadIOSimGen f = do
  Capture eval <- capture
  let tr = runSimTrace (sim eval)
  return $
    logsOnError tr $
      case traceResult False tr of
        Right a -> property a
        Left (FailureException (SomeException ex)) ->
          case cast ex of
            Just (HUnitFailure loc reason) ->
              False
                & counterexample (formatFailureReason reason)
                & counterexample ("Location: " <> maybe "unknown" prettySrcLoc loc)
            Nothing -> counterexample (show ex) False
        Left ex ->
          counterexample (show ex) False
 where
  logsOnError :: Testable prop => SimTrace a -> prop -> Property
  logsOnError tr =
    -- NOTE: Store trace dump in file when showing the counterexample. Behavior of
    -- this during shrinking is not 100% confirmed, show the trace directly if you
    -- want to be sure:
    --
    -- counterexample $ toString traceDump
    counterexample . unsafePerformIO $ do
      fn <- writeSystemTempFile "io-sim-trace" $ toString traceDump
      pure $ "IOSim trace stored in: " <> toString fn
   where
    traceDump = printTrace (Proxy :: Proxy (HydraLog Tx)) tr

  sim ::
    forall s.
    (Gen (RunMonad (IOSim s) a) -> RunMonad (IOSim s) a) ->
    IOSim s a
  sim eval = do
    v <-
      newLabelledTVarIO
        "sim-nodes"
        Nodes
          { nodes = mempty
          , logger = traceInIOSim
          , threads = mempty
          , chain = dummySimulatedChainNetwork
          , eventStores = mempty
          , nodeThreads = mempty
          }
    runReaderT (runMonad (eval f)) (RunState v)

eventually :: Action WorldState () -> DL WorldState ()
eventually a = action_ (Wait 10) >> action_ a

action_ :: Typeable a => Action WorldState a -> DL WorldState ()
action_ = void . action
