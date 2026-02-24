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
--   do action $ Seed {seedKeys = [("8bbc9f32e4faff669ed1561025f243649f1332902aa79ad7e6e6bbae663f332d",CardanoSigningKey {signingKey = "0400020803030302070808060405040001050408070401040604000005010603"})], seedContestationPeriod = 46s, seedDepositDeadline = 50s, toCommit = fromList [(Party {vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"},[(CardanoSigningKey {signingKey = "0400020803030302070808060405040001050408070401040604000005010603"},valueFromList [(AdaAssetId,54862683)])])]}
--      var2 <- action $ Init (Party {vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"})
--      action $ Commit {headIdVar = var2, party = Party {vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"}, utxoToCommit = [(CardanoSigningKey {signingKey = "0400020803030302070808060405040001050408070401040604000005010603"},valueFromList [(AdaAssetId,54862683)])]}
--      action $ Decommit {party = Party {vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"}, decommitTx = Payment { from = CardanoSigningKey {signingKey = "0400020803030302070808060405040001050408070401040604000005010603"}, to = CardanoSigningKey {signingKey = "0702050602000101050108060302010707060007020308080801060700030306"}, value = valueFromList [(AdaAssetId,54862683)] }}
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
--     action $ Seed{seedKeys = [("8bbc9f32e4faff669ed1561025f243649f1332902aa79ad7e6e6bbae663f332d", CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"})], seedContestationPeriod = UnsafeContestationPeriod 46, seedDepositDeadline = UnsafeDepositDeadline 50, toCommit = fromList [(Party{vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"}, [(CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"}, valueFromList [(AdaAssetId, 54862683)])])]}
--     var2 <- action $ Init (Party{vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"})
--     action $ Commit{headIdVar = var2, party = Party{vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"}, utxoToCommit = [(CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"}, valueFromList [(AdaAssetId, 54862683)])]}
--     action $ Decommit{party = Party{vkey = "b4ea494b4bda6281899727bf4cfef5cdeba8fb3fec4edebc408aa72dfd6ad4f0"}, decommitTx = Payment{from = CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"}, to = CardanoSigningKey{signingKey = "0702050602000101050108060302010707060007020308080801060700030306"}, value = valueFromList [(AdaAssetId, 54862683)]}}
--     action $ Deposit{headIdVar = var2, utxoToDeposit = [(CardanoSigningKey{signingKey = "0400020803030302070808060405040001050408070401040604000005010603"}, valueFromList [(AdaAssetId, 54862683)])], deadline = read "1864-06-06 08:24:08.669152896211 UTC"}
--     pure ()
-- @@
module Hydra.ModelSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude hiding (after)

import Cardano.Api.UTxO qualified as UTxO
import Control.Monad.Class.MonadTimer ()
import Control.Monad.IOSim (Failure (FailureException), IOSim, SimTrace, runSimTrace, traceResult)
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Typeable (cast)
import Hydra.BehaviorSpec (TestHydraClient (..), dummySimulatedChainNetwork)
import Hydra.Logging.Messages (HydraLog)
import Hydra.Model (
  Action (..),
  GlobalState (..),
  Nodes (Nodes, nodes),
  OffChainState (..),
  RunMonad,
  RunState (..),
  WorldState (..),
  genInit,
  genPayment,
  genSeed,
  headUTxO,
  runMonad,
  toRealUTxO,
  toTxOuts,
 )
import Hydra.Model qualified as Model
import Hydra.Model.Payment (Payment (..))
import Hydra.Model.Payment qualified as Payment
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Party (Party (..), deriveParty)
import System.IO.Temp (writeSystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (formatFailureReason)
import Test.Hydra.Node.Fixture (alice, aliceSk)
import Test.QuickCheck (Property, Testable, counterexample, expectFailure, forAllShrink, property, vectorOf, withMaxSuccess, within)
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
  precondition,
  runActions,
  pattern Actions,
 )
import Test.Util (printTrace, traceInIOSim)

instance HasVariables (SigningKey PaymentKey) where
  getAllVariables = mempty
instance HasVariables Payment.CardanoSigningKey where
  getAllVariables = mempty

spec :: Spec
spec = do
  context "modeling" $ do
    prop "not generate actions with 0 Ada" $ withMaxSuccess 10000 propDoesNotGenerate0AdaUTxO
    prop "toRealUTxO is distributive" $ propIsDistributive toRealUTxO
    prop "toTxOuts is distributive" $ propIsDistributive toTxOuts
  prop "check model" propHydraModel
  prop "check model balances" propCheckModelBalances
  -- This scenario seeds a head with a single party and an UTxO set of 42 elements,
  -- which is over the budget of the mocked chain implementation.
  -- See https://github.com/cardano-scaling/hydra/issues/2270
  prop "fails fanout over the limit" $ expectFailure (propFanoutLimit 86)
  prop "succeeds fanout under the limit" $ propFanoutLimit 85
  context "logic" $ do
    prop "check conflict-free liveness" $ propDL conflictFreeLiveness
    prop "check head opens if all participants commit" $ propDL headOpensIfAllPartiesCommit
    prop "fanout contains whole confirmed UTxO" $ propDL fanoutContainsWholeConfirmedUTxO
    prop "parties contest to wrong closed snapshot" $ propDL partyContestsToWrongClosedSnapshot

propFanoutLimit :: Int -> Property
propFanoutLimit limit =
  within 10000000 $ propDL $ do
    aliceCardanoSks <- forAllQ $ withGenQ ((:|) <$> arbitrary <*> vectorOf limit (arbitrary @Payment.CardanoSigningKey)) (const True) (const [])
    let utxo = toList $ fmap (,lovelaceToValue 1_000_000) aliceCardanoSks
    void $
      action $
        Seed
          { seedKeys = [(aliceSk, head aliceCardanoSks)]
          , contestationPeriod = UnsafeContestationPeriod 10
          , toCommit = Map.fromList [(alice, utxo)]
          , additionalUTxO = mempty
          }
    headId <- action $ Init alice
    void $ action $ Commit headId alice utxo
    void $ action Close{party = alice}
    void $ action $ Wait 3600
    void $ action $ Fanout alice

propDL :: DL WorldState () -> Property
propDL d = forAllDL d propHydraModel

propHydraModel :: Actions WorldState -> Property
propHydraModel actions =
  runIOSimProp $ do
    _ <- runActions actions
    assert True

-- XXX: This is very similar to propHydraModel, where the assertion is
-- basically a post condition!?
propCheckModelBalances :: Property
propCheckModelBalances =
  within 30000000 $
    forAllShrink arbitrary shrink $ \actions ->
      runIOSimProp $ do
        (metadata, _symEnv) <- runActions actions
        let WorldState{hydraParties, hydraState} = underlyingState metadata
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
          assertBalancesInOpenHeadAreConsistent hydraState nodes p
 where
  waitForAMinute :: MonadDelay m => m ()
  waitForAMinute = threadDelay 60

assertBalancesInOpenHeadAreConsistent ::
  GlobalState ->
  Map Party (TestHydraClient Tx (IOSim s)) ->
  Party ->
  PropertyM (RunMonad (IOSim s)) ()
assertBalancesInOpenHeadAreConsistent world nodes p = do
  assert (p `member` nodes)
  let node = nodes ! p
  case world of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      utxo <- run $ lift $ headUTxO node
      let sorted :: [TxOut x] -> [TxOut x]
          sorted = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o)))
      let expected = sorted (toTxOuts confirmedUTxO)
      let actual = sorted (UTxO.txOutputs utxo)
      stop $
        expected === actual
          & counterexample ("actual: \n  " <> intercalate "\n  " (map renderTxOut actual))
          & counterexample ("expected: \n  " <> intercalate "\n  " (map renderTxOut expected))
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
partyContestsToWrongClosedSnapshot :: DL WorldState ()
partyContestsToWrongClosedSnapshot = do
  headOpensIfAllPartiesCommit
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{}} -> do
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
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{}} -> do
      (party, payment) <- forAllNonVariableQ (nonConflictingTx st)
      tx <- action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx tx)
      action_ $ Model.Close party
      -- NOTE: The check is actually in the Model postcondition for 'Fanout'
      void $ action $ Model.Fanout party
    _ -> pure ()
  action_ Model.StopTheWorld

nonConflictingTx :: WorldState -> Quantification (Party, Payment.Payment)
nonConflictingTx st =
  withGenQ (genPayment st) (const True) (const [])
    `whereQ` \(party, tx) -> precondition st (Model.NewTx party tx)

headOpensIfAllPartiesCommit :: DL WorldState ()
headOpensIfAllPartiesCommit = do
  seedTheWorld
  initHead
  everybodyCommit
  eventually' ObserveHeadIsOpen
 where
  eventually' :: Action WorldState () -> DL WorldState ()
  eventually' a = action (Wait 1000) >> action_ a

  seedTheWorld = forAllNonVariableQ (withGenQ genSeed (const True) (const [])) >>= action_

  initHead = do
    WorldState{hydraParties} <- getModelStateDL
    forAllQ (withGenQ (genInit hydraParties) (const True) (const [])) >>= action_

  everybodyCommit = do
    WorldState{hydraParties, hydraState} <- getModelStateDL
    case hydraState of
      Initial{headIdVar, pendingCommits} ->
        forM_ hydraParties $ \p -> do
          let party = deriveParty (fst p)
          case Map.lookup party pendingCommits of
            Nothing -> pure ()
            Just utxo ->
              void $ action $ Model.Commit headIdVar party utxo
      _ -> pure ()

-- • Conflict-Free Liveness (Head):
--
-- In presence of a network adversary, a conflict-free execution satisfies the following condition:
-- For any transaction tx input via (new,tx), tx ∈ T i∈[n] Ci eventually holds.
--
-- TODO: make the network adversarial => make the model runner interleave/delay network messages
conflictFreeLiveness :: DL WorldState ()
conflictFreeLiveness = do
  anyActions_
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{}} -> do
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
    _anyVar := (ActionWithPolarity (Model.Commit _ _anyParty utxos) _) -> any contains0Ada utxos
    _anyVar := (ActionWithPolarity (Model.NewTx _anyParty Payment.Payment{value}) _) -> value == lovelaceToValue 0
    _anyOtherStep -> False

  contains0Ada :: (a, Value) -> Bool
  contains0Ada = (== lovelaceToValue 0) . snd

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
          }
    runReaderT (runMonad (eval f)) (RunState v)

eventually :: Action WorldState () -> DL WorldState ()
eventually a = action_ (Wait 10) >> action_ a

action_ :: Typeable a => Action WorldState a -> DL WorldState ()
action_ = void . action
