{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
--   Time 380.1s - ThreadId [4]  node-94455e3e - EventDeschedule Interruptable
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
-- When a property fails it will dump the sequence of actions leading to the failure.
-- This sequence can be copy/pasted and reused directly as a test against either the `Model` or the implementation
-- as exemplified by the following sample:
--
-- @@
--  it "runs actions against actual nodes" $ do
--    let Actions act =
--          Actions
--            [ Var 1
--                := Seed
--                  { seedKeys =
--                      [ (HydraSigningKey (SignKeyEd25519DSIGN "00000000000000000000000000000000000000000000000000000000000000003b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29"),
--                                          "0100000008030606080507030707000607020508050000020207070508040800")
--                      , (HydraSigningKey (SignKeyEd25519DSIGN "2e00000000000000000000000000000000000000000000000000000000000000264a0707979e0d6691f74b055429b5f318d39c2883bb509310b67424252e9ef2"),
--                                          "0106010101070600040403010600080805020003040508030307080706060608")
--                      , (HydraSigningKey (SignKeyEd25519DSIGN "ed785af0fb0000000000000000000000000000000000000000000000000000001c02babf6d3d51b725db8b72043823d66634b39db74836b1494bdb647073d566"),
--                                          "0000070304040705060101030802010105080806050605070104030603010503")
--                      ]
--                  }
--            , Var 2 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")},
--                               command = Init{contestationPeriod = -6.413670805613}}
--            , Var 3 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "264a0707979e0d6691f74b055429b5f318d39c2883bb509310b67424252e9ef2")},
--                               command = Commit{Input.utxo = [("0106010101070600040403010600080805020003040508030307080706060608", valueFromList [(AdaAssetId, 18470954)])]}}
--            , Var 4 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "1c02babf6d3d51b725db8b72043823d66634b39db74836b1494bdb647073d566")},
--                               command = Commit{Input.utxo = [("0000070304040705060101030802010105080806050605070104030603010503", valueFromList [(AdaAssetId, 19691416)])]}}
--            , Var 5 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")},
--                               command = Commit{Input.utxo = [("0100000008030606080507030707000607020508050000020207070508040800", valueFromList [(AdaAssetId, 7003529)])]}}
--            , Var 6
--                := Command
--                  { Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")}
--                  , command =
--                      NewTx
--                        { Input.transaction =
--                            Payment
--                              { from = "0100000008030606080507030707000607020508050000020207070508040800"
--                              , to = "0106010101070600040403010600080805020003040508030307080706060608"
--                              , value = valueFromList [(AdaAssetId, 7003529)]
--                              }
--                        }
--                  }
--            ]
--        -- env and model state are unused in perform
--        env = []
--
--        dummyState :: WorldState (IOSim s)
--        dummyState = WorldState{hydraParties = mempty, hydraState = Start}
--
--        loop [] = pure ()
--        loop ((Var{} := a) : as) = do
--          void $ perform dummyState a (lookUpVar env)
--          loop as
--        tr =
--          runSimTrace $
--            evalStateT
--              (loop act)
--              (Nodes mempty traceInIOSim)
--        traceDump = printTrace (Proxy :: Proxy Tx) tr
--    print traceDump
--    True `shouldBe` True
-- @@
module Hydra.ModelSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude hiding (after)

import qualified Cardano.Api.UTxO as UTxO
import Control.Monad.Class.MonadTimer ()
import Control.Monad.IOSim (Failure (FailureException), IOSim, runSimTrace, traceResult)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.BehaviorSpec (TestHydraNode (..))
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Model (
  Action (ObserveConfirmedTx, Wait),
  GlobalState (..),
  Nodes (Nodes, nodes),
  OffChainState (..),
  RunMonad,
  WorldState (..),
  genPayment,
  runMonad,
 )
import qualified Hydra.Model as Model
import qualified Hydra.Model.Payment as Payment
import Hydra.Party (Party (..), deriveParty)
import Test.QuickCheck (Property, Testable, counterexample, forAll, property, withMaxSuccess, within)
import Test.QuickCheck.DynamicLogic (
  DL,
  action,
  anyActions_,
  forAllDL_,
  forAllQ,
  getModelStateDL,
  withGenQ,
 )
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic', monitor, run)
import Test.QuickCheck.StateModel (Actions, Step ((:=)), runActions, stateAfter, pattern Actions)
import Test.Util (printTrace, traceInIOSim)

spec :: Spec
spec = do
  -- There cannot be a UTxO with no ADAs
  -- See https://github.com/input-output-hk/cardano-ledger/blob/master/doc/explanations/min-utxo-mary.rst
  prop "model should not generate 0 Ada UTxO" $ withMaxSuccess 10000 prop_doesNotGenerate0AdaUTxO
  prop "model generates consistent traces" $ withMaxSuccess 10000 prop_generateTraces
  prop "implementation respects model" $ forAll arbitrary prop_checkModel
  prop "check conflict-free liveness" prop_checkConflictFreeLiveness

prop_checkConflictFreeLiveness :: Property
prop_checkConflictFreeLiveness =
  within 50000000 $
    forAllDL_ conflictFreeLiveness prop_HydraModel

prop_HydraModel :: Actions WorldState -> Property
prop_HydraModel actions = property $
  runIOSimProp $ do
    _ <- runActions actions
    assert True

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
      (party, payment) <- forAllQ (nonConflictingTx st)
      action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx payment)
    _ -> pass
  action Model.StopTheWorld
 where
  nonConflictingTx st = withGenQ (genPayment st) (const [])
  eventually a = action (Wait 10) >> action a

prop_generateTraces :: Actions WorldState -> Property
prop_generateTraces actions =
  let st = stateAfter actions
   in case actions of
        Actions [] -> property True
        Actions _ ->
          hydraState st /= Start
            & counterexample ("state: " <> show st)

prop_doesNotGenerate0AdaUTxO :: Actions WorldState -> Bool
prop_doesNotGenerate0AdaUTxO (Actions actions) =
  not (any contains0AdaUTxO actions)
 where
  contains0AdaUTxO :: Step WorldState -> Bool
  contains0AdaUTxO = \case
    _anyVar := Model.Commit _anyParty utxos -> any contains0Ada utxos
    _anyVar := Model.NewTx _anyParty Payment.Payment{value} -> value == lovelaceToValue 0
    _anyOtherStep -> False
  contains0Ada = (== lovelaceToValue 0) . snd

prop_checkModel :: Actions WorldState -> Property
prop_checkModel actions =
  within 20000000 $
    property $
      runIOSimProp $ do
        (WorldState{hydraParties, hydraState}, _symEnv) <- runActions actions
        -- XXX: This wait time is arbitrary and corresponds to 3 "blocks" from
        -- the underlying simulated chain which produces a block every 20s. It
        -- should be enough to ensure all nodes' threads terminate their actions
        -- and those gets picked up by the chain
        run $ lift waitForAMinute
        let parties = Set.fromList $ deriveParty . fst <$> hydraParties
        nodes <- run $ gets nodes
        assert (parties == Map.keysSet nodes)
        forM_ parties $ \p -> do
          assertBalancesInOpenHeadAreConsistent hydraState nodes p
 where
  waitForAMinute :: MonadDelay m => m ()
  waitForAMinute = threadDelay 60

assertBalancesInOpenHeadAreConsistent ::
  GlobalState ->
  Map Party (TestHydraNode Tx (IOSim s)) ->
  Party ->
  PropertyM (RunMonad (IOSim s)) ()
assertBalancesInOpenHeadAreConsistent world nodes p = do
  let node = nodes ! p
  case world of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      utxo <- run $ getUTxO node
      let expectedBalance =
            Map.fromListWith
              (<>)
              [ (unwrapAddress addr, value)
              | (Payment.CardanoSigningKey sk, value) <- confirmedUTxO
              , let addr = mkVkAddress testNetworkId (getVerificationKey sk)
              , valueToLovelace value /= Just 0
              ]
      let actualBalance =
            Map.fromListWith (<>) $
              [ (unwrapAddress addr, value)
              | (TxOut addr value _ _) <- Map.elems (UTxO.toMap utxo)
              , valueToLovelace value /= Just 0
              ]
      monitor $
        counterexample $
          toString $
            unlines
              [ "actualBalance = " <> show actualBalance
              , "expectedBalance = " <> show expectedBalance
              , "Difference: (" <> show p <> ") " <> show (Map.difference actualBalance expectedBalance)
              ]
      assert (expectedBalance == actualBalance)
    _ -> do
      pure ()
 where
  getUTxO node = lift $ do
    node `send` GetUTxO
    let loop =
          waitForNext node >>= \case
            GetUTxOResponse _ u -> pure u
            _ -> loop
    loop

--

-- * Utilities for `IOSim`

--

-- | Specialised runner similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Monadic.html#runSTGen>.
runIOSimProp :: Testable a => (forall s. PropertyM (RunMonad (IOSim s)) a) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  let tr = runSimTrace $ evalStateT (runMonad $ eval $ monadic' p) (Nodes mempty traceInIOSim mempty)
      traceDump = printTrace (Proxy :: Proxy Tx) tr
      logsOnError = counterexample ("trace:\n" <> toString traceDump)
  case traceResult False tr of
    Right x ->
      pure $ logsOnError x
    Left (FailureException (SomeException ex)) -> do
      pure $ counterexample (show ex) $ logsOnError $ property False
    Left ex ->
      pure $ counterexample (show ex) $ logsOnError $ property False

unwrapAddress :: AddressInEra -> Text
unwrapAddress = \case
  ShelleyAddressInEra addr -> serialiseToBech32 addr
  ByronAddressInEra{} -> error "Byron."
