{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude hiding (after)

-- This is completely safe
import Unsafe.Coerce (unsafeCoerce)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Crypto.DSIGN (SignKeyDSIGN (SignKeyEd25519DSIGN), VerKeyDSIGN (VerKeyEd25519DSIGN))
import Control.Monad.IOSim (Failure (FailureException), IOSim, runSimTrace, traceResult)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.BehaviorSpec (TestHydraNode (..))
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.ClientInput (ClientInput (..))
import qualified Hydra.ClientInput as Input
import Hydra.Crypto (SigningKey (HydraSigningKey), VerificationKey (HydraVerificationKey))
import Hydra.Model (
  Action (..),
  LocalState (..),
  Nodes (Nodes, nodes),
  OffChainState (..),
  Payment (..),
  WorldState (..),
  genNewTx,
  unwrapAddress,
 )
import qualified Hydra.Model as Model
import Hydra.Party (Party (..), deriveParty)
import Hydra.ServerOutput (ServerOutput (..))
import qualified Hydra.ServerOutput as Output
import qualified Hydra.ServerOutput as ServerOutput
import Test.QuickCheck (Property, counterexample, property, withMaxSuccess, within)
import Test.QuickCheck.DynamicLogic (
  DynFormula,
  after,
  afterAny,
  forAllQ,
  withGenQ,
 )
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic', monitor, run)
import Test.QuickCheck.StateModel (Actions, Step ((:=)), Var (Var), lookUpVar, perform, runActions, stateAfter, pattern Actions)
import Test.Util (printTrace, traceInIOSim)
import qualified Prelude

spec :: Spec
spec = do
  it "runs actions against actual nodes" $ do
    let Actions act =
          Actions
            [ Var 1
                := Seed
                  { seedKeys =
                      [ (HydraSigningKey (SignKeyEd25519DSIGN "00000000000000000000000000000000000000000000000000000000000000003b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29"), "0100000008030606080507030707000607020508050000020207070508040800")
                      , (HydraSigningKey (SignKeyEd25519DSIGN "2e00000000000000000000000000000000000000000000000000000000000000264a0707979e0d6691f74b055429b5f318d39c2883bb509310b67424252e9ef2"), "0106010101070600040403010600080805020003040508030307080706060608")
                      , (HydraSigningKey (SignKeyEd25519DSIGN "ed785af0fb0000000000000000000000000000000000000000000000000000001c02babf6d3d51b725db8b72043823d66634b39db74836b1494bdb647073d566"), "0000070304040705060101030802010105080806050605070104030603010503")
                      ]
                  }
            , Var 2 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")}, command = Init{contestationPeriod = -6.413670805613}}
            , Var 3 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "264a0707979e0d6691f74b055429b5f318d39c2883bb509310b67424252e9ef2")}, command = Commit{Input.utxo = [("0106010101070600040403010600080805020003040508030307080706060608", valueFromList [(AdaAssetId, 18470954)])]}}
            , Var 4 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "1c02babf6d3d51b725db8b72043823d66634b39db74836b1494bdb647073d566")}, command = Commit{Input.utxo = [("0000070304040705060101030802010105080806050605070104030603010503", valueFromList [(AdaAssetId, 19691416)])]}}
            , Var 5 := Command{Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")}, command = Commit{Input.utxo = [("0100000008030606080507030707000607020508050000020207070508040800", valueFromList [(AdaAssetId, 7003529)])]}}
            , Var 6
                := Command
                  { Model.party = Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "3b6a27bcceb6a42d62a3a8d02a6f0d73653215771de243a63ac048a18b59da29")}
                  , command =
                      NewTx
                        { Input.transaction =
                            Payment
                              { from = "0100000008030606080507030707000607020508050000020207070508040800"
                              , to = "0106010101070600040403010600080805020003040508030307080706060608"
                              , value = valueFromList [(AdaAssetId, 7003529)]
                              }
                        }
                  }
            ]
        -- env and model state are unused in perform
        env = []

        dummyState :: WorldState (IOSim s)
        dummyState = WorldState{hydraParties = mempty, hydraState = Start}

        loop [] = pure ()
        loop ((Var{} := a) : as) = do
          void $ perform dummyState a (lookUpVar env)
          loop as
        tr =
          runSimTrace $
            evalStateT
              (loop act)
              (Nodes mempty traceInIOSim)
        traceDump = printTrace (Proxy :: Proxy Tx) tr
    print traceDump
    True `shouldBe` True
  prop "model generates consistent traces" $ withMaxSuccess 10000 prop_generateTraces
  prop "implementation respects model" prop_checkModel

prop_generateTraces :: AnyActions -> Property
prop_generateTraces (AnyActions actions) =
  let st = stateAfter actions
   in case actions of
        Actions [] -> property True
        Actions _ ->
          hydraState st /= Start
            & counterexample ("state: " <> show st)

prop_checkModel :: AnyActions -> Property
prop_checkModel (AnyActions actions) =
  within 1000000 $
    property $
      runIOSimProp $
        monadic' $ do
          (WorldState{hydraParties, hydraState}, _symEnv) <- runActions actions
          run $ lift waitUntilTheEndOfTime
          let parties = Set.fromList $ deriveParty . fst <$> hydraParties
          nodes <- run $ gets nodes
          assert (parties == Map.keysSet nodes)
          forM_ parties $ \p -> do
            assertNodeSeesAndReportsAllExpectedCommits hydraState nodes p
            assertOpenHeadWithAllExpectedCommits hydraState nodes p

-- | Conflict-Free Liveness (Head): A conflict-free execution satisfies the following condition:
--
-- For any transaction tx input via @(new, tx)@
-- \[
-- \mathttt{tx} \in \bigcap_{i \in [n]} \bar{C}_i
-- \]
-- eventually holds.
conflictFreeLiveness :: Typeable m => DynFormula (WorldState m)
conflictFreeLiveness =
  afterAny
    ( \st ->
        forAllQ (withGenQ (genNewTx st) (const [])) $ \newTx ->
          after newTx (error "undefined")
    )

assertNodeSeesAndReportsAllExpectedCommits ::
  LocalState ->
  Map Party (TestHydraNode Tx (IOSim s)) ->
  Party ->
  PropertyM (StateT (Nodes (IOSim s)) (IOSim s)) ()
assertNodeSeesAndReportsAllExpectedCommits world nodes p = do
  let node = nodes ! p
  case world of
    Initial{commits} -> do
      outputs <- run $ lift $ serverOutputs @Tx node
      let expectedCommitted =
            fmap (\(sk, value) -> TxOut (mkVkAddress testNetworkId (getVerificationKey sk)) value TxOutDatumNone)
              <$> commits
      let actualCommitted =
            Map.fromList
              [ (party, Map.elems (UTxO.toMap utxo))
              | Committed{Output.party = party, ServerOutput.utxo = utxo} <- outputs
              ]
      monitor $
        counterexample $
          toString $
            unlines
              [ "Actual committed: (" <> show p <> ") " <> show actualCommitted
              , "Expected committed: (" <> show p <> ") " <> show expectedCommitted
              ]
      assert (actualCommitted == expectedCommitted)
    _ -> do
      pure ()

assertOpenHeadWithAllExpectedCommits ::
  LocalState ->
  Map Party (TestHydraNode Tx (IOSim s)) ->
  Party ->
  PropertyM (StateT (Nodes (IOSim s)) (IOSim s)) ()
assertOpenHeadWithAllExpectedCommits world nodes p = do
  let node = nodes ! p
  case world of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      utxo <- run $ getUTxO node
      let expectedBalance =
            Map.fromListWith
              (<>)
              [ (unwrapAddress addr, value)
              | (sk, value) <- confirmedUTxO
              , let addr = mkVkAddress testNetworkId (getVerificationKey sk)
              , valueToLovelace value /= Just 0
              ]
      let actualBalance =
            Map.fromListWith (<>) $
              [ (unwrapAddress addr, value)
              | (TxOut addr value _) <- Map.elems (UTxO.toMap utxo)
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
            GetUTxOResponse u -> pure u
            _ -> loop
    loop

-- NOTE: This is only sound to run in IOSim, because delays are instant. It
-- allows to make sure we wait long-enough for remaining asynchronous actions /
-- events to complete before we make any test assertion.
waitUntilTheEndOfTime :: MonadDelay m => m ()
waitUntilTheEndOfTime = threadDelay 1000000000000

--

-- * Utilities for `IOSim`

--

-- | Specialised runner similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Monadic.html#runSTGen>.
runIOSimProp :: (forall s. Gen (StateT (Nodes (IOSim s)) (IOSim s) Property)) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  let tr = runSimTrace $ evalStateT (eval p) (Nodes mempty traceInIOSim)
      traceDump = printTrace (Proxy :: Proxy Tx) tr
      logsOnError = counterexample ("trace:\n" <> toString traceDump)
  case traceResult False tr of
    Right x ->
      pure $ logsOnError x
    Left (FailureException (SomeException ex)) -> do
      pure $ counterexample (show ex) $ logsOnError $ property False
    Left ex ->
      pure $ counterexample (show ex) $ logsOnError $ property False

newtype AnyActions = AnyActions {unAnyActions :: forall s. Actions (WorldState (IOSim s))}

instance Show AnyActions where
  show (AnyActions acts) = Prelude.show (acts @())

instance Arbitrary AnyActions where
  arbitrary = do
    Capture eval <- capture
    return (AnyActions (eval arbitrary))

  shrink (AnyActions actions) = case actions of
    Actions [] -> []
    acts -> [AnyActions (unsafeCoerce act) | act <- shrink acts]
