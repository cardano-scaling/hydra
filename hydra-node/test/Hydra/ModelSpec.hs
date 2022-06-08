{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Unsafe.Coerce (unsafeCoerce)

-- This is completely safe
import qualified Prelude

import Control.Monad.IOSim (IOSim, runSim)
import Data.Map ((!))
import Hydra.BehaviorSpec (TestHydraNode (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Model (LocalState (..), Nodes, PartyState (..), WorldState (..))
import Hydra.ServerOutput (ServerOutput (..))
import Test.QuickCheck (Property, counterexample, property)
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic', monitor, run)
import Test.QuickCheck.StateModel (Actions, Env, initialState, runActionsInState, pattern Actions)

import qualified Data.Map as Map

newtype WrapIOSim a = WrapIOSim {unwrapIOSim :: forall s. IOSim s a}

instance Functor WrapIOSim where
  fmap f (WrapIOSim m) = WrapIOSim (fmap f m)

instance Applicative WrapIOSim where
  WrapIOSim f <*> WrapIOSim a = WrapIOSim (f <*> a)
  pure a = WrapIOSim (pure a)

instance Monad WrapIOSim where
  WrapIOSim m >>= k = WrapIOSim (m >>= unwrapIOSim . k)

spec :: Spec
spec =
  modifyMaxSuccess (const 1000) $
    prop "implementation respects model" prop_checkModel

runIOSimProp :: (forall s. Gen (StateT (Nodes (IOSim s)) (IOSim s) Property)) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  case runSim $ evalStateT (eval p) mempty of
    Left f -> pure $ counterexample (show f) $ property False
    Right p' -> pure p'

newtype AnyActions = AnyActions {unAnyActions :: forall s. Actions (WorldState (IOSim s))}

instance Show AnyActions where
  show (AnyActions acts) = Prelude.show (acts @())

instance Arbitrary AnyActions where
  arbitrary = do
    Capture eval <- capture
    return (AnyActions (eval arbitrary))

  shrink (AnyActions actions) = case actions of
    Actions [] -> []
    acts -> [AnyActions (unsafeCoerceActions act) | act <- shrink acts]

unsafeCoerceActions :: Actions (WorldState (IOSim s)) -> Actions (WorldState (IOSim s'))
unsafeCoerceActions = unsafeCoerce

prop_checkModel :: AnyActions -> Property
prop_checkModel (AnyActions actions) =
  property $
    runIOSimProp $
      monadic' $ do
        (WorldState world, _symEnv) <- runActions' actions
        let parties = Map.keysSet world

        nodes <- error "think about it."

        assert (parties == Map.keysSet nodes)

        forM_ parties $ \party -> do
          let st = world ! party
          let node = nodes ! party
          case partyState st of
            Initial{commits} -> do
              outputs <- serverOutputs @Tx node
              let actualCommitted =
                    Map.fromList
                      [ (party, utxo)
                      | Committed{party, utxo} <- outputs
                      ]
              monitor $
                counterexample $
                  toString $
                    unlines
                      [ "Actual committed:   " <> show actualCommitted
                      , "Expected committed: " <> show commits
                      ]
              assert (actualCommitted == commits)
            _ -> do
              pure ()

runActions' ::
  Actions (WorldState (IOSim s)) ->
  PropertyM (StateT (Nodes (IOSim s)) (IOSim s)) (WorldState (IOSim s), Env)
runActions' = runActionsInState initialState
