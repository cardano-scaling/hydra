{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.ModelSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Unsafe.Coerce (unsafeCoerce)

-- This is completely safe
import qualified Prelude

import Control.Monad.IOSim (IOSim, runSim)
import Hydra.Model (WorldState (WorldState))
import Test.QuickCheck (Property, counterexample, property)
import Test.QuickCheck.Gen.Unsafe (Capture (Capture), capture)
import Test.QuickCheck.Monadic (assert, monadic', monitor)
import Test.QuickCheck.StateModel (Actions, runActions)

newtype WrapIOSim a = WrapIOSim {unwrapIOSim :: forall s. IOSim s a}

instance Functor WrapIOSim where
  fmap f (WrapIOSim m) = WrapIOSim (fmap f m)

instance Applicative WrapIOSim where
  WrapIOSim f <*> WrapIOSim a = WrapIOSim (f <*> a)
  pure a = WrapIOSim (pure a)

instance Monad WrapIOSim where
  WrapIOSim m >>= k = WrapIOSim (m >>= unwrapIOSim . k)

spec :: Spec
spec = prop "implementation respects model" prop_checkModel

runIOSimProp :: (forall s. Gen (IOSim s Property)) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  case runSim (eval p) of
    Left f -> pure $ counterexample (show f) $ property False
    Right p' -> pure p'

newtype AnyActions = AnyActions {unAnyActions :: forall s. Typeable s => Actions (WorldState (IOSim s))}

instance Show AnyActions where
  show (AnyActions acts) = Prelude.show (acts @())

instance Arbitrary AnyActions where
  arbitrary = do
    Capture eval <- capture
    return (AnyActions (eval arbitrary))

  shrink (AnyActions actions) = [AnyActions (unsafeCoerceActions act) | act <- shrink (actions @())]

unsafeCoerceActions :: Actions (WorldState (IOSim s)) -> Actions (WorldState (IOSim s'))
unsafeCoerceActions = unsafeCoerce

prop_checkModel :: AnyActions -> Property
prop_checkModel (AnyActions actions) =
  property $
    runIOSimProp $
      monadic' $ do
        (WorldState{}, _symEnv) <- runActions actions
        let someAssertionAboutWorldState = True
        when (not someAssertionAboutWorldState) $ do
          monitor $ counterexample "What went wrong if it went wrong"
        assert someAssertionAboutWorldState
