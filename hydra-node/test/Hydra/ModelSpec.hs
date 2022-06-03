{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}

module Hydra.ModelSpec where

import qualified Prelude as Prelude
import Unsafe.Coerce -- This is completely safe

import Control.Monad.IOSim (IOSim, runSim, Failure)
import Hydra.Model (WorldState)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel (Actions, runActions)
import Test.QuickCheck.Gen.Unsafe

newtype WrapIOSim a = WrapIOSim { unwrapIOSim :: forall s. IOSim s a }

instance Functor WrapIOSim where
  fmap f (WrapIOSim m) = WrapIOSim (fmap f m)

instance Applicative WrapIOSim where
  WrapIOSim f <*> WrapIOSim a = WrapIOSim (f <*> a)
  pure = \ a -> WrapIOSim (pure a)

instance Monad WrapIOSim where
  WrapIOSim m >>= k = WrapIOSim (m >>= unwrapIOSim . k)

spec :: Spec
spec = prop "implementation respects model" $ prop_checkModel

runIOSimProp :: (forall s. Typeable s => Gen (IOSim s Property)) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  case runSim' (eval p) of
    Left f  -> pure $ counterexample (show f) $ property False
    Right p' -> pure $ p'

runSim' :: (forall s. Typeable s => IOSim s a) -> Either Failure a
runSim' = runSim'

newtype AnyActions = AnyActions { unAnyActions :: forall s. Typeable s => Actions (WorldState (IOSim s)) }

instance Show AnyActions where
  show (AnyActions acts) = Prelude.show (acts @())

instance Arbitrary AnyActions where
  arbitrary = do
    Capture eval <- capture
    return (AnyActions (eval arbitrary))

  shrink (AnyActions actions) = [ AnyActions (unsafeCoerceActions act) | act <- shrink (actions @()) ]

unsafeCoerceActions :: Actions (WorldState (IOSim s)) -> Actions (WorldState (IOSim s'))
unsafeCoerceActions = unsafeCoerce

prop_checkModel :: AnyActions -> Property
prop_checkModel (AnyActions actions) =
  property $ runIOSimProp $ monadic' $ do
    (_worldState, _symEnv) <- runActions actions
    let someAssertionAboutWorldState = True
    when (not someAssertionAboutWorldState) $ do
      monitor $ counterexample "What went wrong if it went wrong"
    assert someAssertionAboutWorldState
