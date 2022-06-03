{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}

module Hydra.ModelSpec where

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
spec = spec --prop "implementation respects model" $ prop_checkModel

runIOSimProp :: (forall s. Typeable s => Gen (IOSim s Property)) -> Gen Property
runIOSimProp p = do
  Capture eval <- capture
  case runSim' (eval p) of
    Left f  -> pure $ counterexample (show f) $ property False
    Right p' -> pure $ p'

runSim' :: (forall s. Typeable s => IOSim s a) -> Either Failure a
runSim' = runSim'

prop_checkModel :: Property
prop_checkModel =
  property $ runIOSimProp $ monadic' m
  where m :: forall s. Typeable s => PropertyM (IOSim s) ()
        m = do actions <- pickSane (arbitrary @(Actions (WorldState (IOSim s)))) shrink
               (_worldState, _symEnv) <- runActions actions
               let someAssertionAboutWorldState = True
               when (not someAssertionAboutWorldState) $ do
                 monitor $ counterexample "What went wrong if it went wrong"
               assert someAssertionAboutWorldState

pickSane :: (Monad m, Show a) => Gen a -> (a -> [a]) -> PropertyM m a
pickSane gen shrinker = MkPropertyM $ \k ->
  do a <- gen
     mp <- k a
     return (do p <- mp
                return (forAllShrink (return a) shrinker (const p)))

-- monadicST :: Testable a => (forall s. PropertyM (ST s) a) -> Property
-- monadicST m = property (runSTGen (monadic' m))
--
-- runSTGen :: (forall s. Gen (ST s a)) -> Gen a
-- runSTGen f = do
--   Capture eval <- capture
--   return (runST (eval f))
