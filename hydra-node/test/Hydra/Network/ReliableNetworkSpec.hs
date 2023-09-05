{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.ReliableNetworkSpec where

import Hydra.Prelude hiding (Any, label)

import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable (property))
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel, anyActions_, forAllDL)
import Test.QuickCheck.Monadic (assert, PropertyM)
import Test.QuickCheck.StateModel (Action, Actions, Any (Some), LookUp, Realized, RunModel (..), StateModel (..), VarContext, runActions)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))
import Control.Monad.IOSim (IOSim)

spec :: Spec
spec = do
  prop "State between nodes is eventually consistent" prop_eventuallyConsistentState

data ClusterState = ClusterState
  deriving (Show, Eq, Generic)

instance DynLogicModel ClusterState
instance HasVariables (Action ClusterState a) where
  getAllVariables = mempty
deriving instance Show (Action ClusterState a)
deriving instance Eq (Action ClusterState a)

newtype OurMonad m a = OurMonad {ourMonad :: ReaderT ClusterState m a}
  deriving (Functor, Applicative, Monad, MonadReader ClusterState)

type instance Realized (OurMonad m) a = a

instance Monad m => RunModel ClusterState (OurMonad m) where
  perform st action _ = case action of
    Noop -> pure ()

instance StateModel ClusterState where
  data Action ClusterState a where
    Noop :: Action ClusterState ()

  arbitraryAction :: VarContext -> ClusterState -> Gen (Any (Action ClusterState))
  arbitraryAction _ _ = pure $ Some Noop

  initialState = ClusterState

runNetworkActions :: Actions ClusterState -> Property
runNetworkActions actions = property $
  runIOSimProp $ do
    _ <- runActions actions
    assert True

-- | Specialised runner similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Monadic.html#runSTGen>.
runIOSimProp :: Testable a => (forall s. PropertyM (OurMonad (IOSim s)) a) -> Gen Property
runIOSimProp p = undefined

prop_eventuallyConsistentState :: Property
prop_eventuallyConsistentState =
  forAllDL stateIsEventuallyConsistent runNetworkActions

stateIsEventuallyConsistent :: DL ClusterState ()
stateIsEventuallyConsistent = do
  anyActions_
