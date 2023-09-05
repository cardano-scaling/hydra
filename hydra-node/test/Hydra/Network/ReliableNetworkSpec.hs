{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.ReliableNetworkSpec where

import Hydra.Prelude hiding (Any, label)

import Hydra.ModelSpec (runIOSimProp)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable (property))
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel, anyActions_, forAllDL)
import Test.QuickCheck.Monadic (assert)
import Test.QuickCheck.StateModel (Action, Actions, Any (Some), LookUp, Realized, RunModel (..), StateModel (..), VarContext, runActions)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))

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

instance Monad m => RunModel ClusterState m where
  perform ::
    (Monad m, Typeable a) =>
    ClusterState ->
    Action ClusterState a ->
    LookUp m ->
    m (Realized (ReaderT ClusterState m) a)
  perform st action _ = case action of
    Noop -> pure ()

instance StateModel ClusterState where
  data Action ClusterState a where
    Noop :: Action ClusterState ()

  arbitraryAction :: VarContext -> ClusterState -> Gen (Any (Action ClusterState))
  arbitraryAction _ _ = pure $ Some Noop

runNetworkActions :: Actions ClusterState -> Property
runNetworkActions actions = property $
  runIOSimProp $ do
    _ <- runActions actions
    assert True

prop_eventuallyConsistentState :: Property
prop_eventuallyConsistentState =
  forAllDL stateIsEventuallyConsistent runNetworkActions

stateIsEventuallyConsistent :: DL ClusterState ()
stateIsEventuallyConsistent = do
  anyActions_
