{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.ReliableNetworkSpec where

import Hydra.Prelude hiding (Any, label)

import Control.Monad.IOSim (IOSim)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable (property))
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel, anyActions_, forAllDL)
import Test.QuickCheck.Monadic (PropertyM, assert)
import Test.QuickCheck.StateModel (Action, Actions, Any (Some), LookUp, Realized, RunModel (..), StateModel (..), VarContext, runActions)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))

spec :: Spec
spec = do
  prop "State between nodes is eventually consistent" prop_eventuallyConsistentState

data ClusterModel = ClusterModel
  deriving (Show, Eq, Generic)

data Cluster = Cluster

instance DynLogicModel ClusterModel
instance HasVariables (Action ClusterModel a) where
  getAllVariables = mempty
deriving instance Show (Action ClusterModel a)
deriving instance Eq (Action ClusterModel a)

newtype RunMonad m a = RunMonad {runMonad :: ReaderT Cluster m a}
  deriving (Functor, Applicative, Monad, MonadReader Cluster)

type instance Realized (RunMonad m) a = a

instance Monad m => RunModel ClusterModel (RunMonad m) where
  perform st action _ = case action of
    Noop -> pure ()

instance StateModel ClusterModel where
  data Action ClusterModel a where
    Noop :: Action ClusterModel ()

  arbitraryAction :: VarContext -> ClusterModel -> Gen (Any (Action ClusterModel))
  arbitraryAction _ _ = pure $ Some Noop

  initialState = ClusterModel

runNetworkActions :: Actions ClusterModel -> Property
runNetworkActions actions = property $
  runIOSimProp $ do
    _ <- runActions actions
    assert True

-- | Specialised runner similar to <runSTGen https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Monadic.html#runSTGen>.
runIOSimProp :: Testable a => (forall s. PropertyM (RunMonad (IOSim s)) a) -> Gen Property
runIOSimProp p = undefined

prop_eventuallyConsistentState :: Property
prop_eventuallyConsistentState =
  forAllDL stateIsEventuallyConsistent runNetworkActions

stateIsEventuallyConsistent :: DL ClusterModel ()
stateIsEventuallyConsistent = do
  anyActions_
