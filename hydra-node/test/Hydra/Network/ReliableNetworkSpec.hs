{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.ReliableNetworkSpec where

import Hydra.Prelude hiding (Any, label)

import Control.Monad.IOSim (IOSim, runSimTrace, traceResult)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable (property), counterexample)
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel, anyActions_, forAllDL)
import Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic')
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
  deriving newtype (Functor, Applicative, Monad, MonadReader Cluster)

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
runIOSimProp p = do
  Capture eval <- capture
  let tr =
        -- p           :: PropertyM (RunMonad (IOSim s)) a
        -- monadic'    :: PropertyM (RunMonad (IOSim s)) a -> Gen (RunMonad (IOSim s))
        -- eval        :: Gen (RunMonad (IOSim s)) -> (RunMonad (IOSim s))
        -- runMonad    :: (RunMonad (IOSim s)) a -> ReaderT Cluster (IOSim s) a
        -- runReaderT  :: ReaderT Cluster (IOSim s) a -> Cluster -> IOSim s a
        -- runSimTrace :: IOSim s a -> Trace a
        runSimTrace $ runReaderT (runMonad $ eval $ monadic' p) Cluster
  case traceResult False tr of
    Right x ->
      pure x
    Left ex ->
      pure $ counterexample (show ex) $ property False

prop_eventuallyConsistentState :: Property
prop_eventuallyConsistentState =
  forAllDL stateIsEventuallyConsistent runNetworkActions

stateIsEventuallyConsistent :: DL ClusterModel ()
stateIsEventuallyConsistent = do
  anyActions_
