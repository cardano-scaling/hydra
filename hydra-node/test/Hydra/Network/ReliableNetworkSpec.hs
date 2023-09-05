{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.ReliableNetworkSpec where

import Hydra.Prelude hiding (Any, label)

import Control.Monad.IOSim (IOSim, runSimTrace, traceResult)
import Data.List ((\\))
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Smart (..), Testable (property), counterexample, elements, frequency)
import Test.QuickCheck.DynamicLogic (DL, DynLogicModel, action, anyAction, anyActions_, forAllDL, getModelStateDL)
import Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import Test.QuickCheck.Monadic (PropertyM, assert, monadic')
import Test.QuickCheck.StateModel (Action, Actions (..), Annotated (Metadata), Any (Some), Realized, RunModel (..), StateModel (..), Step (..), VarContext, runActions)
import Test.QuickCheck.StateModel.Variables (HasVariables (..), Var)

spec :: Spec
spec = do
  prop "State between nodes is eventually consistent" prop_eventuallyConsistentState

--
-- Cluster model, constructor and utility functions
--
data ClusterModel = ClusterModel
  { aliceSent :: [Int]
  , bobReceived :: [Int]
  , connected :: Bool
  }
  deriving (Show, Eq, Generic)

newClusterModel :: ClusterModel
newClusterModel =
  ClusterModel
    { aliceSent = mempty
    , bobReceived = mempty
    , connected = False
    }

aliceSends :: ClusterModel -> Int -> ClusterModel
aliceSends model@ClusterModel{aliceSent} i = model{aliceSent = aliceSent <> [i]}

bobReceives :: ClusterModel -> Int -> ClusterModel
bobReceives model@ClusterModel{bobReceived} i = model{bobReceived = bobReceived <> [i]}

connectionUp :: ClusterModel -> ClusterModel
connectionUp model = model{connected = True}

connectionDown :: ClusterModel -> ClusterModel
connectionDown model = model{connected = False}

pendingMessages :: ClusterModel -> [Int]
pendingMessages ClusterModel{aliceSent, bobReceived} = aliceSent \\ bobReceived

--

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
  perform _ action _ = case action of
    AliceSends{} -> pure ()
    BobReceives{} -> pure ()
    ConnectionUp{} -> pure ()
    ConnectionDown{} -> pure ()

  postcondition _ _ _ _ = pure True

instance StateModel ClusterModel where
  data Action ClusterModel a where
    ConnectionUp :: Action ClusterModel ()
    ConnectionDown :: Action ClusterModel ()
    AliceSends :: Int -> Action ClusterModel ()
    BobReceives :: Int -> Action ClusterModel ()

  arbitraryAction :: VarContext -> ClusterModel -> Gen (Any (Action ClusterModel))
  arbitraryAction _ model =
    frequency
      [ (5, Some . AliceSends <$> arbitrary)
      , (5, Some . BobReceives <$> elements (pendingMessages model))
      , (1, pure $ Some ConnectionDown)
      , (1, pure $ Some ConnectionUp)
      ]

  initialState = newClusterModel

  precondition :: ClusterModel -> Action ClusterModel a -> Bool
  precondition model (BobReceives message) = message `elem` pendingMessages model
  precondition _ _ = True

  nextState :: ClusterModel -> Action ClusterModel a -> Var a -> ClusterModel
  nextState model act _ =
    case act of
      AliceSends i -> aliceSends model i
      BobReceives i -> bobReceives model i
      ConnectionUp -> connectionUp model
      ConnectionDown -> connectionDown model

propNoPendingMessages :: Actions ClusterModel -> Property
propNoPendingMessages actions = property $
  runIOSimProp $ do
    (Metadata _vars model, _env) <- runActions actions
    assert $ null (pendingMessages model)

prop_eventuallyConsistentState :: Property
prop_eventuallyConsistentState =
  forAllDL allMessagesAreEventuallyDelivered propNoPendingMessages

allMessagesAreEventuallyDelivered :: DL ClusterModel ()
allMessagesAreEventuallyDelivered = do
  anyActions_
  model <- getModelStateDL
  bobReceivesAllPendingMessages model
 where
  bobReceivesAllPendingMessages model = do
    _ <- foldlM dtc () [BobReceives m | m <- pendingMessages model]
    pure ()
   where
    dtc acc a = do
      action a
      pure ()

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