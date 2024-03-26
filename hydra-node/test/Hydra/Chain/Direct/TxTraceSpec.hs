module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (Any, State, label)
import Test.Hydra.Prelude

import Debug.Trace (traceM)
import Test.QuickCheck (Property, Smart (..), checkCoverage, cover, elements, forAll)
import Test.QuickCheck.StateModel (
  ActionWithPolarity (..),
  Actions (..),
  Any (..),
  HasVariables (getAllVariables),
  LookUp,
  RunModel (..),
  StateModel (..),
  Step ((:=)),
  Var,
  VarContext,
 )

data State
  = Open
  | Closed
  | Final
  deriving (Show)

instance StateModel State where
  data Action State a where
    Close :: Action State ()
    Contest :: Action State ()
    Fanout :: Action State ()
    Stop :: Action State ()

  arbitraryAction :: VarContext -> State -> Gen (Any (Action State))
  arbitraryAction _lookup = \case
    Open -> pure $ Some Close
    Closed -> Some <$> elements [Contest, Fanout]
    Final -> pure $ Some Stop

  initialState = Open

  nextState :: State -> Action State a -> Var a -> State
  nextState s t _ =
    case (s, t) of
      (_, Stop) -> s
      (Open, Close) -> Closed
      (Closed, Contest) -> Closed
      (Closed, Fanout) -> Final
      _ -> s

instance HasVariables State where
  getAllVariables = mempty

instance HasVariables (Action State a) where
  getAllVariables = mempty

deriving instance Eq (Action State a)
deriving instance Show (Action State a)

instance RunModel State Identity where
  perform :: State -> Action State a -> LookUp Identity -> Identity a
  perform _s action _lookup = do
    traceM $ "performing action: " <> show action
    case action of
      Close -> pure ()
      Contest -> pure ()
      Fanout -> pure ()
      Stop -> pure ()

spec :: Spec
spec =
  prop "generates trace of transitions" prop_traces

prop_traces :: Property
prop_traces =
  forAll (arbitrary :: Gen (Actions State)) $ \(Actions_ _ (Smart _ steps)) ->
    checkCoverage $
      True
        & cover 1 (null steps) "empty"
        & cover 10 (hasFanout steps) "reach fanout"
        & cover 5 (countContests steps >= 2) "has multiple contests"
 where
  hasFanout =
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        Fanout{} -> True
        _ -> False
  countContests s =
    length $
      filter
        ( \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
            Contest{} -> True
            _ -> False
        )
        s
