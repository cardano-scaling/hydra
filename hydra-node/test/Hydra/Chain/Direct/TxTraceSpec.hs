module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (State)
import Test.Hydra.Prelude
import Test.QuickCheck (elements, forAll)

data State
  = Open
  | Closed
  | Final

data Transition
  = Close
  | Contest
  | Fanout
  deriving (Show)

genTransition :: State -> Gen Transition
genTransition = \case
  Open -> pure Close
  Closed -> elements [Contest, Fanout]
  Final -> undefined

startingState :: State
startingState = Open

nextState :: State -> Transition -> State
nextState s t =
  case (s, t) of
    (Open, Close) -> Closed
    (Closed, Contest) -> Closed
    (Closed, Fanout) -> Final
    _ -> s

genTrace :: Gen [Transition]
genTrace = go [] startingState
 where
  go ts s = do
    t <- genTransition s
    let s' = nextState s t
    case s' of
      Final -> pure $ t : ts
      _ -> go (t : ts) s'

spec :: Spec
spec =
  prop "generates trace of transitions" $ do
    forAll genTrace $ \transitions ->
      not $ null transitions
