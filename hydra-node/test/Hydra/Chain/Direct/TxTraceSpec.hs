module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (Any, State, label)
import Test.Hydra.Prelude

import Data.Map.Strict qualified as Map
import Hydra.Chain.Direct.Contract.Close (healthyCloseTx)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Snapshot (Snapshot)
import Test.QuickCheck (Property, Smart (..), checkCoverage, cover, elements, forAll)
import Test.QuickCheck.Monadic (monadicIO)
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
  runActions,
 )

data Model = Model {snapshots :: [Snapshot Tx], headState :: State} deriving (Show)

data State
  = Open
  | Closed
  | Final
  deriving (Show)

instance StateModel Model where
  data Action Model a where
    ProduceSnapshots :: [Snapshot Tx] -> Action Model ()
    Close :: Action Model ()
    Contest :: Action Model ()
    Fanout :: Action Model ()
    -- \| Helper action to identify the terminal state 'Final' and shorten
    -- traces using the 'precondition'.
    Stop :: Action Model ()

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState} =
    case headState of
      Open -> pure $ Some Close
      Closed -> Some <$> elements [Contest, Fanout]
      Final -> pure $ Some Stop

  initialState = Model{snapshots = [], headState = Open}

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m Stop _ = m
  nextState m t _ =
    m
      { headState =
          case (headState m, t) of
            (Open, Close) -> Closed
            (Closed, Contest) -> Closed
            (Closed, Fanout) -> Final
            _ -> headState m
      }

  precondition :: Model -> Action Model a -> Bool
  precondition Model{headState = Final} Stop = False
  precondition _ _ = True

instance HasVariables Model where
  getAllVariables = mempty

instance HasVariables (Action Model a) where
  getAllVariables = mempty

deriving instance Eq (Action Model a)
deriving instance Show (Action Model a)

instance RunModel Model IO where
  perform :: Model -> Action Model a -> LookUp IO -> IO a
  perform m action _lookup = do
    case headState m of
      Open -> putStrLn "=========OPEN======="
      _ -> pure ()

    putStrLn $ "performing action: " <> show action

    case action of
      ProduceSnapshots _snapshots -> pure ()
      Close -> do
        let (tx, utxo) = healthyCloseTx

        case evaluateTx tx utxo of
          Left err ->
            fail $ show err
          Right redeemerReport ->
            when (any isLeft (Map.elems redeemerReport)) $
              fail $
                "Some redeemers failed: " <> show redeemerReport
      Contest -> pure ()
      Fanout -> pure ()
      Stop -> pure ()

spec :: Spec
spec = do
  prop "generates interesting transaction traces" prop_traces
  prop "all valid transactions" prop_runActions

prop_traces :: Property
prop_traces =
  forAll (arbitrary :: Gen (Actions Model)) $ \(Actions_ _ (Smart _ steps)) ->
    checkCoverage $
      True
        & cover 1 (null steps) "empty"
        & cover 10 (hasFanout steps) "reach fanout"
        & cover 5 (countContests steps >= 2) "has multiple contests"
        & cover 5 (containSomeSnapshots steps) "has some snapshots"
 where
  containSomeSnapshots =
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        ProduceSnapshots snapshots -> not $ null snapshots
        _ -> False

  hasFanout =
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        Fanout{} -> True
        _ -> False

  countContests =
    length
      . filter
        ( \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
            Contest{} -> True
            _ -> False
        )

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  monadicIO $
    void (runActions actions)
