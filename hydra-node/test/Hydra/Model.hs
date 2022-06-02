{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A /Model/ of the Hydra head Protocol
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
module Hydra.Model where

import Control.Monad.Class.MonadSTM (newTQueue)
import Control.Monad.IOSim (runSimTrace, traceResult)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.BehaviorSpec (createHydraNode, simulatedChainAndNetwork)
import Hydra.Cardano.Api (UTxO)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger.Cardano (Tx, cardanoLedger)
import Hydra.Node (HydraNode, handleClientInput)
import Hydra.Party (Party, deriveParty)
import Hydra.Prelude hiding (State)
import Hydra.Snapshot (ConfirmedSnapshot)
import Test.QuickCheck (Property, elements, forAll, property, (===))
import Test.QuickCheck.Property (counterexample)
import Test.QuickCheck.StateModel (StateModel (..), Var)

-- | Local state as seen by each Head participant.
data LocalState
  = Idle {idleParties :: [Party]}
  | Initial
      { headParameters :: HeadParameters
      , commits :: Committed Tx
      , pendingCommits :: PendingCommits
      }
  deriving stock (Eq, Show)

--  Open
--     { headParameters :: HeadParameters
--     , offChainState :: OffChainState
--     }

--  Closed
--     { headParameters :: HeadParameters
--     , confirmedSnapshot :: ConfirmedSnapshot Tx
--     , offChainState :: OffChainState
--     }
--  Final {finalUTxO :: UTxO}

isFinalState :: LocalState -> Bool
isFinalState Initial{} = True
isFinalState _ = False

isIdleState :: LocalState -> Bool
isIdleState Idle{} = True
isIdleState _ = False

data OffChainState = OffChainState
  { confirmedSnapshots :: [ConfirmedSnapshot Tx]
  , seenTransactions :: [Tx]
  }
  deriving stock (Eq, Show)

-- | Global state maintained by the model.
newtype GlobalState = GlobalState {globalState :: Map.Map Party LocalState}
  deriving stock (Eq, Show)

instance StateModel GlobalState where
  data Action GlobalState a where
    Seed :: {seedParties :: [Party]} -> Action GlobalState ()
    Action ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action GlobalState ()

  initialState = GlobalState mempty

  arbitraryAction :: GlobalState -> Gen (Any (Action GlobalState))
  arbitraryAction = \case
    GlobalState{globalState}
      | isEmpty globalState -> Seed . reasonableSized <$> arbitrary
      | all isIdleState (Map.elems globalState) -> do
        initContestationPeriod <- arbitrary
        party <- elements $ Map.keys globalState
        let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
        pure $ Action{party, command}
      | otherwise -> error "not implemented"

  nextState :: GlobalState -> Action GlobalState a -> Var a -> GlobalState
  nextState GlobalState{globalState} Seed{seedParties} _ =
    GlobalState{globalState = Map.fromList $ map (,seedParties) seedParties}
  nextState GlobalState{globalState} Action{command = Input.Init{Input.contestationPeriod}} _ =
    GlobalState{globalState = Map.map mkInitialState globalState}
   where
    mkInitialState = \case
      Idle{idleParties} ->
        Initial
          { headParameters =
              HeadParameters
                { parties = idleParties
                , contestationPeriod
                }
          , commits = mempty
          , pendingCommits = Set.fromList idleParties
          }
      _ -> error "unexpected state"
  nextState _ _ _ = error "not implemented"

  precondition GlobalState{globalState} Seed{seedParties} = isEmpty globalState
  precondition GlobalState{globalState} Action{command = Input.Init{}} =
sequenceOfActions :: [Party] -> Gen [Action]
sequenceOfActions idleParties = go (startState idleParties) []
 where
  go st@GlobalState{globalState} acc
    | shouldStop globalState = pure $ reverse acc
    | otherwise = generateAction st >>= \a -> go (interpret a st) (a : acc)

  shouldStop = (== 1) . Map.size

-- all isFinalState . Map.elems

startState :: [Party] -> GlobalState
startState idleParties = GlobalState{globalState = Map.fromList $ map (,Idle{idleParties}) idleParties}
--

-- * Properties

--

prop_checkModel :: Property
prop_checkModel =
  forAll (reasonablySized arbitrary) $ \hydraKeys ->
    let parties = map deriveParty hydraKeys
     in forAll (sequenceOfActions parties) $ \actions ->
          let result =
                traceResult False $
                  runSimTrace $
                    evalStateT
                      ( runWorld $ do
                          initWorld parties hydraKeys
                          traverse_ runAction actions
                          getState
                      )
                      WorldState{worldState = mempty}
              expected = foldr interpret (startState parties) actions
           in case result of
                Right actual -> actual === expected
                Left err -> property False & counterexample (show err)

class World m where
  initWorld :: [Party] -> [Hydra.SigningKey] -> m ()
  runAction :: Action GlobalState () -> m ()
  getState :: m GlobalState

newtype WorldMonad m a = WorldMonad {runWorld :: StateT (WorldState m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadState (WorldState m))

instance MonadTrans WorldMonad where
  lift m = WorldMonad $ StateT $ \s -> (,s) <$> m

newtype WorldState m = WorldState {worldState :: Map.Map Party (Actor m)}

data Actor m = Actor {actorState :: LocalState, actorNode :: HydraNode Tx m}

instance (MonadSTM m, MonadAsync m, MonadDelay m) => World (WorldMonad m) where
  initWorld parties hydraKeys = do
    nodes <- lift $ do
      let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
      connectToChain <- simulatedChainAndNetwork
      forM hydraKeys $ \sk -> do
        outputs <- atomically newTQueue
        node <- createHydraNode ledger sk parties outputs connectToChain
        runHydraNode tracer node
        pure node

    put $
      WorldState
        { worldState =
            Map.fromList $
              zip parties $
                map (\n -> Actor{actorState = Idle{idleParties = parties}, actorNode = n}) nodes
        }

  runAction Action{party, command} = do
    WorldState{worldState} <- get
    case Map.lookup party worldState of
      Nothing -> error $ "unexpected party " <> show party
      Just Actor{actorNode} -> lift $ actorNode `handleClientInput` command

  getState = gets worldState <&> (GlobalState . Map.map actorState)

selectActor :: Monad m => Party -> WorldMonad m (HydraNode Tx m)
selectActor party = do
  actors <- gets worldState
  case Map.lookup party actors of
    Nothing -> error $ "unknown party " <> show party
    Just (actorNode -> node) -> pure node
