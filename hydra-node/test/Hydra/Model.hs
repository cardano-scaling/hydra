{-# OPTIONS_GHC -Wno-orphans #-}

-- | A /Model/ of the Hydra head Protocol
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
module Hydra.Model where

import Hydra.Prelude hiding (Any)

import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Show (show)
import Hydra.BehaviorSpec (createHydraNode, simulatedChainAndNetwork)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger.Cardano (Tx, cardanoLedger)
import Hydra.Logging (traceInTVar)
import Hydra.Node (HydraNode, handleClientInput, runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot)
import Test.QuickCheck (elements)
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.StateModel (Any (..), LookUp, StateModel (..), Var (Var))

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
newtype WorldState (m :: Type -> Type) = WorldState {worldState :: Map.Map Party LocalState}
  deriving (Eq, Show)

type Nodes m = Map.Map Party (HydraNode Tx m)

instance Show (HydraNode Tx m) where
  show _ = "node"

instance
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadCatch m
  , MonadTime m
  , MonadFork m
  , Typeable m
  ) =>
  StateModel (WorldState m)
  where
  data Action (WorldState m) a where
    Seed :: {seedKeys :: [Hydra.SigningKey]} -> Action (WorldState m) (Nodes m)
    Action ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action (WorldState m) ()

  type ActionMonad (WorldState m) = m

  initialState = WorldState mempty

  arbitraryAction :: WorldState m -> Gen (Any (Action (WorldState m)))
  arbitraryAction = \case
    WorldState{worldState} ->
      oneof
        [ Some . Seed <$> reasonablySized arbitrary
        , do
            initContestationPeriod <- arbitrary
            party <- elements $ Map.keys worldState
            let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
            pure $ Some Action{party, command}
        ]

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState _ Seed{seedKeys} _ =
    WorldState{worldState = Map.fromList $ map mkIdle idleParties}
   where
    idleParties = map deriveParty seedKeys
    mkIdle p = (p, Idle{idleParties})
  nextState WorldState{worldState} Action{command = Input.Init{Input.contestationPeriod}} _ =
    WorldState{worldState = Map.map mkInitialState worldState}
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

  precondition WorldState{worldState} Seed{} = null worldState
  precondition WorldState{worldState} Action{command = Input.Init{}} = all isIdleState (Map.elems worldState)
  precondition _ _ = True

  perform :: WorldState m -> Action (WorldState m) a -> LookUp -> m a
  perform _worldState Seed{seedKeys} _ = do
    let parties = map deriveParty seedKeys
    tvar <- newTVarIO []
    nodes <- do
      let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
      connectToChain <- simulatedChainAndNetwork
      forM seedKeys $ \sk -> do
        outputs <- atomically newTQueue
        node <- createHydraNode ledger sk parties outputs connectToChain
        runHydraNode (traceInTVar tvar) node
        pure (deriveParty sk, node)

    pure $ Map.fromList nodes
  perform _ Action{party, command} lookupVar = do
    let nodes = lookupVar (Var 0 :: Var (Nodes m))
    case Map.lookup party nodes of
      Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
      Just actorNode -> actorNode `handleClientInput` command

instance Show (Action (WorldState m) a) where
  show (Seed sks) = "Seed { seedKeys = " <> Hydra.Prelude.show sks <> "}"
  show (Action pa ci) = "Action { party = " <> Hydra.Prelude.show pa <> ", command = " <> Hydra.Prelude.show ci <> "}"

instance Eq (Action (WorldState m) a) where
  (Seed sks) == (Seed sks') = sks == sks'
  (Action pa ci) == (Action pa' ci') = pa == pa' && ci == ci'
