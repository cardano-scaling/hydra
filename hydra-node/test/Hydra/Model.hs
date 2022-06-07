{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A /Model/ of the Hydra head Protocol
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
module Hydra.Model where

import Hydra.Prelude hiding (Any)

import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Show (show)
import Hydra.BehaviorSpec (createHydraNode, simulatedChainAndNetwork)
import Hydra.Cardano.Api (PaymentKey, SigningKey (PaymentSigningKey), VerificationKey, getVerificationKey)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genOneUTxOFor, genSigningKey)
import Hydra.Logging (traceInTVar)
import Hydra.Node (HydraNode, handleClientInput, runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.Snapshot (ConfirmedSnapshot)
import Test.QuickCheck (elements, listOf1, resize)
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.StateModel (Any (..), LookUp, StateModel (..), Var)

-- | Local state as seen by each Head participant.
data LocalState
  = Idle
      { idleParties :: [Party]
      , cardanoKeys :: [VerificationKey PaymentKey]
      }
  | Initial
      { headParameters :: HeadParameters
      , commits :: Committed Tx
      , pendingCommits :: PendingCommits
      }
  | Open
      { headParameters :: HeadParameters
      , offChainState :: OffChainState
      }
  deriving stock (Eq, Show)

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

isPendingCommitFrom :: Party -> LocalState -> Bool
isPendingCommitFrom party Initial{pendingCommits} =
  party `Set.notMember` pendingCommits
isPendingCommitFrom _ _ = False

data OffChainState = OffChainState
  { confirmedSnapshots :: [ConfirmedSnapshot Tx]
  , seenTransactions :: [Tx]
  }
  deriving stock (Eq, Show)

data PartyState = PartyState {cardanoKey :: CardanoSigningKey, partyState :: LocalState}
  deriving (Show)

instance Eq PartyState where
  (PartyState (PaymentSigningKey skd) ls) == (PartyState (PaymentSigningKey skd') ls') = skd == skd' && ls == ls'

-- | Global state maintained by the model.
newtype WorldState (m :: Type -> Type) = WorldState {worldState :: Map.Map Party PartyState}
  deriving (Eq, Show)

type Nodes m = Map.Map Party (HydraNode Tx m)

instance Show (HydraNode Tx m) where
  show _ = "node"

type CardanoSigningKey = SigningKey PaymentKey

instance
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadCatch m
  , MonadTime m
  , MonadFork m
  ) =>
  StateModel (WorldState m)
  where
  data Action (WorldState m) a where
    Seed :: {seedKeys :: [(Hydra.SigningKey, CardanoSigningKey)]} -> Action (WorldState m) ()
    Action ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action (WorldState m) ()

  type ActionMonad (WorldState m) = StateT (Nodes m) m

  initialState = WorldState mempty

  arbitraryAction :: WorldState m -> Gen (Any (Action (WorldState m)))
  arbitraryAction = \case
    WorldState{worldState} ->
      oneof
        [ Some . Seed <$> resize 5 (listOf1 partyKeys)
        , do
            initContestationPeriod <- arbitrary
            party <- elements $ Map.keys worldState
            let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
            pure $ Some Action{party, command}
        , do
            (party, PartyState{cardanoKey}) <- elements $ Map.toList worldState
            utxo <- genOneUTxOFor (getVerificationKey cardanoKey)
            let command = Input.Commit{Input.utxo = utxo}
            pure $ Some Action{party, command}
        ]

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState _ Seed{seedKeys} _ =
    WorldState{worldState = Map.fromList $ map mkIdle seedKeys}
   where
    idleParties = map (deriveParty . fst) seedKeys
    cardanoKeys = map (getVerificationKey . snd) seedKeys
    mkIdle p = (deriveParty $ fst p, PartyState{cardanoKey = snd p, partyState = Idle{idleParties, cardanoKeys}})
  nextState WorldState{worldState} Action{command = Input.Init{Input.contestationPeriod}} _ =
    WorldState{worldState = Map.map mkInitialState worldState}
   where
    mkInitialState = \case
      st@PartyState{partyState = Idle{idleParties}} ->
        st
          { partyState =
              Initial
                { headParameters =
                    HeadParameters
                      { parties = idleParties
                      , contestationPeriod
                      }
                , commits = mempty
                , pendingCommits = Set.fromList idleParties
                }
          }
      _ -> error "unexpected state"
  nextState WorldState{worldState} Action{party, command = Input.Commit{Input.utxo}} _ =
    WorldState{worldState = Map.map updateWithCommit worldState}
   where
    updateWithCommit = \case
      st@PartyState{partyState = Initial{headParameters, commits, pendingCommits}} ->
        st
          { partyState =
              Initial
                { headParameters
                , commits = Map.insert party utxo commits
                , pendingCommits = party `Set.delete` pendingCommits
                }
          }
      _ -> error "unexpected state"
  nextState _ _ _ = error "not implemented"

  precondition WorldState{worldState} Seed{} = null worldState
  precondition WorldState{worldState} Action{command = Input.Init{}} =
    not (null worldState) && all (isIdleState . partyState) (Map.elems worldState)
  precondition WorldState{worldState} Action{party, command = Input.Commit{}} =
    not (null worldState) && all (isPendingCommitFrom party . partyState) (Map.elems worldState)
  precondition _ _ = True

  perform :: WorldState m -> Action (WorldState m) a -> LookUp -> StateT (Nodes m) m a
  perform _worldState Seed{seedKeys} _ = do
    let parties = map (deriveParty . fst) seedKeys
    tvar <- lift $ newTVarIO []
    nodes <- lift $ do
      let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
      connectToChain <- trace "simulate chain" simulatedChainAndNetwork
      forM seedKeys $ \(sk, _csk) -> trace ("starting node " <> Hydra.Prelude.show sk) $ do
        outputs <- atomically newTQueue
        node <- trace ("creating node " <> Hydra.Prelude.show sk) $ createHydraNode ledger sk parties outputs connectToChain
        void $ async $ runHydraNode (traceInTVar tvar) node
        pure (deriveParty sk, node)

    trace ("initialised nodes " <> Hydra.Prelude.show (map fst nodes) <> ", from seed:  " <> Hydra.Prelude.show seedKeys) $ put $ Map.fromList nodes
  perform _ Action{party, command} _lookupVar = do
    nodes <- get
    case Map.lookup party nodes of
      Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
      Just actorNode -> lift $ actorNode `handleClientInput` command

partyKeys :: Gen (Hydra.SigningKey, CardanoSigningKey)
partyKeys = do
  sk <- arbitrary
  csk <- genSigningKey
  pure (sk, csk)

instance Show (Action (WorldState m) a) where
  show (Seed sks) = "Seed { seedKeys = " <> Hydra.Prelude.show sks <> "}"
  show (Action pa ci) = "Action { party = " <> Hydra.Prelude.show pa <> ", command = " <> Hydra.Prelude.show ci <> "}"

instance Eq (Action (WorldState m) a) where
  (Seed sks) == (Seed sks') = map fst sks == map fst sks'
  (Action pa ci) == (Action pa' ci') = pa == pa' && ci == ci'
  _ == _ = False
