{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A /Model/ of the Hydra head Protocol
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
module Hydra.Model where

import Hydra.Prelude hiding (Any, label)

import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.BehaviorSpec (TestHydraNode, createHydraNode, createTestHydraNode, send, simulatedChainAndNetwork, waitUntil)
import Hydra.Cardano.Api (PaymentKey, SigningKey (PaymentSigningKey), UTxO, VerificationKey, getVerificationKey)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger.Cardano (Tx, cardanoLedger, genOneUTxOFor, genSigningKey)
import Hydra.Logging (traceInTVar)
import Hydra.Node (runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (ReadyToCommit))
import Test.QuickCheck (elements, label, listOf1, resize)
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
  | Final {finalUTxO :: UTxO}
  deriving stock (Eq, Show)

--  Closed
--     { headParameters :: HeadParameters
--     , confirmedSnapshot :: ConfirmedSnapshot Tx
--     , offChainState :: OffChainState
--     }
--  Final {finalUTxO :: UTxO}

isInitialState :: LocalState -> Bool
isInitialState Initial{} = True
isInitialState _ = False

isFinalState :: LocalState -> Bool
isFinalState Final{} = True
isFinalState _ = False

isIdleState :: LocalState -> Bool
isIdleState Idle{} = True
isIdleState _ = False

isPendingCommitFrom :: Party -> LocalState -> Bool
isPendingCommitFrom party Initial{pendingCommits} =
  party `Set.member` pendingCommits
isPendingCommitFrom _ _ = False

data OffChainState = OffChainState
  { confirmedSnapshots :: [UTxO]
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

type Nodes m = Map.Map Party (TestHydraNode Tx m)

type CardanoSigningKey = SigningKey PaymentKey

instance Eq CardanoSigningKey where
  (PaymentSigningKey skd) == (PaymentSigningKey skd') = skd == skd'

instance
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadCatch m
  , MonadTime m
  , MonadTimer m
  , MonadFork m
  ) =>
  StateModel (WorldState m)
  where
  data Action (WorldState m) a where
    Seed ::
      { seedKeys :: [(Hydra.SigningKey, CardanoSigningKey)]
      } ->
      Action (WorldState m) ()
    Init ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action (WorldState m) ()
    Commit ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action (WorldState m) ()
    Abort ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action (WorldState m) ()

  type ActionMonad (WorldState m) = StateT (Nodes m) m

  initialState = WorldState mempty

  arbitraryAction :: WorldState m -> Gen (Any (Action (WorldState m)))
  arbitraryAction = \case
    WorldState{worldState}
      | null worldState ->
        genSeed
      | otherwise ->
        oneof
          [ genInit
          , genCommit
          , genAbort
          ]
     where
      genSeed = Some . Seed <$> resize 5 (listOf1 partyKeys)

      genInit = do
        initContestationPeriod <- arbitrary
        party <- elements $ Map.keys worldState
        let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
        pure $ Some Init{party, command}

      genCommit = do
        (party, PartyState{cardanoKey}) <- elements $ Map.toList worldState
        utxo <- genOneUTxOFor (getVerificationKey cardanoKey)
        let command = Input.Commit{Input.utxo = utxo}
        pure $ Some Commit{party, command}

      genAbort = do
        (party, _) <- elements $ Map.toList worldState
        pure $ Some Abort{party, command = Input.Abort}

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState _ Seed{seedKeys} _ =
    WorldState{worldState = Map.fromList $ map mkIdle seedKeys}
   where
    idleParties = map (deriveParty . fst) seedKeys
    cardanoKeys = map (getVerificationKey . snd) seedKeys
    mkIdle p = (deriveParty $ fst p, PartyState{cardanoKey = snd p, partyState = Idle{idleParties, cardanoKeys}})
  nextState WorldState{worldState} Init{command = Input.Init{Input.contestationPeriod}} _ =
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
  nextState WorldState{worldState} Commit{party, command = Input.Commit{Input.utxo}} _ =
    WorldState{worldState = Map.map updateWithCommit worldState}
   where
    updateWithCommit = \case
      st@PartyState{partyState = Initial{headParameters, commits, pendingCommits}} ->
        st
          { partyState = updatedState
          }
       where
        commits' = Map.insert party utxo commits
        pendingCommits' = party `Set.delete` pendingCommits
        updatedState =
          if null pendingCommits
            then
              Open
                { headParameters
                , offChainState =
                    OffChainState
                      { confirmedSnapshots = [mconcat (Map.elems commits')]
                      , seenTransactions = []
                      }
                }
            else
              Initial
                { headParameters
                , commits = commits'
                , pendingCommits = pendingCommits'
                }
      _ -> error "unexpected state"
  nextState WorldState{worldState} Abort{command = Input.Abort} _ =
    WorldState{worldState = Map.map updateWithAbort worldState}
   where
    updateWithAbort = \case
      st@PartyState{partyState = Initial{commits}} ->
        st
          { partyState = Final committedUTxO
          }
       where
        committedUTxO = mconcat $ Map.elems commits
      st@PartyState{} ->
        st
          { partyState = Final mempty
          }
  nextState _ _ _ = error "not implemented"

  precondition WorldState{worldState} Seed{} = null worldState
  precondition WorldState{worldState} Init{command = Input.Init{}} =
    not (null worldState) && all (isIdleState . partyState) (Map.elems worldState)
  precondition WorldState{worldState} Commit{party, command = Input.Commit{}} =
    not (null worldState) && all (isPendingCommitFrom party . partyState) (Map.elems worldState)
  precondition WorldState{worldState} Abort{command = Input.Abort{}} =
    not (null worldState) && all (isInitialState . partyState) (Map.elems worldState)
  precondition _ _ = True

  perform :: WorldState m -> Action (WorldState m) a -> LookUp -> StateT (Nodes m) m a
  perform _worldState Seed{seedKeys} _ = do
    let parties = map (deriveParty . fst) seedKeys
    tvar <- lift $ newTVarIO []
    nodes <- lift $ do
      let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
      connectToChain <- simulatedChainAndNetwork
      forM seedKeys $ \(sk, _csk) -> do
        outputs <- atomically newTQueue
        outputHistory <- newTVarIO []
        node <- createHydraNode ledger sk parties outputs outputHistory connectToChain
        let testNode = createTestHydraNode outputs outputHistory node connectToChain
        void $ async $ runHydraNode (traceInTVar tvar) node
        pure (deriveParty sk, testNode)

    put $ Map.fromList nodes
  perform _ Init{party, command} _ = party `performs` command
  perform _ Commit{party, command} _ = do
    nodes <- get
    case Map.lookup party nodes of
      Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
      Just actorNode -> do
        lift $ waitUntil [actorNode] $ ReadyToCommit (Set.fromList $ Map.keys nodes)
        party `performs` command
  perform _ Abort{party, command} _ = party `performs` command

  monitoring (s, s') _action _lookup _return =
    case (localState s, localState s') of
      (Just Initial{}, Just Open{}) -> label "Initial -> Open"
      _ -> identity
   where
    -- TODO: we should rework WorldState to only contain one state which
    -- represent the agreed upon state according to the consensus inside the
    -- head. Here we take the state of the 'first party' because it doesn't
    -- matter, they're all (supposed to be) equal.
    localState = fmap (partyState . head) . NE.nonEmpty . Map.elems . worldState

performs :: Monad m => Party -> ClientInput Tx -> StateT (Nodes m) m ()
performs party command = do
  nodes <- get
  case Map.lookup party nodes of
    Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
    Just actorNode -> lift $ actorNode `send` command

partyKeys :: Gen (Hydra.SigningKey, CardanoSigningKey)
partyKeys = do
  sk <- arbitrary
  csk <- genSigningKey
  pure (sk, csk)

deriving instance Show (Action (WorldState m) a)
deriving instance Eq (Action (WorldState m) a)
