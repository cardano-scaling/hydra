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
  = Start
  | Idle
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

-- | Global state maintained by the model.
data WorldState (m :: Type -> Type) = WorldState
  { hydraParties :: [(Hydra.SigningKey, CardanoSigningKey)]
  , hydraState :: LocalState
  }
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

  initialState =
    WorldState
      { hydraParties = mempty
      , hydraState = Start
      }

  arbitraryAction :: WorldState m -> Gen (Any (Action (WorldState m)))
  arbitraryAction WorldState{hydraParties, hydraState} =
    case hydraState of
      Start -> genSeed
      Idle{} -> genInit
      Initial{} ->
        oneof
          [ genCommit
          , genAbort
          ]
      _ -> genSeed
   where
    genSeed = Some . Seed <$> resize 10 (listOf1 partyKeys)

    genInit = do
      initContestationPeriod <- arbitrary
      key <- fst <$> elements hydraParties
      let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
      pure $ Some Init{party = deriveParty key, command}

    genCommit = do
      (key, cardanoKey) <- elements hydraParties
      utxo <- genOneUTxOFor (getVerificationKey cardanoKey)
      let command = Input.Commit{Input.utxo = utxo}
      pure $ Some Commit{party = deriveParty key, command}

    genAbort = do
      (key, _) <- elements hydraParties
      pure $ Some Abort{party = deriveParty key, command = Input.Abort}

  precondition WorldState{hydraState = Start} Seed{} = True
  precondition WorldState{hydraState = Idle{}} Init{command = Input.Init{}} = True
  precondition WorldState{hydraState = hydraState@Initial{}} Commit{party, command = Input.Commit{}} = isPendingCommitFrom party hydraState
  precondition WorldState{hydraState = Initial{}} Abort{command = Input.Abort{}} = True
  precondition _ _ = True

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState _ Seed{seedKeys} _ =
    WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys}}
   where
    idleParties = map (deriveParty . fst) seedKeys
    cardanoKeys = map (getVerificationKey . snd) seedKeys
  nextState WorldState{hydraParties, hydraState} Init{command = Input.Init{Input.contestationPeriod}} _ =
    WorldState{hydraParties, hydraState = mkInitialState hydraState}
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
  nextState WorldState{hydraParties, hydraState} Commit{party, command = Input.Commit{Input.utxo}} _ =
    WorldState{hydraParties, hydraState = updateWithCommit hydraState}
   where
    updateWithCommit = \case
      Initial{headParameters, commits, pendingCommits} -> updatedState
       where
        commits' = Map.insert party utxo commits
        pendingCommits' = party `Set.delete` pendingCommits
        updatedState =
          if null pendingCommits'
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
  nextState WorldState{hydraParties, hydraState} Abort{command = Input.Abort} _ =
    WorldState{hydraParties, hydraState = updateWithAbort hydraState}
   where
    updateWithAbort = \case
      Initial{commits} -> Final committedUTxO
       where
        committedUTxO = mconcat $ Map.elems commits
      _ -> Final mempty
  nextState _ _ _ = error "not implemented"

  perform :: WorldState m -> Action (WorldState m) a -> LookUp -> StateT (Nodes m) m a
  perform _ Seed{seedKeys} _ = do
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
    case (hydraState s, hydraState s') of
      (Start{}, Idle{}) -> label "Start -> Idle"
      (Idle{}, Initial{}) -> label "Idle -> Initial"
      (Initial{}, Initial{}) -> label "Initial -> Initial"
      (Initial{}, Open{}) -> label "Initial -> Open"
      _ -> identity

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
