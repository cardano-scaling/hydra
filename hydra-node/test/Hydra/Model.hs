{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A /Model/ of the Hydra head Protocol
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
module Hydra.Model where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import qualified Cardano.Api.UTxO as UTxO
import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.BehaviorSpec (TestHydraNode, createHydraNode, createTestHydraNode, send, simulatedChainAndNetwork, waitUntil)
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import qualified Hydra.Crypto as Hydra
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger.Cardano (cardanoLedger, genOneUTxOFor, genSigningKey, mkSimpleTx)
import Hydra.Logging (traceInTVar)
import Hydra.Node (runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (ReadyToCommit))
import Test.QuickCheck (elements, frequency, listOf1, resize, suchThat, tabulate)
import Test.QuickCheck.StateModel (Any (..), LookUp, StateModel (..), Var)
import qualified Prelude

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
    Command ::
      { party :: Party
      , command :: ClientInput Tx
      } ->
      Action (WorldState m) ()

  type ActionMonad (WorldState m) = StateT (Nodes m) m

  actionName :: Action (WorldState m) a -> String
  actionName Command{command} = unsafeConstructorName command
  actionName Seed{} = "Seed"

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
      Initial{pendingCommits} ->
        frequency
          [ (5, genCommit pendingCommits)
          , (1, genAbort)
          ]
      Open{offChainState} ->
        frequency
          [ (10, genNewTx offChainState)
          ]
      _ -> genSeed
   where
    genSeed = Some . Seed <$> resize 7 (listOf1 partyKeys)

    genInit = do
      initContestationPeriod <- arbitrary
      key <- fst <$> elements hydraParties
      let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
      pure $ Some Command{party = deriveParty key, command}

    genCommit pending = do
      party <- elements $ toList pending
      let (_, cardanoKey) = fromJust $ find ((== party) . deriveParty . fst) hydraParties
      utxo <- genOneUTxOFor (getVerificationKey cardanoKey)
      let command = Input.Commit{Input.utxo = utxo}
      pure $ Some Command{party = party, command}

    genAbort = do
      (key, _) <- elements hydraParties
      pure $ Some Command{party = deriveParty key, command = Input.Abort}

    genNewTx OffChainState{confirmedSnapshots} = do
      (hk, sk) <- elements hydraParties
      addr <- mkVkAddress testNetworkId . getVerificationKey . snd <$> elements hydraParties
      -- TODO: Make 'confirmedSnapshots' a 'NonEmpty' to avoid unsafe 'head'
      let mostRecentSnapshot = Prelude.head confirmedSnapshots
      (i, o) <- elements (UTxO.pairs mostRecentSnapshot) `suchThat` isOwned sk
      let command = case mkSimpleTx (i, o) (addr, txOutValue o) sk of
            Left e -> error (show e)
            Right tx -> Input.NewTx tx
      pure $ Some Command{party = deriveParty hk, command}

  precondition WorldState{hydraState = Start} Seed{} = True
  precondition WorldState{hydraState = Idle{}} Command{command = Input.Init{}} = True
  precondition WorldState{hydraState = hydraState@Initial{}} Command{party, command = Input.Commit{}} = isPendingCommitFrom party hydraState
  precondition WorldState{hydraState = Initial{}} Command{command = Input.Abort{}} = True
  precondition WorldState{hydraState = Open{}} Command{command = Input.NewTx{}} = True
  precondition _ _ = False

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState _ Seed{seedKeys} _ =
    WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys}}
   where
    idleParties = map (deriveParty . fst) seedKeys
    cardanoKeys = map (getVerificationKey . snd) seedKeys
  nextState WorldState{hydraParties, hydraState} Command{command = Input.Init{Input.contestationPeriod}} _ =
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
  nextState WorldState{hydraParties, hydraState} Command{party, command = Input.Commit{Input.utxo}} _ =
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
  nextState WorldState{hydraParties, hydraState} Command{command = Input.Abort} _ =
    WorldState{hydraParties, hydraState = updateWithAbort hydraState}
   where
    updateWithAbort = \case
      Initial{commits} -> Final committedUTxO
       where
        committedUTxO = mconcat $ Map.elems commits
      _ -> Final mempty
  nextState WorldState{hydraParties, hydraState} Command{command = Input.NewTx{Input.transaction = tx}} _ =
    WorldState{hydraParties, hydraState = updateWithNewTx hydraState}
   where
    updateWithNewTx = \case
      Open{confirmedSnapshots, seenTransactions} -> Open{confirmedSnapshots, seenTransactions = tx : seenTransactions}
      _ -> error "unexpected state"
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
  perform _ Command{party, command = commit@Input.Commit{}} _ = do
    nodes <- get
    case Map.lookup party nodes of
      Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
      Just actorNode -> do
        lift $ waitUntil [actorNode] $ ReadyToCommit (Set.fromList $ Map.keys nodes)
        party `performs` commit
  perform _ Command{party, command} _ = party `performs` command

  monitoring (s, s') _action _lookup _return =
    case (hydraState s, hydraState s') of
      (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

unsafeConstructorName :: (Show a) => a -> String
unsafeConstructorName = Prelude.head . Prelude.words . show

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

isOwned :: SigningKey PaymentKey -> (TxIn, TxOut ctx) -> Bool
isOwned sk (_, TxOut (ShelleyAddressInEra (ShelleyAddress _ cre _)) _ _) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False

deriving instance Show (Action (WorldState m) a)
deriving instance Eq (Action (WorldState m) a)
