{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A /Model/ of the Hydra head Protocol.
--
-- This model integrates in a single state-machine like abstraction the whole behaviour of
-- a Hydra Head, taking into account both on-chain state and contracts, and off-chain
-- interactions. It is written from the point of view of a pre-defined set of Hydra node
-- /operators/ that want to create a channel between them.
-- It's a "happy path" model that does not implement any kind of adversarial behaviour and
-- whose transactions are very simple: Each tx is a payment of one Ada-only UTxO transferred
-- to another party in full, without any change.
--
-- More intricate and specialised models shall be developed once we get a firmer grasp of
-- the whole framework, injecting faults, taking into account more parts of the stack,
-- modelling more complex transactions schemes...
module Hydra.Model where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (pairs)
import qualified Cardano.Api.UTxO as UTxO
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTVarIO,
  modifyTVar,
  newTQueue,
  newTVarIO,
  readTVarIO,
 )
import Control.Monad.Class.MonadAsync (Async, async, cancel, link)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadTimer (timeout)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import GHC.Natural (wordToNatural)
import Hydra.API.ClientInput (ClientInput)
import qualified Hydra.API.ClientInput as Input
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.BehaviorSpec (
  TestHydraNode (..),
  createHydraNode,
  createTestHydraNode,
  shortLabel,
  waitMatch,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Committed (),
  PendingCommits,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genSigningKey, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Model.MockChain (mkMockTxIn, mockChainAndNetwork)
import Hydra.Model.Payment (CardanoSigningKey (..), Payment (..), applyTx, genAdaValue)
import Hydra.Node (
  NodeState (NodeState),
  modifyHeadState,
  queryHeadState,
  runHydraNode,
 )
import Hydra.Options (maximumNumberOfParties)
import Hydra.Party (Party (..), deriveParty)
import qualified Hydra.Snapshot as Snapshot
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (choose, counterexample, elements, frequency, resize, sized, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), HasVariables, Realized, RunModel (..), StateModel (..), VarContext)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))
import qualified Prelude

-- * The Model

-- | State maintained by the model.
data WorldState = WorldState
  { -- | List of parties identified by both signing keys required to run protocol.
    -- This list must not contain any duplicated key.
    hydraParties :: [(SigningKey HydraKey, CardanoSigningKey)]
  , -- | Expected consensus state
    -- All nodes should be in the same state.
    hydraState :: GlobalState
  }
  deriving (Eq, Show)

-- | Global state of the Head protocol.
-- While each participant in the Hydra Head protocol has its own private
-- view of the state, we model the expected global state whose properties
-- stem from the consensus built into the Head protocol. In other words, this
-- state is what each node's local state should be /eventually/.
data GlobalState
  = -- |Start of the "world".
    -- This state is left implicit in the node's logic as it
    -- represents that state where the node does not even
    -- exist.
    Start
  | Idle
      { idleParties :: [Party]
      , cardanoKeys :: [VerificationKey PaymentKey]
      , idleContestationPeriod :: ContestationPeriod
      }
  | Initial
      { headParameters :: HeadParameters
      , commits :: Committed Payment
      , pendingCommits :: PendingCommits
      }
  | Open
      { headParameters :: HeadParameters
      , offChainState :: OffChainState
      }
  | Final {finalUTxO :: UTxOType Payment}
  deriving stock (Eq, Show)

isInitialState :: GlobalState -> Bool
isInitialState Initial{} = True
isInitialState _ = False

isOpenState :: GlobalState -> Bool
isOpenState Open{} = True
isOpenState _ = False

isFinalState :: GlobalState -> Bool
isFinalState Final{} = True
isFinalState _ = False

isIdleState :: GlobalState -> Bool
isIdleState Idle{} = True
isIdleState _ = False

isPendingCommitFrom :: Party -> GlobalState -> Bool
isPendingCommitFrom party Initial{pendingCommits} =
  party `Set.member` pendingCommits
isPendingCommitFrom _ _ = False

data OffChainState = OffChainState
  { confirmedUTxO :: UTxOType Payment
  , seenTransactions :: [Payment]
  }
  deriving stock (Eq, Show)

-- This is needed to be able to use `WorldState` inside DL formulae
instance DynLogicModel WorldState

type ActualCommitted = UTxOType Payment

-- | Basic instantiation of `StateModel` for our `WorldState` state.
instance StateModel WorldState where
  -- The list of possible "Actions" within our `Model`
  -- Not all of them need to actually represent an actual user `Action`, but they
  -- can represent _observations_ which are useful when defining properties in
  -- DL. Those observations would usually not be generated.
  data Action WorldState a where
    Seed :: {seedKeys :: [(SigningKey HydraKey, CardanoSigningKey)], seedContestationPeriod :: ContestationPeriod} -> Action WorldState ()
    -- NOTE: No records possible here as we would duplicate 'Party' fields with
    -- different return values.
    Init :: Party -> Action WorldState ()
    Commit :: Party -> UTxOType Payment -> Action WorldState ActualCommitted
    Abort :: Party -> Action WorldState ()
    NewTx :: Party -> Payment -> Action WorldState ()
    Wait :: DiffTime -> Action WorldState ()
    ObserveConfirmedTx :: Payment -> Action WorldState ()
    -- Check that all parties have observed the head as open
    ObserveHeadIsOpen :: Action WorldState ()
    StopTheWorld :: Action WorldState ()

  initialState =
    WorldState
      { hydraParties = mempty
      , hydraState = Start
      }

  arbitraryAction :: VarContext -> WorldState -> Gen (Any (Action WorldState))
  arbitraryAction _ st@WorldState{hydraParties, hydraState} =
    case hydraState of
      Start -> fmap Some genSeed
      Idle{} -> fmap Some $ genInit hydraParties
      Initial{pendingCommits} ->
        frequency
          [ (5, genCommit pendingCommits)
          , (1, genAbort)
          ]
      Open{} -> do
        -- FIXME: Generation of arbitrary NewTx disabled as we don't control
        -- rollbacks in the MockChain and the hydra-node purges L2 state when
        -- rolling back "past open".
        void genNewTx
        pure $ error "NewTx disabled because of rollbacks past open"
      _ -> fmap Some genSeed
   where
    genCommit pending = do
      party <- elements $ toList pending
      let (_, sk) = fromJust $ find ((== party) . deriveParty . fst) hydraParties
      value <- genAdaValue
      let utxo = [(sk, value)]
      pure . Some $ Commit party utxo

    genAbort = do
      (key, _) <- elements hydraParties
      let party = deriveParty key
      pure . Some $ Abort party

    genNewTx = genPayment st >>= \(party, transaction) -> pure . Some $ NewTx party transaction

  precondition WorldState{hydraState = Start} Seed{} =
    True
  precondition WorldState{hydraState = Idle{}} Init{} =
    True
  precondition WorldState{hydraState = hydraState@Initial{}} (Commit party _) =
    isPendingCommitFrom party hydraState
  precondition WorldState{hydraState = Initial{}} Abort{} =
    True
  precondition WorldState{hydraState = Open{offChainState}} (NewTx _ tx) =
    (from tx, value tx) `List.elem` confirmedUTxO offChainState
  precondition _ Wait{} =
    True
  precondition WorldState{hydraState = Open{}} ObserveConfirmedTx{} =
    True
  precondition WorldState{hydraState = Open{}} ObserveHeadIsOpen =
    True
  precondition _ StopTheWorld =
    True
  precondition _ _ =
    False

  nextState s@WorldState{hydraParties, hydraState} a _ =
    case a of
      Seed{seedKeys, seedContestationPeriod} -> WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys, idleContestationPeriod}}
       where
        idleParties = map (deriveParty . fst) seedKeys
        cardanoKeys = map (getVerificationKey . signingKey . snd) seedKeys
        idleContestationPeriod = seedContestationPeriod
      --
      Init{} ->
        WorldState{hydraParties, hydraState = mkInitialState hydraState}
       where
        mkInitialState = \case
          Idle{idleParties, idleContestationPeriod} ->
            Initial
              { headParameters =
                  HeadParameters
                    { parties = idleParties
                    , contestationPeriod = idleContestationPeriod
                    }
              , commits = mempty
              , pendingCommits = Set.fromList idleParties
              }
          _ -> error "unexpected state"
      --
      Commit party utxo ->
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
                          { confirmedUTxO = mconcat (Map.elems commits')
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
      --
      Abort{} ->
        WorldState{hydraParties, hydraState = updateWithAbort hydraState}
       where
        updateWithAbort = \case
          Initial{commits} -> Final committedUTxO
           where
            committedUTxO = mconcat $ Map.elems commits
          _ -> Final mempty
      --
      (NewTx _ tx) ->
        WorldState{hydraParties, hydraState = updateWithNewTx hydraState}
       where
        updateWithNewTx = \case
          Open{headParameters, offChainState = OffChainState{confirmedUTxO, seenTransactions}} ->
            Open
              { headParameters
              , offChainState =
                  OffChainState
                    { confirmedUTxO = confirmedUTxO `applyTx` tx
                    , seenTransactions = tx : seenTransactions
                    }
              }
          _ -> error "unexpected state"
      Wait _ -> s
      ObserveConfirmedTx _ -> s
      ObserveHeadIsOpen -> s
      StopTheWorld -> s

instance HasVariables WorldState where
  getAllVariables _ = mempty

-- XXX: This is a bit annoying and non-obviously needed. It's coming from the
-- default implementation of HasVariables on the associated Action type. The
-- default implementation required 'Generic', which is not available if we use
-- GADTs for actions (as in the examples).
instance HasVariables (Action WorldState a) where
  getAllVariables _ = mempty

deriving instance Show (Action WorldState a)
deriving instance Eq (Action WorldState a)

-- ** Generator Helper

genSeed :: Gen (Action WorldState ())
genSeed = Seed <$> resize maximumNumberOfParties partyKeys <*> genContestationPeriod

genContestationPeriod :: Gen ContestationPeriod
genContestationPeriod = do
  n <- choose (1, 200)
  pure $ UnsafeContestationPeriod $ wordToNatural n

genInit :: [(SigningKey HydraKey, b)] -> Gen (Action WorldState ())
genInit hydraParties = do
  key <- fst <$> elements hydraParties
  let party = deriveParty key
  pure $ Init party

genCommit' ::
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  (SigningKey HydraKey, CardanoSigningKey) ->
  Gen (Action WorldState [(CardanoSigningKey, Value)])
genCommit' hydraParties hydraParty = do
  let (_, sk) = fromJust $ find (== hydraParty) hydraParties
  value <- genAdaValue
  let utxo = [(sk, value)]
  pure $ Commit (deriveParty . fst $ hydraParty) utxo

genPayment :: WorldState -> Gen (Party, Payment)
genPayment WorldState{hydraParties, hydraState} =
  case hydraState of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      (from, value) <-
        elements (filter (not . null . valueToList . snd) confirmedUTxO)
      let party = deriveParty $ fst $ fromJust $ List.find ((== from) . snd) hydraParties
      -- NOTE: It's perfectly possible this yields a payment to self and it
      -- assumes hydraParties is not empty else `elements` will crash
      (_, to) <- elements hydraParties
      pure (party, Payment{from, to, value})
    _ -> error $ "genPayment impossible in state: " <> show hydraState

unsafeConstructorName :: (Show a) => a -> String
unsafeConstructorName = Prelude.head . Prelude.words . show

-- |Generate a list of pairs of Hydra/Cardano signing keys.
-- All the keys in this list are guaranteed to be unique.
partyKeys :: Gen [(SigningKey HydraKey, CardanoSigningKey)]
partyKeys =
  sized $ \len -> do
    hks <- nub <$> vectorOf len arbitrary
    cks <- nub . fmap CardanoSigningKey <$> vectorOf len genSigningKey
    pure $ zip hks cks

-- * Running the model

-- | Concrete state needed to run actions against the implementation.
-- This state is used and might be updated when actually `perform`ing actions generated from the `StateModel`.
data Nodes m = Nodes
  { -- | Map from party identifiers to a /handle/ for interacting with a node.
    nodes :: Map.Map Party (TestHydraNode Tx m)
  , -- | Logger used by each node.
    -- The reason we put this here is because the concrete value needs to be
    -- instantiated upon the test run initialisation, outiside of the model.
    logger :: Tracer m (HydraLog Tx ())
  , -- | List of threads spawned when executing `RunMonad`
    -- FIXME: Remove it? It's not actually useful when running inside io-sim as
    -- there's no risk of leaking threads anyway, but perhaps it's good hygiene
    -- to keep it?
    threads :: [Async m ()]
  }

-- NOTE: This newtype is needed to allow its use in typeclass instances
newtype RunState m = RunState {nodesState :: TVar m (Nodes m)}

-- | Our execution `MonadTrans`former.
--
-- This type is needed in order to keep the execution monad `m` abstract  and thus
-- simplify the definition of the `RunModel` instance which requires a proper definition
-- of `Realized`  type family. See [this issue](https://github.com/input-output-hk/quickcheck-dynamic/issues/29)
-- for a discussion on why this monad is needed.
--
-- We could perhaps getaway with it and just have a type based on `IOSim` monad
-- but this is cumbersome to write.
newtype RunMonad m a = RunMonad {runMonad :: ReaderT (RunState m) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (RunState m), MonadThrow)

instance MonadTrans RunMonad where
  lift = RunMonad . lift

instance (MonadSTM m) => MonadState (Nodes m) (RunMonad m) where
  get = ask >>= lift . readTVarIO . nodesState

  put n = ask >>= lift . atomically . flip modifyTVar (const n) . nodesState

data RunException
  = TransactionNotObserved Payment UTxO
  | UnexpectedParty Party
  | UnknownAddress AddressInEra [(AddressInEra, CardanoSigningKey)]
  | CannotFindSpendableUTxO Payment UTxO
  deriving (Eq, Show)

instance Exception RunException

-- | This type family is needed to link the _actual_ output from runnign actions
-- with the ones that are modelled.
--
-- In our case we can keep things simple and use the same types on both side of
-- the fence.
type instance Realized (RunMonad m) a = a

instance
  ( MonadAsync m
  , MonadFork m
  , MonadMask m
  , MonadTimer m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  ) =>
  RunModel WorldState (RunMonad m)
  where
  postcondition (_, st) action _l result = pure $ checkOutcome st action result

  monitoring (s, s') action _l result =
    decoratePostconditionFailure
      . decorateTransitions
   where
    -- REVIEW: This should be part of the quickcheck-dynamic runActions
    decoratePostconditionFailure
      | checkOutcome s action result = id
      | otherwise =
        counterexample "postcondition failed"
          . counterexample ("Action: " <> show action)
          . counterexample ("State: " <> show s)
          . counterexample ("Result: " <> showFromAction (show result) action)

    decorateTransitions =
      case (hydraState s, hydraState s') of
        (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

  perform st action _ = do
    case action of
      Seed{seedKeys, seedContestationPeriod} ->
        seedWorld seedKeys seedContestationPeriod
      Commit party utxo ->
        performCommit (snd <$> hydraParties st) party utxo
      NewTx party transaction ->
        performNewTx party transaction
      Init party ->
        performInit party
      Abort party -> do
        performAbort party
      Wait delay ->
        lift $ threadDelay delay
      ObserveConfirmedTx tx -> do
        nodes <- Map.toList <$> gets nodes
        forM_ nodes $ \(_, node) -> do
          lift (waitForUTxOToSpend mempty (to tx) (value tx) node) >>= \case
            Left u -> error $ "Did not observe transaction " <> show tx <> " applied: " <> show u
            Right _ -> pure ()
      ObserveHeadIsOpen -> do
        nodes' <- Map.toList <$> gets nodes
        forM_ nodes' $ \(_, node) -> do
          outputs <- lift $ serverOutputs node
          case find headIsOpen outputs of
            Just _ -> pure ()
            Nothing -> error $ "The head is not open for node " -- <> show node
      StopTheWorld ->
        stopTheWorld
   where
    headIsOpen = \case
      HeadIsOpen{} -> True
      _otherwise -> False

-- ** Performing actions

seedWorld ::
  ( MonadDelay m
  , MonadAsync m
  , MonadTimer m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadFork m
  , MonadMask m
  ) =>
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  ContestationPeriod ->
  RunMonad m ()
seedWorld seedKeys seedCP = do
  let parties = map (deriveParty . fst) seedKeys
      dummyNodeState =
        NodeState
          { modifyHeadState = error "undefined"
          , queryHeadState = error "undefined"
          }
  tr <- gets logger
  nodes <- lift $ do
    let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
    nodes <- newTVarIO []
    labelTVarIO nodes "nodes"
    (connectToChain, tickThread) <-
      mockChainAndNetwork (contramap DirectChain tr) seedKeys nodes seedCP
    res <- forM seedKeys $ \(hsk, _csk) -> do
      outputs <- atomically newTQueue
      outputHistory <- newTVarIO []
      labelTVarIO nodes ("outputs-" <> shortLabel hsk)
      labelTVarIO nodes ("history-" <> shortLabel hsk)
      let party = deriveParty hsk
          otherParties = filter (/= party) parties
      node <- createHydraNode ledger dummyNodeState hsk otherParties outputs outputHistory connectToChain seedCP
      let testNode = createTestHydraNode outputs outputHistory node
      nodeThread <- async $ labelThisThread ("node-" <> shortLabel hsk) >> runHydraNode (contramap Node tr) node
      link nodeThread
      pure (party, testNode, nodeThread)
    pure (res, tickThread)

  modify $ \n ->
    n
      { nodes = Map.fromList $ (\(p, t, _) -> (p, t)) <$> fst nodes
      , threads = snd nodes : ((\(_, _, t) -> t) <$> fst nodes)
      }

performCommit ::
  (MonadThrow m, MonadTimer m) =>
  [CardanoSigningKey] ->
  Party ->
  [(CardanoSigningKey, Value)] ->
  RunMonad m ActualCommitted
performCommit parties party paymentUTxO = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
    Just actorNode -> do
      let realUTxO =
            UTxO.fromPairs $
              [ (mkMockTxIn vk ix, txOut)
              | (ix, (CardanoSigningKey sk, val)) <- zip [0 ..] paymentUTxO
              , let vk = getVerificationKey sk
              , let txOut = TxOut (mkVkAddress testNetworkId vk) val TxOutDatumNone ReferenceScriptNone
              ]
      party `sendsInput` Input.Commit{Input.utxo = realUTxO}
      observedUTxO <-
        lift $
          waitMatch actorNode $ \case
            Committed{party = cp, utxo = committedUTxO}
              | cp == party -> Just committedUTxO
            err@CommandFailed{} -> error $ show err
            _ -> Nothing

      pure $ fromUtxo observedUTxO
 where
  fromUtxo :: UTxO -> [(CardanoSigningKey, Value)]
  fromUtxo utxo = findSigningKey . (txOutAddress &&& txOutValue) . snd <$> pairs utxo

  knownAddresses :: [(AddressInEra, CardanoSigningKey)]
  knownAddresses = zip (makeAddressFromSigningKey <$> parties) parties

  findSigningKey :: (AddressInEra, Value) -> (CardanoSigningKey, Value)
  findSigningKey (addr, value) =
    case List.lookup addr knownAddresses of
      Nothing -> error $ "cannot invert address:  " <> show addr
      Just sk -> (sk, value)

  makeAddressFromSigningKey :: CardanoSigningKey -> AddressInEra
  makeAddressFromSigningKey = mkVkAddress testNetworkId . getVerificationKey . signingKey

performNewTx ::
  (MonadThrow m, MonadAsync m, MonadTimer m) =>
  Party ->
  Payment ->
  RunMonad m ()
performNewTx party tx = do
  let recipient = mkVkAddress testNetworkId . getVerificationKey . signingKey $ to tx
  nodes <- gets nodes
  let thisNode = nodes ! party

  waitForOpen thisNode

  (i, o) <-
    lift (waitForUTxOToSpend mempty (from tx) (value tx) thisNode) >>= \case
      Left u -> error $ "Cannot execute NewTx for " <> show tx <> ", no spendable UTxO in " <> show u
      Right ok -> pure ok

  let realTx =
        either
          (error . show)
          id
          (mkSimpleTx (i, o) (recipient, value tx) (signingKey $ from tx))

  party `sendsInput` Input.NewTx realTx
  lift $
    waitUntilMatch [thisNode] $ \case
      SnapshotConfirmed{snapshot = snapshot} ->
        realTx `elem` Snapshot.confirmed snapshot
      RolledBack{} -> error ("rolled back while in open state!")
      err@TxInvalid{} -> error ("expected tx to be valid: " <> show err)
      _ -> False

-- | Wait for the head to be open. Search from the beginning of history and make
-- sure there is no RolledBack after the last HeadIsOpen. Wait and retry forever
-- otherwise.
waitForOpen :: MonadDelay m => TestHydraNode tx m -> RunMonad m ()
waitForOpen node = do
  outs <- lift $ serverOutputs node
  if isOpen False outs
    then pure ()
    else waitAndRetry
 where
  isOpen status [] = status
  isOpen _ (HeadIsOpen{} : rest) = isOpen True rest
  isOpen _ (RolledBack{} : rest) = isOpen False rest
  isOpen status (_ : rest) = isOpen status rest

  waitAndRetry = lift (threadDelay 0.1) >> waitForOpen node

sendsInput :: (MonadSTM m, MonadThrow m) => Party -> ClientInput Tx -> RunMonad m ()
sendsInput party command = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> throwIO $ UnexpectedParty party
    Just actorNode -> lift $ actorNode `send` command

performInit :: (MonadDelay m, MonadThrow m, MonadAsync m, MonadTimer m) => Party -> RunMonad m ()
performInit party = do
  party `sendsInput` Input.Init

  nodes <- gets nodes
  lift $
    waitUntilMatch (toList nodes) $ \case
      HeadIsInitializing{} -> True
      err@CommandFailed{} -> error $ show err
      _ -> False

performAbort :: (MonadDelay m, MonadThrow m, MonadAsync m, MonadTimer m) => Party -> RunMonad m ()
performAbort party = do
  party `sendsInput` Input.Abort

  nodes <- gets nodes
  lift $
    waitUntilMatch (toList nodes) $ \case
      HeadIsAborted{} -> True
      err@CommandFailed{} -> error $ show err
      _ -> False

stopTheWorld :: MonadAsync m => RunMonad m ()
stopTheWorld =
  gets threads >>= mapM_ (void . lift . cancel)

-- ** Utility functions

-- | Bring `Show` instance in scope drawing it from the `Action` type.
--
-- This is a neat trick to provide `show`able results from action in a context where
-- there's no explicit `Show a` instance, eg. in the `monitoring` and `postcondition`
-- functions. We don't have access to an `a` directly because its value depends on
-- type family `Realized`.
showFromAction :: (Show a => b) -> Action WorldState a -> b
showFromAction k = \case
  Seed{} -> k
  Init{} -> k
  Commit{} -> k
  Abort{} -> k
  NewTx{} -> k
  Wait{} -> k
  ObserveConfirmedTx{} -> k
  StopTheWorld -> k
  ObserveHeadIsOpen -> k

checkOutcome :: WorldState -> Action WorldState a -> a -> Bool
checkOutcome _st (Commit _party expectedCommitted) actualCommitted =
  expectedCommitted == actualCommitted
checkOutcome _ _ _ = True

waitForUTxOToSpend ::
  forall m.
  (MonadDelay m, MonadTimer m) =>
  UTxO ->
  CardanoSigningKey ->
  Value ->
  TestHydraNode Tx m ->
  m (Either UTxO (TxIn, TxOut CtxUTxO))
waitForUTxOToSpend utxo key value node = go 100
 where
  go :: Int -> m (Either UTxO (TxIn, TxOut CtxUTxO))
  go = \case
    0 ->
      pure $ Left utxo
    n -> do
      node `send` Input.GetUTxO
      threadDelay 5
      timeout 10 (waitForNext node) >>= \case
        Just (GetUTxOResponse _ u)
          | u /= mempty ->
            maybe
              (go (n - 1))
              (pure . Right)
              (find matchPayment (UTxO.pairs u))
        _ -> go (n - 1)

  matchPayment p@(_, txOut) =
    isOwned key p && value == txOutValue txOut

isOwned :: CardanoSigningKey -> (TxIn, TxOut ctx) -> Bool
isOwned (CardanoSigningKey sk) (_, TxOut{txOutAddress = ShelleyAddressInEra (ShelleyAddress _ cre _)}) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False
