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
import Hydra.Prelude hiding (Any, label, lookup)

import Cardano.Api.UTxO (pairs)
import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTQueueIO,
  labelTVarIO,
  modifyTVar,
  newTQueue,
  newTVarIO,
  readTVarIO,
 )
import Control.Monad.Class.MonadAsync (Async, async, cancel, link)
import Control.Monad.Class.MonadFork (labelThisThread)
import Data.List (nub)
import Data.List qualified as List
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GHC.Natural (wordToNatural)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ClientInput qualified as Input
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.BehaviorSpec (
  SimulatedChainNetwork (..),
  TestHydraClient (..),
  createHydraNode,
  createTestHydraClient,
  shortLabel,
  waitMatch,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (HeadParameters (..), maximumNumberOfParties)
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Committed (),
  IdleState (..),
 )
import Hydra.HeadLogic qualified as HeadState
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genSigningKey, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Model.MockChain (mkMockTxIn, mockChainAndNetwork)
import Hydra.Model.Payment (CardanoSigningKey (..), Payment (..), applyTx, genAdaValue)
import Hydra.Node (createNodeState, runHydraNode)
import Hydra.Party (Party (..), deriveParty)
import Hydra.Snapshot qualified as Snapshot
import Test.QuickCheck (choose, counterexample, elements, frequency, resize, sized, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), HasVariables, Realized, RunModel (..), StateModel (..), Var, VarContext)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))
import Prelude qualified

-- * The Model

-- | State maintained by the model.
data WorldState = WorldState
  { hydraParties :: [(SigningKey HydraKey, CardanoSigningKey)]
  -- ^ List of parties identified by both signing keys required to run protocol.
  -- This list must not contain any duplicated key.
  , hydraState :: GlobalState
  -- ^ Expected consensus state
  -- All nodes should be in the same state.
  }
  deriving stock (Eq, Show)

-- | Global state of the Head protocol.
-- While each participant in the Hydra Head protocol has its own private
-- view of the state, we model the expected global state whose properties
-- stem from the consensus built into the Head protocol. In other words, this
-- state is what each node's local state should be /eventually/.
data GlobalState
  = -- | Start of the "world".
    --  This state is left implicit in the node's logic as it
    --  represents that state where the node does not even
    --  exist.
    Start
  | Idle
      { idleParties :: [Party]
      , cardanoKeys :: [VerificationKey PaymentKey]
      , idleContestationPeriod :: ContestationPeriod
      , toCommit :: Uncommitted
      }
  | Initial
      { headParameters :: HeadParameters
      , commits :: Committed Payment
      , pendingCommits :: Uncommitted
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
  party `Map.member` pendingCommits
isPendingCommitFrom _ _ = False

type Uncommitted = Map.Map Party (UTxOType Payment)

data OffChainState = OffChainState {confirmedUTxO :: UTxOType Payment}
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
    Seed ::
      { seedKeys :: [(SigningKey HydraKey, CardanoSigningKey)]
      , seedContestationPeriod :: ContestationPeriod
      , toCommit :: Uncommitted
      } ->
      Action WorldState ()
    -- NOTE: No records possible here as we would duplicate 'Party' fields with
    -- different return values.
    Init :: Party -> Action WorldState ()
    Commit :: Party -> UTxOType Payment -> Action WorldState ActualCommitted
    Abort :: Party -> Action WorldState ()
    NewTx :: Party -> Payment -> Action WorldState Payment
    Wait :: DiffTime -> Action WorldState ()
    ObserveConfirmedTx :: Var Payment -> Action WorldState ()
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
      Idle{} -> Some <$> genInit hydraParties
      Initial{pendingCommits} ->
        frequency
          [ (5, genCommit pendingCommits)
          , (1, genAbort)
          ]
      Open{} -> do
        genNewTx
      _ -> fmap Some genSeed
   where
    genCommit :: Uncommitted -> Gen (Any (Action WorldState))
    genCommit pending = do
      (party, commits) <- elements $ Map.toList pending
      pure . Some $ Commit party commits

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

  nextState s@WorldState{hydraParties, hydraState} a _var =
    case a of
      Seed{seedKeys, seedContestationPeriod, toCommit} ->
        WorldState{hydraParties = seedKeys, hydraState = idleState}
       where
        idleState = Idle{idleParties, cardanoKeys, idleContestationPeriod, toCommit}
        idleParties = map (deriveParty . fst) seedKeys
        cardanoKeys = map (getVerificationKey . signingKey . snd) seedKeys
        idleContestationPeriod = seedContestationPeriod
      --
      Init{} ->
        WorldState{hydraParties, hydraState = mkInitialState hydraState}
       where
        mkInitialState = \case
          Idle{idleParties, idleContestationPeriod, toCommit} ->
            Initial
              { headParameters =
                  HeadParameters
                    { parties = idleParties
                    , contestationPeriod = idleContestationPeriod
                    }
              , commits = mempty
              , pendingCommits = toCommit
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
            pendingCommits' = party `Map.delete` pendingCommits
            updatedState =
              if null pendingCommits'
                then
                  Open
                    { headParameters
                    , offChainState =
                        OffChainState
                          { confirmedUTxO = mconcat (Map.elems commits')
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
          Open{headParameters, offChainState = OffChainState{confirmedUTxO}} ->
            Open
              { headParameters
              , offChainState =
                  OffChainState
                    { confirmedUTxO = confirmedUTxO `applyTx` tx
                    }
              }
          _ -> error "unexpected state"
      Wait _ -> s
      ObserveConfirmedTx _ -> s
      ObserveHeadIsOpen -> s
      StopTheWorld -> s

instance HasVariables WorldState where
  getAllVariables _ = mempty

instance HasVariables (Action WorldState a) where
  getAllVariables = \case
    ObserveConfirmedTx tx -> Set.singleton (Some tx)
    _other -> mempty

deriving stock instance Show (Action WorldState a)
deriving stock instance Eq (Action WorldState a)

-- ** Generator Helper

genSeed :: Gen (Action WorldState ())
genSeed = do
  seedKeys <- resize maximumNumberOfParties partyKeys
  seedContestationPeriod <- genContestationPeriod
  toCommit <- mconcat <$> mapM genToCommit seedKeys
  pure $ Seed{seedKeys, seedContestationPeriod, toCommit}

genToCommit :: (SigningKey HydraKey, CardanoSigningKey) -> Gen (Map Party [(CardanoSigningKey, Value)])
genToCommit (hk, ck) = do
  value <- genAdaValue
  pure $ Map.singleton (deriveParty hk) [(ck, value)]

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

unsafeConstructorName :: Show a => a -> String
unsafeConstructorName = Prelude.head . Prelude.words . show

-- | Generate a list of pairs of Hydra/Cardano signing keys.
--  All the keys in this list are guaranteed to be unique.
partyKeys :: Gen [(SigningKey HydraKey, CardanoSigningKey)]
partyKeys =
  sized $ \len -> do
    numParties <- choose (1, len)
    hks <- nub <$> vectorOf numParties arbitrary
    cks <- nub . fmap CardanoSigningKey <$> vectorOf numParties genSigningKey
    pure $ zip hks cks

-- * Running the model

-- | Concrete state needed to run actions against the implementation.
-- This state is used and might be updated when actually `perform`ing actions generated from the `StateModel`.
data Nodes m = Nodes
  { nodes :: Map.Map Party (TestHydraClient Tx m)
  -- ^ Map from party identifiers to a /handle/ for interacting with a node.
  , logger :: Tracer m (HydraLog Tx ())
  -- ^ Logger used by each node.
  -- The reason we put this here is because the concrete value needs to be
  -- instantiated upon the test run initialisation, outiside of the model.
  , threads :: [Async m ()]
  -- ^ List of threads spawned when executing `RunMonad`
  , chain :: SimulatedChainNetwork Tx m
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

instance MonadSTM m => MonadState (Nodes m) (RunMonad m) where
  get = ask >>= lift . readTVarIO . nodesState

  put n = ask >>= lift . atomically . flip modifyTVar (const n) . nodesState

data RunException
  = TransactionNotObserved Payment UTxO
  | UnexpectedParty Party
  | UnknownAddress AddressInEra [(AddressInEra, CardanoSigningKey)]
  | CannotFindSpendableUTxO Payment UTxO
  deriving stock (Eq, Show)

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
  , MonadDelay m
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

  perform st action lookup = do
    case action of
      Seed{seedKeys, seedContestationPeriod, toCommit} ->
        seedWorld seedKeys seedContestationPeriod toCommit
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
      ObserveConfirmedTx var -> do
        let tx = lookup var
        nodes <- Map.toList <$> gets nodes
        forM_ nodes $ \(_, node) -> do
          lift (waitForUTxOToSpend mempty (to tx) (value tx) node) >>= \case
            Left u -> throwIO $ TransactionNotObserved tx u
            Right _ -> pure ()
      ObserveHeadIsOpen -> do
        nodes' <- Map.toList <$> gets nodes
        forM_ nodes' $ \(_, node) -> do
          outputs <- lift $ serverOutputs node
          case find headIsOpen outputs of
            Just _ -> pure ()
            Nothing -> error "The head is not open for node"
      StopTheWorld ->
        stopTheWorld
   where
    headIsOpen = \case
      HeadIsOpen{} -> True
      _otherwise -> False

-- ** Performing actions

seedWorld ::
  ( MonadAsync m
  , MonadTimer m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadFork m
  , MonadMask m
  , MonadDelay m
  ) =>
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  ContestationPeriod ->
  Uncommitted ->
  RunMonad m ()
seedWorld seedKeys seedCP futureCommits = do
  tr <- gets logger

  mockChain@SimulatedChainNetwork{tickThread} <-
    lift $
      mockChainAndNetwork (contramap DirectChain tr) seedKeys (foldMap toRealUTxO $ Map.elems futureCommits)
  pushThread tickThread

  clients <- forM seedKeys $ \(hsk, _csk) -> do
    let party = deriveParty hsk
        otherParties = filter (/= party) parties
    (testClient, nodeThread) <- lift $ do
      outputs <- atomically newTQueue
      labelTQueueIO outputs ("outputs-" <> shortLabel hsk)
      outputHistory <- newTVarIO []
      labelTVarIO outputHistory ("history-" <> shortLabel hsk)
      nodeState <- createNodeState $ HeadState.Idle IdleState{chainState = initialChainState}
      node <- createHydraNode ledger nodeState hsk otherParties outputs outputHistory mockChain seedCP
      let testClient = createTestHydraClient outputs outputHistory node
      nodeThread <- async $ labelThisThread ("node-" <> shortLabel hsk) >> runHydraNode (contramap Node tr) node
      link nodeThread
      pure (testClient, nodeThread)
    pushThread nodeThread
    pure (party, testClient)

  modify $ \n ->
    n{nodes = Map.fromList clients, chain = mockChain}
 where
  parties = map (deriveParty . fst) seedKeys

  ledger = cardanoLedger defaultGlobals defaultLedgerEnv

  pushThread t = modify $ \s ->
    s{threads = t : threads s}

performCommit ::
  (MonadThrow m, MonadTimer m) =>
  [CardanoSigningKey] ->
  Party ->
  [(CardanoSigningKey, Value)] ->
  RunMonad m ActualCommitted
performCommit parties party paymentUTxO = do
  nodes <- gets nodes
  SimulatedChainNetwork{simulateCommit} <- gets chain
  case Map.lookup party nodes of
    Nothing -> throwIO $ UnexpectedParty party
    Just actorNode -> do
      let realUTxO = toRealUTxO paymentUTxO
      lift $ simulateCommit (party, realUTxO)
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

toRealUTxO :: [(CardanoSigningKey, Value)] -> UTxO
toRealUTxO paymentUTxO =
  UTxO.fromPairs $
    [ (mkMockTxIn vk ix, txOut)
    | (ix, (CardanoSigningKey sk, val)) <- zip [0 ..] paymentUTxO
    , let vk = getVerificationKey sk
    , let txOut = TxOut (mkVkAddress testNetworkId vk) val TxOutDatumNone ReferenceScriptNone
    ]

performNewTx ::
  (MonadThrow m, MonadAsync m, MonadTimer m, MonadDelay m) =>
  Party ->
  Payment ->
  RunMonad m Payment
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
  lift $ do
    waitUntilMatch [thisNode] $ \case
      SnapshotConfirmed{snapshot = snapshot} ->
        txId realTx `elem` Snapshot.confirmed snapshot
      err@TxInvalid{} -> error ("expected tx to be valid: " <> show err)
      _ -> False
    pure tx

-- | Wait for the head to be open by searching from the beginning. Note that
-- there rollbacks or multiple life-cycles of heads are not handled here.
waitForOpen :: MonadDelay m => TestHydraClient tx m -> RunMonad m ()
waitForOpen node = do
  outs <- lift $ serverOutputs node
  unless
    (any headIsOpen outs)
    waitAndRetry
 where
  headIsOpen = \case
    HeadIsOpen{} -> True
    _ -> False

  waitAndRetry = lift (threadDelay 0.1) >> waitForOpen node

sendsInput :: (MonadSTM m, MonadThrow m) => Party -> ClientInput Tx -> RunMonad m ()
sendsInput party command = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> throwIO $ UnexpectedParty party
    Just actorNode -> lift $ actorNode `send` command

performInit :: (MonadThrow m, MonadAsync m, MonadTimer m) => Party -> RunMonad m ()
performInit party = do
  party `sendsInput` Input.Init

  nodes <- gets nodes
  lift $
    waitUntilMatch (toList nodes) $ \case
      HeadIsInitializing{} -> True
      err@CommandFailed{} -> error $ show err
      _ -> False

performAbort :: (MonadThrow m, MonadAsync m, MonadTimer m) => Party -> RunMonad m ()
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
  gets threads >>= mapM_ (lift . cancel)

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
  (MonadTimer m, MonadDelay m) =>
  UTxO ->
  CardanoSigningKey ->
  Value ->
  TestHydraClient Tx m ->
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
