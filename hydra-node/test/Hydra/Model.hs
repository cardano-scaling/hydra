{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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

import Data.Foldable qualified
import Hydra.Cardano.Api hiding (utxoFromTx)
import Hydra.Prelude hiding (Any, label, lookup, toList)

import Cardano.Api.UTxO (pairs)
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
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
import GHC.IsList (IsList (..))
import GHC.Natural (wordToNatural)
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ClientInput qualified as Input
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.BehaviorSpec (SimulatedChainNetwork (..), TestHydraClient (..), createHydraNode, createTestHydraClient, getHeadUTxO, shortLabel, waitMatch, waitUntilMatch)
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic (Committed ())
import Hydra.Ledger.Cardano (cardanoLedger, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Model.MockChain (mockChainAndNetwork)
import Hydra.Model.Payment (CardanoSigningKey (..), Payment (..), applyTx, genAdaValue)
import Hydra.Node (runHydraNode)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Tx.Crypto (HydraKey)
import Hydra.Tx.DepositDeadline (DepositDeadline (UnsafeDepositDeadline))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Party (Party (..), deriveParty)
import Hydra.Tx.Snapshot qualified as Snapshot
import Test.Hydra.Node.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Test.Hydra.Prelude (failure)
import Test.Hydra.Tx.Gen (genSigningKey)
import Test.QuickCheck (choose, elements, frequency, oneof, resize, sized, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), HasVariables, PostconditionM, Realized, RunModel (..), StateModel (..), Var, VarContext, counterexamplePost)
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
      , committed :: Committed Payment
      }
  | Closed
      { headParameters :: HeadParameters
      , closedUTxO :: UTxOType Payment
      }
  | Final {finalUTxO :: UTxOType Payment}
  deriving stock (Eq, Show)

type Uncommitted = Map.Map Party (UTxOType Payment)

newtype OffChainState = OffChainState {confirmedUTxO :: UTxOType Payment}
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
      , seedDepositDeadline :: DepositDeadline
      , toCommit :: Uncommitted
      } ->
      Action WorldState ()
    -- NOTE: No records possible here as we would duplicate 'Party' fields with
    -- different return values.
    Init :: Party -> Action WorldState ()
    Commit :: Party -> UTxOType Payment -> Action WorldState ActualCommitted
    Decommit :: Party -> Payment -> Action WorldState ()
    Abort :: Party -> Action WorldState ()
    Close :: Party -> Action WorldState ()
    Fanout :: Party -> Action WorldState UTxO
    NewTx :: Party -> Payment -> Action WorldState Payment
    Wait :: DiffTime -> Action WorldState ()
    ObserveConfirmedTx :: Var Payment -> Action WorldState ()
    -- Check that all parties have observed the head as open
    ObserveHeadIsOpen :: Action WorldState ()
    RollbackAndForward :: Natural -> Action WorldState ()
    CloseWithInitialSnapshot :: Party -> Action WorldState ()
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
      Open{offChainState = OffChainState{confirmedUTxO}} ->
        genOpenActions confirmedUTxO
      Closed{} ->
        frequency
          [ (5, genFanout)
          , (1, genRollbackAndForward)
          ]
      Final{} -> fmap Some genSeed
   where
    genCommit :: Uncommitted -> Gen (Any (Action WorldState))
    genCommit pending = do
      (party, commits) <- elements $ Map.toList pending
      pure . Some $ Commit party commits

    -- NOTE: Some actions depend on confirmed 'UTxO' in the head so
    -- we need to make sure there are funds to spend when generating a
    -- `NewTx` action for example but also want to make sure that after
    -- a 'Decommit' we are not left without any funds so further actions
    -- can be generated.
    genOpenActions :: UTxOType Payment -> Gen (Any (Action WorldState))
    genOpenActions confirmedUTxO =
      if null confirmedUTxO
        then
          oneof
            [ genClose
            , genRollbackAndForward
            ]
        else
          frequency $
            [ (10, genNewTx)
            , (1, genClose)
            , (1, genRollbackAndForward)
            ]
              <> [(2, genDecommit) | length confirmedUTxO > 1]

    genDecommit :: Gen (Any (Action WorldState))
    genDecommit = do
      genPayment st >>= \(party, tx) -> pure . Some $ Decommit party tx

    genAbort =
      Some . Abort . deriveParty . fst <$> elements hydraParties

    genNewTx = genPayment st >>= \(party, transaction) -> pure . Some $ NewTx party transaction

    genClose =
      Some . Close . deriveParty . fst <$> elements hydraParties

    genFanout =
      Some . Fanout . deriveParty . fst <$> elements hydraParties

    genRollbackAndForward = do
      numberOfBlocks <- choose (1, 2)
      pure . Some $ RollbackAndForward (wordToNatural numberOfBlocks)

  precondition WorldState{hydraState = Start} Seed{} =
    True
  precondition WorldState{hydraState = Idle{idleParties}} (Init p) =
    p `elem` idleParties
  precondition WorldState{hydraState = Initial{pendingCommits}} (Commit party _) =
    party `Map.member` pendingCommits
  precondition WorldState{hydraState = Initial{commits, pendingCommits}} (Abort party) =
    party `Set.member` (Map.keysSet pendingCommits <> Map.keysSet commits)
  precondition WorldState{hydraState = Open{}} (Close _) =
    True
  precondition WorldState{hydraState = Open{offChainState}} (NewTx _ tx) =
    (from tx, value tx) `List.elem` confirmedUTxO offChainState
  precondition _ Wait{} =
    True
  precondition WorldState{hydraState = Open{offChainState}} (Decommit _ tx) =
    (from tx, value tx) `List.elem` confirmedUTxO offChainState
  precondition WorldState{hydraState = Open{}} (ObserveConfirmedTx _) =
    True
  precondition WorldState{hydraState = Open{}} ObserveHeadIsOpen =
    True
  precondition WorldState{hydraState = Closed{}} (Fanout _) =
    True
  precondition WorldState{hydraState = Open{}} (CloseWithInitialSnapshot _) =
    True
  precondition WorldState{hydraState} (RollbackAndForward _) =
    case hydraState of
      Start{} -> False
      Idle{} -> False
      Initial{} -> False
      Open{} -> True
      Closed{} -> True
      Final{} -> False
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
      Decommit _party tx ->
        WorldState{hydraParties, hydraState = updateWithDecommit hydraState}
       where
        updateWithDecommit = \case
          Open{headParameters, committed, offChainState = OffChainState{confirmedUTxO}} ->
            Open
              { headParameters
              , committed
              , offChainState =
                  OffChainState
                    { confirmedUTxO =
                        List.delete (from tx, value tx) confirmedUTxO
                    }
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
                    , committed = commits'
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
      Close{} ->
        WorldState{hydraParties, hydraState = updateWithClose hydraState}
       where
        updateWithClose = \case
          Open{offChainState = OffChainState{confirmedUTxO}, headParameters} -> Closed{headParameters, closedUTxO = confirmedUTxO}
          _ -> error "unexpected state"
      Fanout{} ->
        WorldState{hydraParties, hydraState = updateWithFanout hydraState}
       where
        updateWithFanout = \case
          Closed{closedUTxO} -> Final closedUTxO
          _ -> error "unexpected state"
      --
      (NewTx _ tx) ->
        WorldState{hydraParties, hydraState = updateWithNewTx hydraState}
       where
        updateWithNewTx = \case
          Open{headParameters, committed, offChainState = OffChainState{confirmedUTxO}} ->
            Open
              { headParameters
              , committed
              , offChainState =
                  OffChainState
                    { confirmedUTxO = confirmedUTxO `applyTx` tx
                    }
              }
          _ -> error "unexpected state"
      CloseWithInitialSnapshot _ ->
        WorldState{hydraParties, hydraState = updateWithClose hydraState}
       where
        updateWithClose = \case
          Open{offChainState = OffChainState{confirmedUTxO}, headParameters} -> Closed{headParameters, closedUTxO = confirmedUTxO}
          _ -> error "unexpected state"
      RollbackAndForward _numberOfBlocks -> s
      Wait _ -> s
      ObserveConfirmedTx _ -> s
      ObserveHeadIsOpen -> s
      StopTheWorld -> s

  shrinkAction _ctx _st = \case
    seed@Seed{seedKeys, toCommit} ->
      [ Some seed{seedKeys = seedKeys', toCommit = toCommit'}
      | seedKeys' <- shrink seedKeys
      , let toCommit' = Map.filterWithKey (\p _ -> p `elem` (deriveParty . fst <$> seedKeys')) toCommit
      ]
    _other -> []

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
  seedDepositDeadline <- genDepositDeadline
  toCommit <- mconcat <$> mapM genToCommit seedKeys
  pure $ Seed{seedKeys, seedContestationPeriod, seedDepositDeadline, toCommit}

genToCommit :: (SigningKey HydraKey, CardanoSigningKey) -> Gen (Map Party [(CardanoSigningKey, Value)])
genToCommit (hk, ck) = do
  value <- genAdaValue
  pure $ Map.singleton (deriveParty hk) [(ck, value)]

genContestationPeriod :: Gen ContestationPeriod
genContestationPeriod = do
  n <- choose (1, 200)
  pure $ UnsafeContestationPeriod $ wordToNatural n

genDepositDeadline :: Gen DepositDeadline
genDepositDeadline = do
  n <- choose (1, 200)
  pure $ UnsafeDepositDeadline $ wordToNatural n

genInit :: [(SigningKey HydraKey, b)] -> Gen (Action WorldState ())
genInit hydraParties = do
  key <- fst <$> elements hydraParties
  let party = deriveParty key
  pure $ Init party

genPayment :: WorldState -> Gen (Party, Payment)
genPayment WorldState{hydraParties, hydraState} =
  case hydraState of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      (from, value) <-
        elements (filter (not . null . toList . snd) confirmedUTxO)
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
  , logger :: Tracer m (HydraLog Tx)
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
  , MonadTime m
  ) =>
  RunModel WorldState (RunMonad m)
  where
  postcondition (_, st) action _lookup result = do
    counterexamplePost "Postcondition failed"
    counterexamplePost ("Action:   " <> show action)
    counterexamplePost ("State:    " <> show st)

    case action of
      (Commit _party expectedCommitted) ->
        expectedCommitted === result
      Fanout{} ->
        case hydraState st of
          Final{finalUTxO} -> do
            -- NOTE: Sort `[TxOut]` by the address and values. We want to make
            -- sure that the fanout outputs match what we had in the open Head
            -- exactly.
            let sorted = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o)))
            sorted (toTxOuts finalUTxO) === sorted (Data.Foldable.toList result)
          _ -> pure False
      _ -> pure True

  monitoring (s, s') _action _lookup _result =
    decorateTransitions
   where
    decorateTransitions =
      case (hydraState s, hydraState s') of
        (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

  perform st action lookup = do
    case action of
      Seed{seedKeys, seedContestationPeriod, seedDepositDeadline, toCommit} ->
        seedWorld seedKeys seedContestationPeriod seedDepositDeadline toCommit
      Commit party utxo ->
        performCommit (snd <$> hydraParties st) party utxo
      Decommit party tx ->
        performDecommit party tx
      NewTx party transaction ->
        performNewTx party transaction
      Init party ->
        performInit party
      Abort party -> do
        performAbort party
      Close party ->
        performClose party
      Fanout party ->
        performFanout party
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
      CloseWithInitialSnapshot party ->
        performCloseWithInitialSnapshot st party
      RollbackAndForward numberOfBlocks ->
        performRollbackAndForward numberOfBlocks
      StopTheWorld ->
        stopTheWorld

-- ** Performing actions

seedWorld ::
  ( MonadAsync m
  , MonadTimer m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadFork m
  , MonadMask m
  , MonadDelay m
  , MonadTime m
  ) =>
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  ContestationPeriod ->
  DepositDeadline ->
  Uncommitted ->
  RunMonad m ()
seedWorld seedKeys seedCP depositDeadline futureCommits = do
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
      messages <- atomically newTQueue
      labelTQueueIO messages ("messages-" <> shortLabel hsk)
      outputHistory <- newTVarIO []
      labelTVarIO outputHistory ("history-" <> shortLabel hsk)
      node <- createHydraNode (contramap Node tr) ledger initialChainState hsk otherParties outputs messages outputHistory mockChain seedCP depositDeadline
      let testClient = createTestHydraClient outputs messages outputHistory node
      nodeThread <- async $ labelThisThread ("node-" <> shortLabel hsk) >> runHydraNode node
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
    Just{} -> do
      let realUTxO = toRealUTxO paymentUTxO
      lift $ simulateCommit (party, realUTxO)
      observedUTxO <-
        lift $
          forM nodes $ \n ->
            waitMatch n $ \case
              Committed{party = cp, utxo = committedUTxO}
                | cp == party, committedUTxO == realUTxO -> Just committedUTxO
              _ -> Nothing
      pure $ fromUtxo $ List.head $ Data.Foldable.toList observedUTxO
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

performDecommit ::
  (MonadThrow m, MonadTimer m, MonadAsync m, MonadDelay m) =>
  Party ->
  Payment ->
  RunMonad m ()
performDecommit party tx = do
  let recipient = mkVkAddress testNetworkId . getVerificationKey . signingKey $ to tx
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode

  (i, o) <-
    lift (waitForUTxOToSpend mempty (from tx) (value tx) thisNode) >>= \case
      Left u -> error $ "Cannot execute Decommit for " <> show tx <> ", no spendable UTxO in " <> show u
      Right ok -> pure ok

  let realTx =
        either
          (error . show)
          id
          (mkSimpleTx (i, o) (recipient, value tx) (signingKey $ from tx))

  party `sendsInput` Input.Decommit realTx

  lift $ do
    waitUntilMatch (Map.elems nodes) $ \case
      DecommitFinalized{} -> True
      _ -> False

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
    waitUntilMatch (Data.Foldable.toList nodes) $ \case
      SnapshotConfirmed{snapshot = snapshot} ->
        realTx `elem` Snapshot.confirmed snapshot
      err@(TxInvalid{}) -> error ("expected tx to be valid: " <> show err)
      _ -> False
    pure tx

-- | Wait for the head to be open by searching from the beginning. Note that
-- there rollbacks or multiple life-cycles of heads are not handled here.
waitForOpen :: MonadDelay m => TestHydraClient tx m -> RunMonad m ()
waitForOpen node = do
  outs <- lift $ serverOutputs node
  unless (any headIsOpen outs) waitAndRetry
 where
  waitAndRetry = lift (threadDelay 0.1) >> waitForOpen node

-- | Wait for the head to be closed by searching from the beginning. Note that
-- there rollbacks or multiple life-cycles of heads are not handled here.
waitForReadyToFanout :: MonadDelay m => TestHydraClient tx m -> RunMonad m ()
waitForReadyToFanout node = do
  outs <- lift $ serverOutputs node
  unless (any headIsReadyToFanout outs) waitAndRetry
 where
  waitAndRetry = lift (threadDelay 0.1) >> waitForReadyToFanout node

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
    waitUntilMatch (Data.Foldable.toList nodes) $ \case
      HeadIsInitializing{} -> True
      _ -> False

performAbort :: (MonadThrow m, MonadAsync m, MonadTimer m) => Party -> RunMonad m ()
performAbort party = do
  party `sendsInput` Input.Abort

  nodes <- gets nodes
  lift $
    waitUntilMatch (Data.Foldable.toList nodes) $ \case
      HeadIsAborted{} -> True
      _ -> False

performClose :: (MonadThrow m, MonadAsync m, MonadTimer m, MonadDelay m) => Party -> RunMonad m ()
performClose party = do
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode
  party `sendsInput` Input.Close

  lift $
    waitUntilMatch (Data.Foldable.toList nodes) $ \case
      HeadIsClosed{} -> True
      _ -> False

performFanout :: (MonadThrow m, MonadAsync m, MonadDelay m) => Party -> RunMonad m UTxO
performFanout party = do
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForReadyToFanout thisNode
  party `sendsInput` Input.Fanout
  findInOutput thisNode (100 :: Int)
 where
  findInOutput node n
    | n == 0 = failure "Failed to perform Fanout"
    | otherwise = do
        outputs <- lift $ serverOutputs node
        case find headIsFinalized outputs of
          Just (HeadIsFinalized{utxo}) -> pure utxo
          _ -> lift (threadDelay 1) >> findInOutput node (n - 1)
  headIsFinalized = \case
    HeadIsFinalized{} -> True
    _otherwise -> False

performCloseWithInitialSnapshot :: (MonadThrow m, MonadTimer m, MonadDelay m, MonadAsync m) => WorldState -> Party -> RunMonad m ()
performCloseWithInitialSnapshot st party = do
  nodes <- gets nodes
  let thisNode = nodes ! party
  waitForOpen thisNode
  case hydraState st of
    Open{committed} -> do
      SimulatedChainNetwork{closeWithInitialSnapshot} <- gets chain
      _ <- lift $ closeWithInitialSnapshot (party, toRealUTxO $ foldMap snd $ Map.toList committed)
      lift $
        waitUntilMatch (Data.Foldable.toList nodes) $ \case
          HeadIsClosed{snapshotNumber} ->
            -- we deliberately wait to see close with the initial snapshot
            -- here to mimic one node not seeing the confirmed tx
            snapshotNumber == Snapshot.UnsafeSnapshotNumber 0
          _ -> False
    _ -> error "Not in open state"

performRollbackAndForward :: (MonadThrow m, MonadTimer m) => Natural -> RunMonad m ()
performRollbackAndForward numberOfBlocks = do
  SimulatedChainNetwork{rollbackAndForward} <- gets chain
  lift $ rollbackAndForward numberOfBlocks

stopTheWorld :: MonadAsync m => RunMonad m ()
stopTheWorld =
  gets threads >>= mapM_ (lift . cancel)

-- ** Utility functions

-- | Convert payment-style utxos into transaction outputs.
toTxOuts :: [(CardanoSigningKey, Value)] -> [TxOut CtxUTxO]
toTxOuts payments =
  uncurry mkTxOut <$> payments

-- | Convert payment-style utxos into real utxos. The 'Payment' tx domain is
-- smaller than UTxO and we map every unique signer + value entry to a mocked
-- 'TxIn' on the real cardano domain.
toRealUTxO :: UTxOType Payment -> UTxOType Tx
toRealUTxO paymentUTxO =
  UTxO.fromPairs $
    [ (mkMockTxIn sk ix, mkTxOut sk val)
    | (sk, vals) <- Map.toList skMap
    , (ix, val) <- zip [0 ..] vals
    ]
 where
  skMap = foldMap (\(sk, v) -> Map.singleton sk [v]) paymentUTxO

mkTxOut :: CardanoSigningKey -> Value -> TxOut CtxUTxO
mkTxOut (CardanoSigningKey sk) val =
  TxOut (mkVkAddress testNetworkId (getVerificationKey sk)) val TxOutDatumNone ReferenceScriptNone

mkMockTxIn :: CardanoSigningKey -> Word -> TxIn
mkMockTxIn (CardanoSigningKey sk) ix =
  TxIn (TxId tid) (TxIx ix)
 where
  vk = getVerificationKey sk
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)

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
  Decommit{} -> k
  Abort{} -> k
  Close{} -> k
  Fanout{} -> k
  NewTx{} -> k
  Wait{} -> k
  ObserveConfirmedTx{} -> k
  CloseWithInitialSnapshot{} -> k
  RollbackAndForward{} -> k
  StopTheWorld -> k
  ObserveHeadIsOpen -> k

-- | Like '===', but works in PostconditionM.
(===) :: (Eq a, Show a, Monad m) => a -> a -> PostconditionM m Bool
x === y = do
  counterexamplePost (show x <> "\n" <> interpret res <> "\n" <> show y)
  pure res
 where
  res = x == y
  interpret True = "=="
  interpret False = "/="

waitForUTxOToSpend ::
  forall m.
  MonadDelay m =>
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
      u <- headUTxO node
      if u /= mempty
        then case find matchPayment (UTxO.pairs u) of
          Nothing -> go (n - 1)
          Just (txIn, txOut) -> pure $ Right (txIn, txOut)
        else go (n - 1)

  matchPayment p@(_, txOut) =
    isOwned key p && value == txOutValue txOut

headUTxO ::
  (IsTx tx, MonadDelay m) =>
  TestHydraClient tx m ->
  m (UTxOType tx)
headUTxO node = do
  fromMaybe mempty . getHeadUTxO <$> queryState node

isOwned :: CardanoSigningKey -> (TxIn, TxOut ctx) -> Bool
isOwned (CardanoSigningKey sk) (_, TxOut{txOutAddress = ShelleyAddressInEra (ShelleyAddress _ cre _)}) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False

headIsOpen :: ServerOutput tx -> Bool
headIsOpen = \case
  HeadIsOpen{} -> True
  _otherwise -> False

headIsReadyToFanout :: ServerOutput tx -> Bool
headIsReadyToFanout = \case
  ReadyToFanout{} -> True
  _otherwise -> False
