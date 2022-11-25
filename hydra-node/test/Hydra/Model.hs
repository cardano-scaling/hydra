{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

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
--
-- **NOTE**: This is still pretty much a work-in-progress esp. regarding the execution
-- model as we explore different ways of putting this at work.
module Hydra.Model where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (pairs)
import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (TxSeq))
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadAsync (Async, async, cancel)
import Control.Monad.Class.MonadSTM (modifyTVar, newTQueue, newTQueueIO, newTVarIO, readTVarIO, tryReadTQueue, writeTQueue)
import Control.Monad.Class.MonadTimer (timeout)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.API.ClientInput (ClientInput)
import qualified Hydra.API.ClientInput as Input
import Hydra.API.ServerOutput (ServerOutput (Committed, GetUTxOResponse, SnapshotConfirmed))
import qualified Hydra.API.ServerOutput as Output
import Hydra.BehaviorSpec (
  ConnectToChain (..),
  TestHydraNode (..),
  createHydraNode,
  createTestHydraNode,
  waitMatch,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (Chain (..), HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.Chain.Direct.Handlers (ChainSyncHandler, DirectChainLog, SubmitTx, chainSyncHandler, mkChain, onRollForward)
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.State (ChainContext, ChainStateAt (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle)
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Committed (),
  Environment (party),
  Event (NetworkEvent),
  HeadState (..),
  PendingCommits,
  defaultTTL,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genSigningKey, genTxIn, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  HydraNode (..),
  NodeState (NodeState),
  chainCallback,
  createNodeState,
  modifyHeadState,
  putEvent,
  queryHeadState,
  runHydraNode,
 )
import Hydra.Party (Party (..), deriveParty)
import qualified Hydra.Snapshot as Snapshot
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (choose, counterexample, elements, frequency, resize, sized, suchThat, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), LookUp, RunModel (..), StateModel (..), Var)
import qualified Prelude

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

-- | Concrete state needed to run actions against the implementation.
data Nodes m = Nodes
  { -- | Map from party identifiers to a /handle/ for interacting with a node.
    nodes :: Map.Map Party (TestHydraNode Tx m)
  , -- | Logger used by each node.
    -- The reason we put this here is because the concrete value needs to be
    -- instantiated upon the test run initialisation, outiside of the model.
    logger :: Tracer m (HydraLog Tx ())
  , threads :: [Async m ()]
  }

newtype CardanoSigningKey = CardanoSigningKey {signingKey :: SigningKey PaymentKey}

instance Show CardanoSigningKey where
  show CardanoSigningKey{signingKey} =
    show . mkVkAddress @Era testNetworkId . getVerificationKey $ signingKey

-- NOTE: We need this orphan instance in order to lookup keys in lists.
instance Eq CardanoSigningKey where
  CardanoSigningKey (PaymentSigningKey skd) == CardanoSigningKey (PaymentSigningKey skd') = skd == skd'

instance ToJSON CardanoSigningKey where
  toJSON = error "don't use"

instance FromJSON CardanoSigningKey where
  parseJSON = error "don't use"

instance Arbitrary Value where
  arbitrary = genAdaValue

instance Arbitrary CardanoSigningKey where
  arbitrary = CardanoSigningKey . snd <$> genKeyPair

-- | A single Ada-payment only transaction in our model.
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Payment where
  -- NOTE: We display derived addresses instead of raw signing keys in order to help troubleshooting
  -- tests failures or errors.
  show Payment{from, to, value} =
    "Payment { from = " <> show from
      <> ", to = "
      <> show to
      <> ", value = "
      <> show value
      <> " }"

instance Arbitrary Payment where
  arbitrary = error "don't use"

instance ToCBOR Payment where
  toCBOR = error "don't use"

instance FromCBOR Payment where
  fromCBOR = error "don't use"

-- | Making `Payment` an instance of `IsTx` allows us to use it with `HeadLogic'`s messages.
instance IsTx Payment where
  type TxIdType Payment = Int
  type UTxOType Payment = [(CardanoSigningKey, Value)]
  type ValueType Payment = Value
  txId = error "undefined"
  balance = foldMap snd
  hashUTxO = encodeUtf8 . show @Text

applyTx :: UTxOType Payment -> Payment -> UTxOType Payment
applyTx utxo Payment{from, to, value} =
  (to, value) : List.delete (from, value) utxo

instance DynLogicModel WorldState

type ActualCommitted = UTxOType Payment

-- | Basic instantiation of `StateModel` for our `WorldState` state.
instance StateModel WorldState where
  data Action WorldState a where
    Seed :: {seedKeys :: [(SigningKey HydraKey, CardanoSigningKey)]} -> Action WorldState ()
    -- NOTE: No records possible here as we would duplicate 'Party' fields with
    -- different return values.
    Init :: Party -> ContestationPeriod -> Action WorldState ()
    Commit :: Party -> UTxOType Payment -> Action WorldState ActualCommitted
    Abort :: Party -> Action WorldState ()
    NewTx :: Party -> Payment -> Action WorldState ()
    Wait :: DiffTime -> Action WorldState ()
    ObserveConfirmedTx :: Payment -> Action WorldState ()
    -- | Symmetric to `Seed`
    StopTheWorld :: Action WorldState ()

  initialState =
    WorldState
      { hydraParties = mempty
      , hydraState = Start
      }

  arbitraryAction :: WorldState -> Gen (Any (Action WorldState))
  arbitraryAction st@WorldState{hydraParties, hydraState} =
    case hydraState of
      Start -> genSeed
      Idle{} -> genInit
      Initial{pendingCommits} ->
        frequency
          [ (5, genCommit pendingCommits)
          , (1, genAbort)
          ]
      Open{} -> genNewTx
      _ -> genSeed
   where
    genSeed = Some . Seed <$> resize 3 partyKeys

    genInit = do
      contestationPeriod <- arbitrary
      key <- fst <$> elements hydraParties
      let party = deriveParty key
      pure . Some $ Init party contestationPeriod

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
  precondition _ StopTheWorld =
    True
  precondition _ _ =
    False

  nextState :: WorldState -> Action WorldState a -> Var a -> WorldState
  nextState s@WorldState{hydraParties, hydraState} a _ =
    case a of
      Seed{seedKeys} -> WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys}}
       where
        idleParties = map (deriveParty . fst) seedKeys
        cardanoKeys = map (getVerificationKey . signingKey . snd) seedKeys
      --
      Init _ contestationPeriod ->
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
      StopTheWorld -> s

  postcondition :: WorldState -> Action WorldState a -> LookUp -> a -> Bool
  postcondition _st (Commit _party expectedCommitted) _ actualCommitted =
    expectedCommitted == actualCommitted
  postcondition _ _ _ _ = True

  monitoring (s, s') action l result =
    decoratePostconditionFailure
      . decorateTransitions
   where
    -- REVIEW: This should be part of the quickcheck-dynamic runActions
    decoratePostconditionFailure
      | postcondition s action l result = id
      | otherwise =
        counterexample "postcondition failed"
          . counterexample ("Action: " <> show action)
          . counterexample ("State: " <> show s)
          . counterexample ("Result: " <> show result)

    decorateTransitions =
      case (hydraState s, hydraState s') of
        (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

deriving instance Show (Action WorldState a)
deriving instance Eq (Action WorldState a)

-- * Running the model

runModel ::
  (MonadAsync m, MonadCatch m, MonadTimer m, MonadTime m, MonadThrow (STM m)) =>
  RunModel WorldState (StateT (Nodes m) m)
runModel = RunModel{perform}
 where
  perform ::
    ( MonadDelay m
    , MonadAsync m
    , MonadCatch m
    , MonadTimer m
    , MonadTime m
    , MonadThrow (STM m)
    ) =>
    WorldState ->
    Action WorldState a ->
    LookUp ->
    StateT (Nodes m) m a
  perform st command _ = do
    case command of
      Seed{seedKeys} ->
        seedWorld seedKeys
      Commit party utxo ->
        performCommit (snd <$> hydraParties st) party utxo
      NewTx party transaction ->
        performNewTx party transaction
      Init party contestationPeriod ->
        party `sendsInput` Input.Init{contestationPeriod}
      Abort party -> do
        performAbort party
      Wait timeout ->
        lift $ threadDelay timeout
      ObserveConfirmedTx tx -> do
        nodes <- Map.toList <$> gets nodes
        trace ("waiting for UTxo paying " <> show (value tx) <> " to " <> show (to tx) <> " on " <> show (length nodes) <> " nodes") $
          forM_ nodes $ \(_, node) -> do
            lift (waitForUTxOToSpend mempty (to tx) (value tx) node 1000000) >>= \case
              Left u -> error $ "Did not observe transaction " <> show tx <> " applied: " <> show u
              Right _ -> trace ("observed UTxO paying " <> show (value tx) <> " to " <> show (to tx)) $ pure ()
      StopTheWorld ->
        stopTheWorld

performAbort :: MonadDelay m => Party -> StateT (Nodes m) m ()
performAbort party = do
  Nodes{nodes} <- get
  lift $ waitForReadyToCommit party nodes 100
  party `sendsInput` Input.Abort

waitForReadyToCommit :: MonadDelay m => Party -> Map Party (TestHydraNode tx m) -> Int -> m ()
waitForReadyToCommit _party _nodes 0 = pure ()
waitForReadyToCommit party nodes n = do
  outs <- serverOutputs (nodes ! party)
  let matchReadyToCommit = \case
        Output.ReadyToCommit{} -> True
        _ -> False
  case find matchReadyToCommit outs of
    Nothing ->
      threadDelay 10 >> waitForReadyToCommit party nodes (n -1)
    Just{} ->
      pure ()

stopTheWorld :: MonadAsync m => StateT (Nodes m) m ()
stopTheWorld = trace "StoppingTheWorld" $ do
  Nodes{threads} <- get
  forM_ threads (lift . cancel)

sendsInput :: Monad m => Party -> ClientInput Tx -> StateT (Nodes m) m ()
sendsInput party command = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
    Just actorNode -> lift $ actorNode `send` command

seedWorld ::
  ( MonadDelay m
  , MonadAsync m
  , MonadCatch m
  , MonadTimer m
  , MonadTime m
  , MonadThrow (STM m)
  ) =>
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  StateT (Nodes m) m ()
seedWorld seedKeys = do
  let parties = map (deriveParty . fst) seedKeys
      -- NB: we pick the first key to be our own
      vKeys = getVerificationKey . signingKey . snd <$> seedKeys
      dummyNodeState =
        NodeState
          { modifyHeadState = error "undefined"
          , queryHeadState = error "undefined"
          }
  tr <- gets logger
  nodes <- lift $ do
    let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
    nodes <- newTVarIO []
    (connectToChain, tickThread) <-
      mockChainAndNetwork (contramap DirectChain tr) seedKeys parties nodes
    forM seedKeys $ \(hsk, _csk) -> do
      outputs <- atomically newTQueue
      outputHistory <- newTVarIO []
      let party = deriveParty hsk
          otherParties = filter (/= party) parties
      node <- createHydraNode ledger dummyNodeState hsk otherParties outputs outputHistory connectToChain
      let testNode = createTestHydraNode outputs outputHistory node
      void $ async $ runHydraNode (contramap Node tr) node
      pure ((party, testNode), tickThread)

  modify $ \n ->
    n
      { nodes = Map.fromList (fst <$> nodes)
      , threads = snd <$> nodes
      }

-- | Provide the logic to connect a list of `MockHydraNode` through a dummy chain.
mockChainAndNetwork ::
  forall m.
  ( MonadSTM m
  , MonadTimer m
  , MonadThrow m
  , MonadTime m
  , MonadAsync m
  , MonadThrow (STM m)
  ) =>
  Tracer m DirectChainLog ->
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  [Party] ->
  TVar m [MockHydraNode m] ->
  m (ConnectToChain Tx m, Async m ())
mockChainAndNetwork tr seedKeys _parties nodes = do
  queue <- newTQueueIO
  tickThread <- async (simulateTicks queue)
  let chainComponent = \node -> do
        let ownParty = party (env node)
        let (vkey, vkeys) = findOwnCardanoKey ownParty seedKeys
        let ctx =
              S.ChainContext
                { networkId = testNetworkId
                , peerVerificationKeys = vkeys
                , ownVerificationKey = vkey
                , ownParty
                , scriptRegistry =
                    -- TODO: we probably want different _scripts_ as initial and commit one
                    let txIn = mkMockTxIn vkey 0
                        txOut =
                          TxOut
                            (mkVkAddress testNetworkId vkey)
                            (lovelaceToValue 10_000_000)
                            TxOutDatumNone
                            ReferenceScriptNone
                     in ScriptRegistry
                          { initialReference = (txIn, txOut)
                          , commitReference = (txIn, txOut)
                          }
                }
            chainState =
              S.ChainStateAt
                { chainState = S.Idle
                , recordedAt = Nothing
                }
        let getTimeHandle = pure $ arbitrary `generateWith` 42
        let seedInput = genTxIn `generateWith` 42
        nodeState <- createNodeState $ IdleState{chainState}
        let HydraNode{eq} = node
        let callback = chainCallback nodeState eq
        let chainHandler = chainSyncHandler tr callback getTimeHandle ctx
        let node' =
              node
                { hn =
                    createMockNetwork node nodes
                , oc =
                    createMockChain tr ctx (atomically . writeTQueue queue) getTimeHandle seedInput
                , nodeState
                }
        let mockNode = MockHydraNode{node = node', chainHandler}
        atomically $ modifyTVar nodes (mockNode :)
        pure node'
      -- NOTE: this is not used (yet) but could be used to trigger arbitrary rollbacks
      -- in the run model
      rollbackAndForward = error "Not implemented, should never be called"
  return (ConnectToChain{..}, tickThread)
 where
  blockTime = 20 -- seconds
  simulateTicks queue = forever $ do
    threadDelay blockTime
    -- now <- getCurrentTime
    hasTx <- atomically $ tryReadTQueue queue
    --fmap node <$> readTVarIO nodes >>= \ns -> mapM_ (`handleChainEvent` Tick now) ns
    case hasTx of
      Just tx -> do
        let block = mkBlock tx
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (`onRollForward` block)
      Nothing -> pure ()
mockChainAndNetwork _ _ _ _ =
  error "Cannot connect chain and network without keys"

-- | Find Cardano vkey corresponding to our Hydra vkey using signing keys lookup.
-- This is a bit cumbersome and a tribute to the fact the `HydraNode` itself has no
-- direct knowlege of the cardano keys which are stored only at the `ChainComponent` level.
findOwnCardanoKey :: Party -> [(SigningKey HydraKey, CardanoSigningKey)] -> (VerificationKey PaymentKey, [VerificationKey PaymentKey])
findOwnCardanoKey me seedKeys = fromMaybe (error $ "cannot find cardano key for " <> show me <> " in " <> show seedKeys) $ do
  csk <- getVerificationKey . signingKey . snd <$> find ((== me) . deriveParty . fst) seedKeys
  pure (csk, filter (/= csk) $ map (getVerificationKey . signingKey . snd) seedKeys)

mkBlock :: Ledger.ValidatedTx LedgerEra -> Util.Block
mkBlock ledgerTx =
  let header = (arbitrary :: Gen (Praos.Header StandardCrypto)) `generateWith` 42
      body = TxSeq . StrictSeq.fromList $ [ledgerTx]
   in BlockBabbage $ mkShelleyBlock $ Ledger.Block header body

-- TODO: unify with BehaviorSpec's ?
createMockNetwork :: MonadSTM m => HydraNode Tx m -> TVar m [MockHydraNode m] -> Network m (Message Tx)
createMockNetwork myNode nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- fmap node <$> readTVarIO nodes
    let otherNodes = filter (\n -> getNodeId n /= getNodeId myNode) allNodes
    mapM_ (`handleMessage` msg) otherNodes

  handleMessage HydraNode{eq} = putEvent eq . NetworkEvent defaultTTL

  getNodeId = party . env

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
  }

createMockChain ::
  (MonadTimer m, MonadThrow (STM m)) =>
  Tracer m DirectChainLog ->
  ChainContext ->
  SubmitTx m ->
  m TimeHandle ->
  TxIn ->
  Chain Tx m
createMockChain tracer ctx submitTx timeHandle seedInput =
  -- NOTE: The wallet basically does nothing
  let wallet =
        TinyWallet
          { getUTxO = pure mempty
          , getSeedInput = pure (Just seedInput)
          , sign = id
          , coverFee = \_ tx -> pure (Right tx)
          , reset = const $ pure ()
          , update = const $ pure ()
          }
   in mkChain tracer timeHandle wallet ctx submitTx

performCommit ::
  (MonadThrow m, MonadAsync m, MonadTimer m) =>
  [CardanoSigningKey] ->
  Party ->
  [(CardanoSigningKey, Value)] ->
  StateT (Nodes m) m ActualCommitted
performCommit parties party paymentUTxO = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
    Just actorNode -> do
      lift $ waitForReadyToCommit party nodes 100
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
  StateT (Nodes m) m ()
performNewTx party tx = trace ("performing new tx " <> show party) $ do
  let recipient = mkVkAddress testNetworkId . getVerificationKey . signingKey $ to tx
  nodes <- gets nodes

  let waitForOpen = do
        outs <- lift $ serverOutputs (nodes ! party)
        let matchHeadIsOpen = \case
              Output.HeadIsOpen{} -> True
              _ -> False
        case find matchHeadIsOpen outs of
          Nothing -> lift (threadDelay 0.1) >> waitForOpen
          Just{} -> pure ()
  waitForOpen

  (i, o) <-
    lift (waitForUTxOToSpend mempty (from tx) (value tx) (nodes ! party) (100000 :: Int)) >>= \case
      Left u -> error $ "Cannot execute NewTx for " <> show tx <> ", no spendable UTxO in " <> show u
      Right ok -> pure ok

  let realTx =
        either
          (error . show)
          id
          (mkSimpleTx (i, o) (recipient, value tx) (signingKey $ from tx))

  party `sendsInput` Input.NewTx realTx
  lift $
    waitUntilMatch [nodes ! party] $ \case
      SnapshotConfirmed{Output.snapshot = snapshot} ->
        trace ("performed new tx " <> show party) $
          realTx `elem` Snapshot.confirmed snapshot
      err@Output.TxInvalid{} -> error ("expected tx to be valid: " <> show err)
      _ -> False

waitForUTxOToSpend ::
  (MonadDelay m, MonadThrow m, MonadTimer m) =>
  UTxO ->
  CardanoSigningKey ->
  Value ->
  TestHydraNode Tx m ->
  Int ->
  m (Either UTxO (TxIn, TxOut CtxUTxO))
waitForUTxOToSpend utxo key value node = go
 where
  go = \case
    0 ->
      pure $ Left utxo
    n -> do
      node `send` Input.GetUTxO
      threadDelay 5
      timeout 1000 (waitForNext node) >>= \case
        Just (GetUTxOResponse u)
          | u /= mempty ->
            maybe
              (trace ("cannot find UTxO paying " <> show value <> " to " <> show key <> " in " <> show u) $ go (n - 1))
              (trace "found it!" . pure . Right)
              (find matchPayment (UTxO.pairs u))
        r -> trace ("retrying, got response :" <> show r) $ go (n - 1)

  matchPayment p@(_, txOut) =
    isOwned key p && value == txOutValue txOut

--

-- * Generator Helpers

--

genPayment :: WorldState -> Gen (Party, Payment)
genPayment WorldState{hydraParties, hydraState} =
  case hydraState of
    Open{offChainState = OffChainState{confirmedUTxO}} -> do
      (from, value) <-
        elements confirmedUTxO `suchThat` (not . null . valueToList . snd)
      let party = deriveParty $ fst $ fromJust $ List.find ((== from) . snd) hydraParties
      (_, to) <- elements hydraParties `suchThat` ((/= from) . snd)
      pure (party, Payment{from, to, value})
    _ -> error $ "genPayment impossible in state: " <> show hydraState

genAdaValue :: Gen Value
genAdaValue = lovelaceToValue . fromInteger <$> choose (1, 10000000000)

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

isOwned :: CardanoSigningKey -> (TxIn, TxOut ctx) -> Bool
isOwned (CardanoSigningKey sk) (_, TxOut{txOutAddress = ShelleyAddressInEra (ShelleyAddress _ cre _)}) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)
