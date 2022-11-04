{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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
--
-- **NOTE**: This is still pretty much a work-in-progress esp. regarding the execution
-- model as we explore different ways of putting this at work.
module Hydra.Model where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (pairs)
import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Control.Monad.Class.MonadAsync (Async, async)
import Control.Monad.Class.MonadSTM (MonadSTM (newEmptyTMVar), modifyTVar, newTQueue, newTQueueIO, newTVarIO, readTVarIO, takeTMVar, writeTQueue)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.API.ClientInput (ClientInput)
import qualified Hydra.API.ClientInput as Input
import Hydra.API.ServerOutput (ServerOutput (Committed, GetUTxOResponse, ReadyToCommit, SnapshotConfirmed))
import qualified Hydra.API.ServerOutput as Output
import Hydra.BehaviorSpec (
  ConnectToChain (..),
  TestHydraNode (..),
  createHydraNode,
  createMockNetwork,
  createTestHydraNode,
  handleChainEvent,
  waitMatch,
  waitUntil,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (Chain (..), ChainEvent (..), ChainSlot (..), HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.Chain.Direct.Handlers (ChainSyncHandler, DirectChainLog, SubmitTx, chainSyncHandler, mkChain)
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.State (ChainStateAt (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle, queryTimeHandle)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (Committed (), PendingCommits)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genSigningKey, genValue, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Node (HydraNode (..), chainCallback, runHydraNode)
import Hydra.Party (Party (..), deriveParty)
import qualified Hydra.Snapshot as Snapshot
import Test.QuickCheck (counterexample, elements, frequency, resize, sized, suchThat, tabulate, vectorOf)
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
    genSeed = Some . Seed <$> resize 7 partyKeys

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
        party `sendsInput` Input.Abort
      Wait timeout ->
        lift $ threadDelay timeout
      ObserveConfirmedTx tx -> do
        nodes <- Map.toList <$> gets nodes
        forM_ nodes $ \(party, node) -> do
          party `sendsInput` Input.GetUTxO
          waitForUTxOToSpend mempty (to tx) (value tx) party node 1000 >>= \case
            Left u -> error $ "Did not observe transaction " <> show tx <> " applied: " <> show u
            Right _ -> pure ()

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
  tr <- gets logger
  nodes <- lift $ do
    let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
        -- XXX: Defining the concrete 'ChainState' here is weird. The simulated
        -- chain shares the provided chainState between all nodes. However, the
        -- normal (non SimpleTx) chain state is node specific (it's context).
        chainState =
          ChainStateAt
            { chainState = error "should not access chainState"
            , recordedAt = Nothing
            }
    nodes <- newTVarIO []
    connectToChain <-
      mockChainAndNetwork (contramap DirectChain tr) vKeys parties nodes
    forM seedKeys $ \(hsk, _csk) -> do
      outputs <- atomically newTQueue
      outputHistory <- newTVarIO []
      let party = deriveParty hsk
          otherParties = filter (/= party) parties
      node <- createHydraNode ledger chainState hsk otherParties outputs outputHistory connectToChain
      let testNode = createTestHydraNode outputs outputHistory node
      void $ async $ runHydraNode (contramap Node tr) node
      pure (party, testNode)

  modify $ \n -> n{nodes = Map.fromList nodes}

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
  [VerificationKey PaymentKey] ->
  [Party] ->
  TVar m [MockHydraNode m] ->
  m (ConnectToChain Tx m)
mockChainAndNetwork tr (vkey : vkeys) (us : _parties) nodes = do
  history <- newTVarIO []
  queue <- newTQueueIO
  tickThread <- async simulateTicks
  let chainComponent = \node -> do
        let ctx =
              S.ChainContext
                { networkId = testNetworkId
                , peerVerificationKeys = vkeys
                , ownVerificationKey = vkey
                , ownParty = us
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
            cs =
              S.ChainStateAt
                { chainState = S.Idle S.IdleState{S.ctx = ctx}
                , recordedAt = ChainSlot 0
                }
        let HydraNode{nodeState, eq} = node
        let callback = chainCallback nodeState eq
        let getTimeHandle = pure $ arbitrary `generateWith` 42
        let chainHandler = chainSyncHandler tr callback getTimeHandle
        let node' =
              node
                { hn =
                    createMockNetwork node nodes
                , oc =
                    createMockChain tr (submitTx queue) timeHandle
                }
        let mockNode = MockHydraNode{node = node', chainHandler}
        atomically $ modifyTVar nodes (mockNode :)
        pure node'
      rollbackAndForward = undefined
  return ConnectToChain{..}
 where
  blockTime = 20 -- seconds
  simulateTicks = forever $ do
    threadDelay blockTime
    now <- getCurrentTime
    readTVarIO nodes >>= \ns -> mapM_ (`handleChainEvent` Tick now) ns

  timeHandle :: m TimeHandle
  timeHandle = undefined

  submitTx queue vtx = do
    response <- atomically $ do
      response <- newEmptyTMVar
      -- writeTQueue queue (vtx, response)
      return response

    atomically (takeTMVar response)

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
  }

createMockChain ::
  (MonadTimer m, MonadThrow (STM m)) =>
  Tracer m DirectChainLog ->
  SubmitTx m ->
  m TimeHandle ->
  Chain Tx m
createMockChain tracer submitTx timeHandle =
  -- TODO: hook wallet here
  let wallet = undefined
   in mkChain tracer timeHandle wallet submitTx

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
      lift $ waitUntil [actorNode] $ ReadyToCommit (Set.fromList $ Map.keys nodes)
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
              | cp == party ->
                Just committedUTxO
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
performNewTx party tx = do
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

  party `sendsInput` Input.GetUTxO

  (i, o) <-
    waitForUTxOToSpend mempty (from tx) (value tx) party (nodes ! party) (5000 :: Int) >>= \case
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
        realTx `elem` Snapshot.confirmed snapshot
      err@Output.TxInvalid{} -> error ("expected tx to be valid: " <> show err)
      _ -> False

waitForUTxOToSpend ::
  (MonadDelay m, MonadThrow m) =>
  UTxO ->
  CardanoSigningKey ->
  Value ->
  Party ->
  TestHydraNode Tx m ->
  Int ->
  StateT (Nodes m) m (Either UTxO (TxIn, TxOut CtxUTxO))
waitForUTxOToSpend utxo key value party node = \case
  0 ->
    pure $ Left utxo
  n ->
    lift (threadDelay 1 >> waitForNext node) >>= \case
      GetUTxOResponse u
        | u == mempty -> do
          party `sendsInput` Input.GetUTxO
          waitForUTxOToSpend u key value party node (n - 1)
        | otherwise -> case find matchPayment (UTxO.pairs u) of
          Nothing -> do
            party `sendsInput` Input.GetUTxO
            waitForUTxOToSpend u key value party node (n - 1)
          Just p -> pure $ Right p
      _ ->
        waitForUTxOToSpend utxo key value party node (n - 1)
 where
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
genAdaValue = genNonNullAdaValue
 where
  genNonNullAdaValue = genAdaAsset `suchThat` (/= lovelaceToValue 0)
  genAdaAsset = onlyAdaAssets <$> genValue
  onlyAdaAssets = lovelaceToValue . selectLovelace

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
