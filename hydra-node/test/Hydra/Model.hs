{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (TxSeq))
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadAsync (Async, async, cancel)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadSTM (
  MonadLabelledSTM,
  labelTQueueIO,
  labelTVarIO,
  modifyTVar,
  newTQueue,
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
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
  shortLabel,
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
import Hydra.Model.MockChain (mockChainAndNetwork)
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
import Test.QuickCheck (choose, counterexample, elements, frequency, resize, sized, tabulate, vectorOf)
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
  (MonadAsync m, MonadCatch m, MonadTimer m, MonadThrow (STM m), MonadLabelledSTM m) =>
  RunModel WorldState (StateT (Nodes m) m)
runModel = RunModel{perform}
 where
  perform ::
    ( MonadDelay m
    , MonadAsync m
    , MonadCatch m
    , MonadTimer m
    , MonadThrow (STM m)
    , MonadLabelledSTM m
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
      Wait delay ->
        lift $ threadDelay delay
      ObserveConfirmedTx tx -> do
        nodes <- Map.toList <$> gets nodes
        forM_ nodes $ \(_, node) -> do
          lift (waitForUTxOToSpend mempty (to tx) (value tx) node 1000000) >>= \case
            Left u -> error $ "Did not observe transaction " <> show tx <> " applied: " <> show u
            Right _ -> pure ()
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
stopTheWorld =
  gets threads >>= mapM_ (void . lift . cancel)

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
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  ) =>
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  StateT (Nodes m) m ()
seedWorld seedKeys = do
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
      mockChainAndNetwork (contramap DirectChain tr) seedKeys parties nodes
    res <- forM seedKeys $ \(hsk, _csk) -> do
      outputs <- atomically newTQueue
      outputHistory <- newTVarIO []
      labelTVarIO nodes ("outputs-" <> shortLabel hsk)
      labelTVarIO nodes ("history-" <> shortLabel hsk)
      let party = deriveParty hsk
          otherParties = filter (/= party) parties
      node <- createHydraNode ledger dummyNodeState hsk otherParties outputs outputHistory connectToChain
      let testNode = createTestHydraNode outputs outputHistory node
      nodeThread <- async $ labelThisThread ("node-" <> shortLabel hsk) >> runHydraNode (contramap Node tr) node
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
        realTx `elem` Snapshot.confirmed snapshot
      err@Output.TxInvalid{} -> error ("expected tx to be valid: " <> show err)
      _ -> False

waitForUTxOToSpend ::
  (MonadDelay m, MonadTimer m) =>
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
              (go (n - 1))
              (pure . Right)
              (find matchPayment (UTxO.pairs u))
        _ -> go (n - 1)

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

-- Failing Scenario

-- hangingScenario =
--   [ Var 0
--       := Seed
--         { seedKeys =
--             [ (HydraSigningKey (SignKeyEd25519DSIGN "17f477f5ad3c80d5537b35e457aa663fd78b612d320a6abf48c245353d3db24fd02b9edae8187011813f51221b73103396a47669179c9cabeadb93daf6f5a86a"), AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull))
--             , (HydraSigningKey (SignKeyEd25519DSIGN "ae3f4619b0413d70d3004b9131c3752153074e45725be13b9a148978895e359e94e5e8cf96492ade5550fce11efc43c9c61d2bdd020b4b6e3a3846c6bfc27e29"), AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull))
--             , (HydraSigningKey (SignKeyEd25519DSIGN "94455e3ed9f716bea425ef99b51fae47128769a1a0cd04244221e4e14631ab83c4eb69646ce90750f542d05f1583ce67ef8a11bee9bc4c77fab228b3889012d5"), AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull))
--             ]
--         }
--   , Var 1 := Init (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "d02b9edae8187011813f51221b73103396a47669179c9cabeadb93daf6f5a86a")}) 19626 s
--   , Var 2
--       := Commit
--         ( Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "c4eb69646ce90750f542d05f1583ce67ef8a11bee9bc4c77fab228b3889012d5")}
--         )
--         [(AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), valueFromList [(AdaAssetId, 9532276957)])]
--   , Var 3 := Commit (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "d02b9edae8187011813f51221b73103396a47669179c9cabeadb93daf6f5a86a")}) [(AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), valueFromList [(AdaAssetId, 2598099504)])]
--   , Var 4 := Commit (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "94e5e8cf96492ade5550fce11efc43c9c61d2bdd020b4b6e3a3846c6bfc27e29")}) [(AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull), valueFromList [(AdaAssetId, 5253942555)])]
--   , Var 5 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "94e5e8cf96492ade5550fce11efc43c9c61d2bdd020b4b6e3a3846c6bfc27e29")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), value = valueFromList [(AdaAssetId, 5253942555)]}
--   , Var 6 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "d02b9edae8187011813f51221b73103396a47669179c9cabeadb93daf6f5a86a")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), value = valueFromList [(AdaAssetId, 2598099504)]}
--   , Var 7 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "d02b9edae8187011813f51221b73103396a47669179c9cabeadb93daf6f5a86a")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), value = valueFromList [(AdaAssetId, 5253942555)]}
--   , Var 8 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "c4eb69646ce90750f542d05f1583ce67ef8a11bee9bc4c77fab228b3889012d5")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull), value = valueFromList [(AdaAssetId, 9532276957)]}
--   , Var 9 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "c4eb69646ce90750f542d05f1583ce67ef8a11bee9bc4c77fab228b3889012d5")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull), value = valueFromList [(AdaAssetId, 5253942555)]}
--   , Var 10 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "94e5e8cf96492ade5550fce11efc43c9c61d2bdd020b4b6e3a3846c6bfc27e29")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), value = valueFromList [(AdaAssetId, 5253942555)]}
--   , Var 11 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "94e5e8cf96492ade5550fce11efc43c9c61d2bdd020b4b6e3a3846c6bfc27e29")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "71a10f7d46a51c2bd19ee120d5a962d3f60ee0a7ce32441986da7ea9")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), value = valueFromList [(AdaAssetId, 9532276957)]}
--   , Var 12 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "c4eb69646ce90750f542d05f1583ce67ef8a11bee9bc4c77fab228b3889012d5")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), value = valueFromList [(AdaAssetId, 2598099504)]}
--   , Var 13 := NewTx (Party{vkey = HydraVerificationKey (VerKeyEd25519DSIGN "d02b9edae8187011813f51221b73103396a47669179c9cabeadb93daf6f5a86a")}) Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), value = valueFromList [(AdaAssetId, 5253942555)]}
--   , Var 14 := Wait 10 s
--   , Var 15 := ObserveConfirmedTx Payment{from = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "b537eef88b33967c50a827b165fb2a0c70f3d29a98dc03d16ccaa3dc")) StakeRefNull), to = AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) (ShelleyAddress Testnet (KeyHashObj (KeyHash "8808184787c937950e26b338150ad54ac3feb91b89ca608d0164671d")) StakeRefNull), value = valueFromList [(AdaAssetId, 5253942555)]}
--   , Var 16 := StopTheWorld
--   ]
