{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
-- the whole framework, injecting faults, taking into account more parts of the stack (eg.
-- `Direct` chain component), modelling more complex transactions schemes...
--
-- **NOTE**: This is still pretty much a work-in-progress esp. regarding the execution
-- model as we explore different ways of putting this at work.
module Hydra.Model where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (pairs)
import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (newTQueue, newTVarIO)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hydra.BehaviorSpec (
  TestHydraNode (..),
  createHydraNode,
  createTestHydraNode,
  simulatedChainAndNetwork,
  waitMatch,
  waitUntil,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.ClientInput (ClientInput)
import qualified Hydra.ClientInput as Input
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (Committed (), PendingCommits)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genAdaValue, genKeyPair, genSigningKey, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Node (HydraNodeLog, runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (Committed, GetUTxOResponse, ReadyToCommit, SnapshotConfirmed))
import qualified Hydra.ServerOutput as Output
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
-- This state is parameterised by the underlying `Monad m` in which `Action`s will
-- be `perform`ed. See its `StateModel` instance for a detailed explanation.
data WorldState = WorldState
  { -- |List of parties identified by both signing keys required to run protocol.
    -- This list must not contain any duplicated key.
    hydraParties :: [(SigningKey HydraKey, CardanoSigningKey)]
  , hydraState :: GlobalState
  }
  deriving (Eq, Show)

-- | Concrete state needed to run actions against the implementation.
data Nodes m = Nodes
  { nodes :: Map.Map Party (TestHydraNode Tx m)
  , -- | Logger used by each node.
    -- The reason we put this here is because the concrete value needs to be
    -- instantiated upon the test run initialisation, outiside of the model.
    logger :: Tracer m (HydraNodeLog Tx)
  }

type CardanoSigningKey = SigningKey PaymentKey

-- NOTE: We need this orphan instance in order to lookup keys in lists.
instance Eq CardanoSigningKey where
  (PaymentSigningKey skd) == (PaymentSigningKey skd') = skd == skd'

instance ToJSON CardanoSigningKey where
  toJSON = error "don't use"

instance FromJSON CardanoSigningKey where
  parseJSON = error "don't use"

instance Arbitrary Value where
  arbitrary = genAdaValue

instance Arbitrary CardanoSigningKey where
  arbitrary = snd <$> genKeyPair

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
    "Payment { from = " <> signingKeyAsAddress from
      <> ", to = "
      <> signingKeyAsAddress to
      <> ", value = "
      <> show value
      <> " }"
   where
    signingKeyAsAddress = show . mkVkAddress @Era testNetworkId . getVerificationKey

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
    -- | Creation of the world.
    Seed :: {seedKeys :: [(SigningKey HydraKey, CardanoSigningKey)]} -> Action WorldState ()
    -- NOTE: No records possible here as we would duplicate 'Party' fields with
    -- different return values.
    Init :: Party -> ContestationPeriod -> Action WorldState ()
    Commit :: Party -> UTxOType Payment -> Action WorldState ActualCommitted
    Abort :: Party -> Action WorldState ()
    NewTx :: Party -> Payment -> Action WorldState ()
    -- | Temporary action to cut the sequence of actions.
    -- TODO: Implement proper Close sequence
    Stop :: Action WorldState ()

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
      Open{} ->
        frequency
          [ (8, genNewTx)
          , (2, pure $ Some Stop)
          ]
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
    case List.lookup (from tx) (confirmedUTxO offChainState) of
      Just v -> v == value tx
      Nothing -> False
  precondition WorldState{hydraState = Open{}} Stop =
    True
  precondition _ _ =
    False

  nextState :: WorldState -> Action WorldState a -> Var a -> WorldState
  nextState s@WorldState{hydraParties, hydraState} a _ =
    case a of
      Stop -> s{hydraState = Final empty}
      --
      Seed{seedKeys} -> WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys}}
       where
        idleParties = map (deriveParty . fst) seedKeys
        cardanoKeys = map (getVerificationKey . snd) seedKeys
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
  (MonadAsync m, MonadCatch m, MonadTimer m) =>
  RunModel WorldState (StateT (Nodes m) m)
runModel = RunModel{perform = perform}
 where
  perform ::
    ( MonadDelay m
    , MonadAsync m
    , MonadCatch m
    , MonadTimer m
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
        performNewTx st party transaction
      Init party contestationPeriod ->
        party `sendsInput` Input.Init{contestationPeriod}
      Abort party -> do
        party `sendsInput` Input.Abort
      Stop -> pure ()

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
  , MonadTime m
  ) =>
  [(SigningKey HydraKey, b)] ->
  StateT (Nodes m) m ()
seedWorld seedKeys = do
  let parties = map (deriveParty . fst) seedKeys
  tr <- gets logger
  nodes <- lift $ do
    let ledger = cardanoLedger defaultGlobals defaultLedgerEnv
    connectToChain <- simulatedChainAndNetwork
    forM seedKeys $ \(sk, _csk) -> do
      outputs <- atomically newTQueue
      outputHistory <- newTVarIO []
      let party = deriveParty sk
          otherParties = filter (/= party) parties
      node <- createHydraNode ledger sk otherParties outputs outputHistory connectToChain
      let testNode = createTestHydraNode outputs outputHistory node connectToChain
      void $ async $ runHydraNode tr node
      pure (party, testNode)

  modify $ \n -> n{nodes = Map.fromList nodes}

performCommit ::
  (MonadThrow m, MonadAsync m, MonadTimer m) =>
  [CardanoSigningKey] ->
  Party ->
  [(SigningKey PaymentKey, Value)] ->
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
              | (ix, (sk, val)) <- zip [0 ..] paymentUTxO
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
  makeAddressFromSigningKey = mkVkAddress testNetworkId . getVerificationKey

performNewTx ::
  (MonadThrow m, MonadAsync m, MonadTimer m) =>
  WorldState ->
  Party ->
  Payment ->
  StateT (Nodes m) m ()
performNewTx st party tx = do
  let recipient = mkVkAddress testNetworkId (getVerificationKey (to tx))
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

  let matchPayment p@(_, txOut) =
        isOwned (from tx) p && value tx == txOutValue txOut

      waitForUTxO utxo = \case
        0 ->
          lift $
            throwIO $
              Prelude.userError
                ( "no utxo matched,\npayment = " <> show tx
                    <> "\nutxo =  "
                    <> show utxo
                    <> "\nconfirmed = "
                    <> show (fmap (first (mkVkAddress @Era testNetworkId . getVerificationKey)) $ confirmedUTxO $ offChainState $ hydraState st)
                )
        n ->
          lift (threadDelay 1 >> waitForNext (nodes ! party)) >>= \case
            GetUTxOResponse u
              | u == mempty -> do
                party `sendsInput` Input.GetUTxO
                waitForUTxO u (n -1)
              | otherwise -> case find matchPayment (UTxO.pairs u) of
                Nothing -> do
                  party `sendsInput` Input.GetUTxO
                  waitForUTxO u (n -1)
                Just p -> pure p
            _ ->
              waitForUTxO utxo (n -1)

  (i, o) <- waitForUTxO mempty (5000 :: Int)

  let realTx =
        either
          (error . show)
          id
          (mkSimpleTx (i, o) (recipient, value tx) (from tx))

  party `sendsInput` Input.NewTx realTx
  lift $
    waitUntilMatch [nodes ! party] $ \case
      SnapshotConfirmed{Output.snapshot = snapshot} ->
        realTx `elem` Snapshot.confirmed snapshot
      err@Output.TxInvalid{} -> error ("expected tx to be valid: " <> show err)
      _ -> False

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

unsafeConstructorName :: (Show a) => a -> String
unsafeConstructorName = Prelude.head . Prelude.words . show

-- |Generate a list of pairs of Hydra/Cardano signing keys.
-- All the keys in this list are guaranteed to be unique.
partyKeys :: Gen [(SigningKey HydraKey, CardanoSigningKey)]
partyKeys =
  sized $ \len -> do
    hks <- nub <$> vectorOf len arbitrary
    cks <- nub <$> vectorOf len genSigningKey
    pure $ zip hks cks

isOwned :: CardanoSigningKey -> (TxIn, TxOut ctx) -> Bool
isOwned sk (_, TxOut{txOutAddress = ShelleyAddressInEra (ShelleyAddress _ cre _)}) =
  case fromShelleyPaymentCredential cre of
    (PaymentCredentialByKey ha) -> verificationKeyHash (getVerificationKey sk) == ha
    _ -> False
isOwned _ _ = False

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)
