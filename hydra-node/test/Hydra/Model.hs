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
  waitUntil,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.ClientInput (ClientInput (NewTx))
import qualified Hydra.ClientInput as Input
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (Committed, PendingCommits)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genAdaValue, genKeyPair, genSigningKey, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Node (HydraNodeLog, runHydraNode)
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (GetUTxOResponse, ReadyToCommit, SnapshotConfirmed))
import qualified Hydra.ServerOutput as Output
import qualified Hydra.Snapshot as Snapshot
import Test.QuickCheck (elements, frequency, resize, sized, suchThat, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), LookUp, StateModel (..), Var)
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
data WorldState (m :: Type -> Type) = WorldState
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

instance
  ( MonadSTM m
  , MonadDelay m
  , MonadAsync m
  , MonadCatch m
  , MonadTime m
  , MonadTimer m
  , MonadFork m
  ) =>
  DynLogicModel (WorldState m)

-- | Basic instantiaion of `StateModel` for our `WorldState m` state.
--
-- The reason why we pack `m` here is technical: What we would like is to have
-- @type ActionMonad WorldState = IOSim s@ but this is not possible because of the
-- existentially quantified `s`. We have tried to use a `newtype`-based wrapper
-- but this extremely cumbersome as it basically requires instantiating all the
-- <io-classes https://github.com/input-output-hk/io-sim/tree/main/io-classes/src/Control/Monad/Class> types /again/.
-- By making it a parameter we can leave the choice of concrete instantiation to the
-- call-site (eg. property runner).
--
-- However, this turns our state essentially not `Typeable` in general which poses
-- some difficulties when working with `DynamicLogic` expressions.
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
    -- | Creation of the world.
    Seed ::
      { seedKeys :: [(SigningKey HydraKey, CardanoSigningKey)]
      } ->
      Action (WorldState m) ()
    -- | All other actions are simply `ClientInput` from some `Party`.
    -- TODO: Provide distinct actions with specific return types that
    -- would make it easier to verify concrete behaviour.
    Command ::
      { party :: Party
      , command :: ClientInput Payment
      } ->
      Action (WorldState m) ()
    -- | Temporary action to cut the sequence of actions.
    -- TODO: Implement proper Close sequence
    Stop :: Action (WorldState m) ()

  -- The monad in which we `perform` actions.
  -- NOTE: We are using a `State` monad in order to be able to retrieve a handle
  -- to the `Node` and send it messages.
  type ActionMonad (WorldState m) = StateT (Nodes m) m

  actionName :: Action (WorldState m) a -> String
  actionName Command{command} = unsafeConstructorName command
  actionName Seed{} = "Seed"
  actionName Stop = "Stop"

  initialState =
    WorldState
      { hydraParties = mempty
      , hydraState = Start
      }

  arbitraryAction :: WorldState m -> Gen (Any (Action (WorldState m)))
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
      initContestationPeriod <- arbitrary
      key <- fst <$> elements hydraParties
      let command = Input.Init{Input.contestationPeriod = initContestationPeriod}
      pure $ Some Command{party = deriveParty key, command}

    genCommit pending = do
      party <- elements $ toList pending
      let (_, sk) = fromJust $ find ((== party) . deriveParty . fst) hydraParties
      value <- genAdaValue
      let command = Input.Commit{Input.utxo = [(sk, value)]}
      pure $ Some Command{party, command}

    genAbort = do
      (key, _) <- elements hydraParties
      pure $ Some Command{party = deriveParty key, command = Input.Abort}

    genNewTx = genPayment st >>= \(party, payment) -> pure $ Some $ Command{party, command = NewTx payment}

  precondition WorldState{hydraState = Start} Seed{} =
    True
  precondition WorldState{hydraState = Idle{}} Command{command = Input.Init{}} =
    True
  precondition WorldState{hydraState = hydraState@Initial{}} Command{party, command = Input.Commit{}} =
    isPendingCommitFrom party hydraState
  precondition WorldState{hydraState = Initial{}} Command{command = Input.Abort{}} =
    True
  precondition WorldState{hydraState = Open{offChainState}} Command{command = Input.NewTx{Input.transaction = tx}} =
    case List.lookup (from tx) (confirmedUTxO offChainState) of
      Just v -> v == value tx
      Nothing -> False
  precondition WorldState{hydraState = Open{}} Stop =
    True
  precondition _ _ =
    False

  nextState :: WorldState m -> Action (WorldState m) a -> Var a -> WorldState m
  nextState s@WorldState{hydraParties, hydraState} a _ =
    case a of
      Stop -> s{hydraState = Final empty}
      --
      Seed{seedKeys} -> WorldState{hydraParties = seedKeys, hydraState = Idle{idleParties, cardanoKeys}}
       where
        idleParties = map (deriveParty . fst) seedKeys
        cardanoKeys = map (getVerificationKey . snd) seedKeys
      --
      Command{command = Input.Init{Input.contestationPeriod}} ->
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
      Command{party, command = Input.Commit{Input.utxo}} ->
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
      Command{command = Input.Abort} ->
        WorldState{hydraParties, hydraState = updateWithAbort hydraState}
       where
        updateWithAbort = \case
          Initial{commits} -> Final committedUTxO
           where
            committedUTxO = mconcat $ Map.elems commits
          _ -> Final mempty
      --
      Command{command = Input.NewTx{Input.transaction = tx}} ->
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
      _ -> error "not implemented"

  perform :: WorldState m -> Action (WorldState m) a -> LookUp -> StateT (Nodes m) m a
  perform _ Seed{seedKeys} _ = seedWorld seedKeys
  perform _ Stop _ = pure ()
  perform st Command{party, command} _ = do
    case command of
      Input.Commit{Input.utxo = utxo} ->
        performCommit party utxo
      Input.NewTx{Input.transaction = tx} ->
        performNewTx st party tx
      Input.Init{Input.contestationPeriod = p} ->
        party `sendsInput` Input.Init{Input.contestationPeriod = p}
      Input.Abort -> do
        party `sendsInput` Input.Abort
      Input.GetUTxO -> do
        party `sendsInput` Input.GetUTxO
      Input.Close -> do
        party `sendsInput` Input.Close
      Input.Contest -> do
        party `sendsInput` Input.Contest
      Input.Fanout -> do
        party `sendsInput` Input.Fanout

  monitoring (s, s') _action _lookup _return =
    case (hydraState s, hydraState s') of
      (st, st') -> tabulate "Transitions" [unsafeConstructorName st <> " -> " <> unsafeConstructorName st']

deriving instance Show (Action (WorldState m) a)
deriving instance Eq (Action (WorldState m) a)

--

-- * Perform Helpers

--

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
  ActionMonad (WorldState m) ()
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
  Party ->
  [(SigningKey PaymentKey, Value)] ->
  StateT (Nodes m) m ()
performCommit party utxo = do
  nodes <- gets nodes
  case Map.lookup party nodes of
    Nothing -> error $ "unexpected party " <> Hydra.Prelude.show party
    Just actorNode -> do
      lift $ waitUntil [actorNode] $ ReadyToCommit (Set.fromList $ Map.keys nodes)
      let realUtxo =
            UTxO.fromPairs $
              [ (mkMockTxIn vk ix, txOut)
              | (ix, (sk, val)) <- zip [0 ..] utxo
              , let vk = getVerificationKey sk
              , let txOut = TxOut (mkVkAddress testNetworkId vk) val TxOutDatumNone ReferenceScriptNone
              ]
      party `sendsInput` Input.Commit{Input.utxo = realUtxo}

performNewTx ::
  (MonadThrow m, MonadAsync m, MonadTimer m) =>
  WorldState m ->
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

genPayment :: WorldState m -> Gen (Party, Payment)
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
