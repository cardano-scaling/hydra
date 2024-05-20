-- | Stateful model-based testing of the transactions created by the "Direct"
-- chain modules.
--
-- The model is focusing on transitions between Open and Closed states of the
-- head right now. Snapshots are only modeled "in proxy" where we generate
-- snapshot numbers and the fact whether they have something to decommit or not,
-- but not the actual snapshot contents. All snapshots are correctly signed by a
-- fixed list of participants and other fixtures are used for head parameters
-- etc.
module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (Any, State, label, show)
import Test.Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  SlotNo (..),
  mkTxOutDatumInline,
  selectLovelace,
  throwError,
  txOutAddress,
  txOutValue,
 )
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.Contract.Mutation (addParticipationTokens, isHeadOutput)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), close, contest, decrement, fanout)
import Hydra.Chain.Direct.Tx (FanoutTxError, HeadObservation, headIdToCurrencySymbol, mkHeadId, mkHeadOutput, observeHeadTx)
import Hydra.Chain.Direct.Tx qualified as Tx
import Hydra.ContestationPeriod qualified as CP
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (MultiSignature, aggregate, sign)
import Hydra.Ledger (hashUTxO, utxoFromTx)
import Hydra.Ledger.Cardano (Tx, adjustUTxO, genUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (partyToChain)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber (..), number)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (genForParty)
import Test.Hydra.Fixture qualified as Fixture
import Test.QuickCheck (Property, Smart (..), checkCoverage, cover, elements, forAll, frequency, ioProperty, oneof, resize)
import Test.QuickCheck.Monadic (monadic)
import Test.QuickCheck.StateModel (
  ActionWithPolarity (..),
  Actions (..),
  Any (..),
  HasVariables (getAllVariables),
  Polarity (PosPolarity),
  PostconditionM,
  Realized,
  RunModel (..),
  StateModel (..),
  Step ((:=)),
  Var,
  VarContext,
  counterexamplePost,
  runActions,
 )
import Text.Show (Show (..))

spec :: Spec
spec = do
  prop "generates interesting transaction traces" prop_traces
  prop "all valid transactions" prop_runActions

prop_traces :: Property
prop_traces =
  forAll (arbitrary :: Gen (Actions Model)) $ \(Actions_ _ (Smart _ steps)) ->
    checkCoverage $
      True
        & cover 1 (null steps) "empty"
        & cover 10 (hasFanout steps) "reach fanout"
        & cover 1 (countContests steps >= 2) "has multiple contests"
        & cover 5 (closeNonInitial steps) "close with non initial snapshots"
        & cover 10 (hasDecrement steps) "has successful decrements"
 where
  hasFanout =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{} -> polarity == PosPolarity
        _ -> False

  countContests =
    length
      . filter
        ( \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
            Contest{} -> polarity == PosPolarity
            _ -> False
        )

  closeNonInitial =
    any $ \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
      Close{snapshot} -> snapshot > 0
      _ -> False

  hasDecrement =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Decrement{} -> polarity == PosPolarity
        _ -> False

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  monadic runAppMProperty $ do
    print actions
    void (runActions actions)
 where
  runAppMProperty :: AppM Property -> Property
  runAppMProperty action = ioProperty $ do
    utxoV <- newIORef openHeadUTxO
    runReaderT (runAppM action) utxoV

-- * Model
data ModelUTxO = A | B | C
  deriving (Show, Eq)

instance Arbitrary ModelUTxO where
  arbitrary = elements [A, B, C]

  shrink = \case
    A -> []
    B -> [A]
    C -> [B]

data Model = Model
  { headState :: State
  , latestSnapshot :: ModelSnapshot
  , alreadyContested :: [Actor]
  , utxoInHead :: [ModelUTxO]
  }
  deriving (Show)

-- | A snapshot that may have pending decommits (= augmented).
data ModelSnapshot = ModelSnapshot {snNumber :: SnapshotNumber, snModel :: [ModelUTxO]}
  deriving (Show, Eq)

snapshotNumber :: ModelSnapshot -> SnapshotNumber
snapshotNumber ModelSnapshot{snNumber} = snNumber

instance Ord ModelSnapshot where
  compare a b = compare (snapshotNumber a) (snapshotNumber b)

instance Num ModelSnapshot where
  a + b = ModelSnapshot{snNumber = snapshotNumber a + snapshotNumber b, snModel = snModel a <> snModel b}
  a - b = ModelSnapshot{snNumber = snapshotNumber a - snapshotNumber b, snModel = snModel a <> snModel b}
  a * b = ModelSnapshot{snNumber = snapshotNumber a * snapshotNumber b, snModel = snModel a <> snModel b}
  abs m = ModelSnapshot{snNumber = abs $ snapshotNumber m, snModel = snModel m}
  signum m = ModelSnapshot{snNumber = signum $ snapshotNumber m, snModel = snModel m}
  fromInteger x = ModelSnapshot{snNumber = UnsafeSnapshotNumber $ fromMaybe 0 $ integerToNatural x, snModel = []}

instance Arbitrary ModelSnapshot where
  arbitrary = ModelSnapshot <$> arbitrary <*> arbitrary

  shrink ModelSnapshot{snNumber, snModel} = ModelSnapshot <$> shrink snNumber <*> shrink snModel

data State
  = Open
  | Closed
  | Final
  deriving (Show, Eq)

data Actor = Alice | Bob | Carol
  deriving (Show, Eq)

data TxResult = TxResult
  { tx :: Either String Tx
  , validationError :: Maybe String
  , observation :: HeadObservation
  }
  deriving (Eq, Show)

instance StateModel Model where
  data Action Model a where
    Decrement :: {actor :: Actor, snapshot :: ModelSnapshot} -> Action Model TxResult
    Close :: {actor :: Actor, snapshot :: ModelSnapshot} -> Action Model TxResult
    Contest :: {actor :: Actor, snapshot :: ModelSnapshot} -> Action Model TxResult
    Fanout :: {snapshot :: ModelSnapshot} -> Action Model TxResult
    -- \| Helper action to identify the terminal state 'Final' and shorten
    -- traces using the 'precondition'.
    Stop :: Action Model ()

  initialState =
    Model
      { headState = Open
      , latestSnapshot = 0
      , alreadyContested = []
      , utxoInHead = []
      }

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState, latestSnapshot} =
    case headState of
      Open{} ->
        oneof
          [ do
              actor <- elements allActors
              snapshot <- latestSnapshot `orSometimes` arbitrary
              pure $ Some $ Close{actor, snapshot}
          , do
              actor <- elements allActors
              snapshot <- (latestSnapshot + 1) `orSometimes` arbitrary
              pure $ Some Decrement{actor, snapshot}
          ]
      Closed{} ->
        oneof
          [ do
              snapshot <- latestSnapshot `orSometimes` arbitrary
              pure . Some $ Fanout{snapshot}
          , do
              actor <- elements allActors
              snapshot <- (latestSnapshot + 1) `orSometimes` arbitrary
              pure $ Some Contest{actor, snapshot}
          ]
      Final -> pure $ Some Stop

  precondition :: Model -> Action Model a -> Bool
  precondition Model{headState, latestSnapshot, alreadyContested} = \case
    Stop -> headState /= Final
    Decrement{snapshot} ->
      -- TODO: assert what to decrement still there
      headState == Open
        && snapshot > latestSnapshot
    Close{snapshot} ->
      headState == Open
        && snapshot >= latestSnapshot
    Contest{actor, snapshot} ->
      headState == Closed
        && actor `notElem` alreadyContested
        && snapshot > latestSnapshot
    Fanout{snapshot} ->
      headState == Closed
        && snapshot == latestSnapshot

  validFailingAction :: Model -> Action Model a -> Bool
  validFailingAction Model{headState, latestSnapshot, alreadyContested} = \case
    Decrement{snapshot} ->
      snapshot <= latestSnapshot
    Close{snapshot} ->
      snapshot < latestSnapshot
    Contest{actor, snapshot} ->
      headState == Closed -- TODO: gracefully fail in perform instead?
        && ( snapshot <= latestSnapshot
              || actor `elem` alreadyContested
           )
    Fanout{snapshot} ->
      headState == Closed -- TODO: gracefully fail in perform instead?
        && snapshot /= latestSnapshot
    _ -> False

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m t _result =
    case t of
      Stop -> m
      Decrement{snapshot} ->
        m
          { headState = Open
          , latestSnapshot = snapshot
          }
      Close{snapshot} ->
        m
          { headState = Closed
          , latestSnapshot = snapshot
          , alreadyContested = []
          }
      Contest{actor, snapshot} ->
        m
          { headState = Closed
          , latestSnapshot = snapshot
          , alreadyContested = actor : alreadyContested m
          }
      Fanout{} -> m{headState = Final}

instance HasVariables Model where
  getAllVariables = mempty

instance HasVariables (Action Model a) where
  getAllVariables = mempty

deriving instance Eq (Action Model a)
deriving instance Show (Action Model a)

-- | Application monad to perform model actions. Currently it only keeps a
-- 'UTxO' which is updated whenever transactions are valid in 'performTx'.
newtype AppM a = AppM {runAppM :: ReaderT (IORef UTxO) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadThrow)

instance MonadReader UTxO AppM where
  ask = AppM $ ask >>= liftIO . readIORef

  local f action = do
    utxo <- ask
    r <- newIORef (f utxo)
    AppM $ local (const r) $ runAppM action

instance MonadState UTxO AppM where
  get = ask
  put utxo = AppM $ ask >>= liftIO . flip writeIORef utxo

type instance Realized AppM a = a

instance RunModel Model AppM where
  perform Model{} action _lookupVar = do
    case action of
      Decrement{actor, snapshot} ->
        performTx =<< newDecrementTx actor (signedSnapshot snapshot)
      Close{actor, snapshot} ->
        performTx =<< newCloseTx actor (confirmedSnapshot snapshot)
      Contest{actor, snapshot} ->
        performTx =<< newContestTx actor (confirmedSnapshot snapshot)
      Fanout{snapshot} -> do
        newFanoutTx Alice snapshot >>= \case
          Left err -> pure $ TxResult{tx = Left (show err), validationError = Nothing, observation = Tx.NoHeadTx}
          Right tx -> performTx tx
      Stop -> pure ()

  postcondition (modelBefore, modelAfter) action _lookup result = runPostconditionM' $ do
    counterexample' (show modelBefore)
    counterexample' (show action)
    case action of
      Decrement{} -> expectValid result $ \case
        Tx.Decrement{} -> pure ()
        _ -> fail "Expected Decrement"
      Close{} -> expectValid result $ \case
        Tx.Close{} -> pure ()
        _ -> fail "Expected Close"
      Contest{} -> expectValid result $ \case
        Tx.Contest Tx.ContestObservation{contesters} -> do
          counterexample' $ "Wrong contesters: expected " <> show (alreadyContested modelAfter) <> ", got " <> show contesters
          guard $ length contesters == length (alreadyContested modelAfter)
        _ -> fail "Expected Contest"
      Fanout{snapshot} -> do
        case result of
          TxResult{tx = Left err} -> fail $ "Failed to construct transaction: " <> err
          TxResult{tx = Right tx} -> do
            -- NOTE: Sort `[TxOut]` by the address and values. We want to make
            -- sure that the fanout outputs match what we had in the open Head
            -- exactly.
            let sorted = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o))) . toList
            let fannedOut = utxoFromTx tx
            -- counterexamplePost ("Fanned out UTxO does not match: " <> renderUTxO fannedOut)
            -- counterexamplePost ("SnapshotUTxO: " <> renderUTxO (snapshotUTxO snapshot))
            guard $ sorted fannedOut == sorted (snapshotUTxO snapshot)

        expectValid result $ \case
          Tx.Fanout{} -> pure ()
          _ -> fail "Expected Fanout"
      _ -> pure ()

  postconditionOnFailure (modelBefore, _modelAfter) action _lookup result = runPostconditionM' $ do
    counterexample' (show modelBefore)
    counterexample' (show action)
    case action of
      Decrement{} -> expectInvalid result
      Close{} -> expectInvalid result
      Contest{} -> expectInvalid result
      Fanout{} -> do
        case result of
          TxResult{validationError = Just _} -> fulfilled
          TxResult{validationError = Nothing} -> counterexample' "Expected to fail validation"

        case result of
          TxResult{tx = Left _} -> fulfilled
          TxResult{tx = Right _} -> counterexample' "Expected failure to build transaction"
      _ -> pure ()

-- | Perform a transaction by evaluating and observing it. This updates the
-- 'UTxO' in the 'AppM' if a transaction is valid and produces a 'TxResult' that
-- can be used to assert expected success / failure.
performTx :: Tx -> AppM TxResult
performTx tx = do
  utxo <- get
  let validationError = getValidationError tx utxo
  when (isNothing validationError) $ do
    put $ adjustUTxO tx utxo
  pure
    TxResult
      { tx = Right tx
      , validationError
      , observation = observeHeadTx Fixture.testNetworkId utxo tx
      }

getValidationError :: Tx -> UTxO -> Maybe String
getValidationError tx utxo =
  case evaluateTx tx utxo of
    Left err ->
      Just $ show err
    Right redeemerReport
      | any isLeft (Map.elems redeemerReport) ->
          Just . toString . unlines $
            fromString
              <$> [ "Transaction evaluation failed: " <> renderTxWithUTxO utxo tx
                  , "Some redeemers failed: " <> show redeemerReport
                  ]
      | otherwise -> Nothing

-- * Fixtures and glue code

-- | List of all model actors corresponding to the fixtures used.
allActors :: [Actor]
allActors = [Alice, Bob, Carol]

-- | A "random" UTxO distribution for a given 'ModelSnapshot'. This always
-- contains one UTxO for alice, bob, and carol.
snapshotUTxO :: ModelSnapshot -> UTxO
snapshotUTxO snapshot =
  (`generateWith` fromIntegral (snapshotNumber snapshot)) . resize 1 $ do
    aliceUTxO <- genUTxOFor (genVerificationKey `genForParty` Fixture.alice)
    bobUTxO <- genUTxOFor (genVerificationKey `genForParty` Fixture.bob)
    carolUTxO <- genUTxOFor (genVerificationKey `genForParty` Fixture.carol)
    pure $ aliceUTxO <> bobUTxO <> carolUTxO

-- | A correctly signed snapshot. Given a snapshot number a snapshot signed by
-- all participants (alice, bob and carol) with some UTxO contained is produced.
signedSnapshot :: ModelSnapshot -> (Snapshot Tx, MultiSignature (Snapshot Tx))
signedSnapshot ms =
  (snapshot, signatures)
 where
  snapshot =
    Snapshot
      { headId = mkHeadId Fixture.testPolicyId
      , number = snapshotNumber ms
      , confirmed = []
      , utxo = allUTxO
      , utxoToDecommit = decommitUTxO
      }
  (allUTxO, decommitUTxO) = pickUTxOToDecommit $ snapshotUTxO ms

  signatures = aggregate [sign sk snapshot | sk <- [Fixture.aliceSk, Fixture.bobSk, Fixture.carolSk]]

splitHeadUTxO :: UTxO -> (UTxO, UTxO)
splitHeadUTxO allUTxO =
  let (headIn, headOut) = List.head $ List.filter (isHeadOutput . snd) (UTxO.pairs allUTxO)
   in (UTxO.singleton (headIn, headOut), UTxO.filter (/= headOut) allUTxO)

pickUTxOToDecommit :: UTxO -> (UTxO, Maybe UTxO)
pickUTxOToDecommit utxo = do
  let pairs = UTxO.pairs utxo
  case pairs of
    [] -> (utxo, Nothing)
    _ -> do
      let toDecommit = elements pairs `generateWith` 42
      (UTxO.fromPairs $ filter (/= toDecommit) pairs, Just $ UTxO.singleton toDecommit)

-- | A confirmed snapshot (either initial or later confirmed), based on
-- 'signedSnapshot'.
confirmedSnapshot :: ModelSnapshot -> ConfirmedSnapshot Tx
confirmedSnapshot = \case
  0 ->
    InitialSnapshot
      { -- -- NOTE: The close validator would not check headId on close with
        -- initial snapshot, but we need to provide it still.
        headId = mkHeadId Fixture.testPolicyId
      , initialUTxO = snapshotUTxO 0
      }
  number -> ConfirmedSnapshot{snapshot, signatures}
   where
    (snapshot, signatures) = signedSnapshot number

-- | UTxO of the open head on-chain. NOTE: This uses fixtures for headId, parties, and cperiod.
openHeadUTxO :: UTxO
openHeadUTxO =
  UTxO.singleton (headTxIn, openHeadTxOut)
    <> registryUTxO testScriptRegistry
 where
  headTxIn = arbitrary `generateWith` 42

  openHeadTxOut =
    mkHeadOutput Fixture.testNetworkId Fixture.testPolicyId openHeadDatum
      & addParticipationTokens [Fixture.alicePVk, Fixture.bobPVk, Fixture.carolPVk]

  openHeadDatum =
    mkTxOutDatumInline
      Head.Open
        { parties = partyToChain <$> [Fixture.alice, Fixture.bob, Fixture.carol]
        , utxoHash = toBuiltin $ hashUTxO @Tx $ snapshotUTxO (ModelSnapshot 0 [])
        , contestationPeriod = CP.toChain Fixture.cperiod
        , headId = headIdToCurrencySymbol $ mkHeadId Fixture.testPolicyId
        , snapshotNumber = 0
        }

-- | Creates a decrement transaction using given utxo and given snapshot.
newDecrementTx :: HasCallStack => Actor -> (Snapshot Tx, MultiSignature (Snapshot Tx)) -> AppM Tx
newDecrementTx actor (snapshot, signatures) = do
  spendableUTxO <- get
  either (failure . show) pure $
    decrement
      (actorChainContext actor)
      (mkHeadId Fixture.testPolicyId)
      Fixture.testHeadParameters
      spendableUTxO
      snapshot
      signatures

-- | Creates a transaction that closes 'openHeadUTxO' with given the snapshot.
-- NOTE: This uses fixtures for headId, parties (alice, bob, carol),
-- contestation period and also claims to close at time 0 resulting in a
-- contestation deadline of 0 + cperiod.
newCloseTx :: HasCallStack => Actor -> ConfirmedSnapshot Tx -> AppM Tx
newCloseTx actor snapshot = do
  spendableUTxO <- get
  either (failure . show) pure $
    close
      (actorChainContext actor)
      spendableUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.testHeadParameters
      snapshot
      lowerBound
      upperBound
 where
  lowerBound = 0

  upperBound = (0, posixSecondsToUTCTime 0)

-- | Creates a contest transaction using given utxo and contesting with given
-- snapshot. NOTE: This uses fixtures for headId, contestation period and also
-- claims to contest at time 0.
newContestTx :: HasCallStack => Actor -> ConfirmedSnapshot Tx -> AppM Tx
newContestTx actor snapshot = do
  spendableUTxO <- get
  either (failure . show) pure $
    contest
      (actorChainContext actor)
      spendableUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.cperiod
      snapshot
      currentTime
 where
  currentTime = (0, posixSecondsToUTCTime 0)

-- | Creates a fanout transaction using given utxo. NOTE: This uses fixtures for
-- seedTxIn and contestation period. Consequently, the lower bound used is
-- precisely at the maximum deadline slot as if everyone contested.
newFanoutTx :: Actor -> ModelSnapshot -> AppM (Either FanoutTxError Tx)
newFanoutTx actor snapshot = do
  spendableUTxO <- get
  let (snapshot', _) = signedSnapshot snapshot
  pure $
    fanout
      (actorChainContext actor)
      spendableUTxO
      Fixture.testSeedInput
      (utxo snapshot')
      (utxoToDecommit snapshot')
      deadline
 where
  CP.UnsafeContestationPeriod contestationPeriod = Fixture.cperiod
  deadline = SlotNo $ fromIntegral contestationPeriod * fromIntegral (length allActors)

-- | Fixture for the chain context of a model 'Actor' on 'testNetworkId'. Uses a generated 'ScriptRegistry'.
actorChainContext :: Actor -> ChainContext
actorChainContext actor =
  ChainContext
    { networkId = Fixture.testNetworkId
    , ownVerificationKey =
        case actor of
          Alice -> Fixture.alicePVk
          Bob -> Fixture.bobPVk
          Carol -> Fixture.carolPVk
    , ownParty =
        case actor of
          Alice -> Fixture.alice
          Bob -> Fixture.bob
          Carol -> Fixture.carol
    , scriptRegistry = testScriptRegistry
    }

testScriptRegistry :: ScriptRegistry
testScriptRegistry = genScriptRegistry `generateWith` 42

-- * Helpers

-- | Run a short-cutting variant of PostconditionM which produces 'True' if it
-- reaches the end, or 'False' if 'fail' is used.
runPostconditionM' :: Monad m => PostconditionM' m () -> PostconditionM m Bool
runPostconditionM' (PostconditionM' action) =
  runExceptT action >>= \case
    Left Nothing -> pure True
    Left (Just err) -> counterexamplePost err $> False
    Right () -> pure True

newtype PostconditionM' m a = PostconditionM' (ExceptT (Maybe String) (PostconditionM m) a)
  deriving newtype (Functor, Applicative, Alternative, Monad)

instance Monad m => MonadFail (PostconditionM' m) where
  fail = PostconditionM' . throwError . Just

-- | Short-cut a postcondition monad like 'fail', but in a successful way. This
-- is useful to not have unrelated counterexample' outputs.
fulfilled :: Monad m => PostconditionM' m ()
fulfilled = PostconditionM' $ throwError Nothing

-- | Add given message in case the postcondition fails.
counterexample' :: Monad m => String -> PostconditionM' m ()
counterexample' msg = PostconditionM' $ ExceptT $ counterexamplePost msg $> Right ()

-- | Assertion helper to check whether a 'TxResult' was valid and the expected
-- 'HeadObservation' could be made. To be used in 'postcondition'.
expectValid :: Monad m => TxResult -> (HeadObservation -> PostconditionM' m a) -> PostconditionM' m a
expectValid TxResult{validationError = Just err} _ =
  fail err
expectValid TxResult{observation} fn = do
  counterexample' $ "Wrong observation: " <> show observation
  fn observation

-- | Assertion helper to check whether a 'TxResult' was invalid.
expectInvalid :: Monad m => TxResult -> PostconditionM' m ()
expectInvalid = \case
  TxResult{validationError = Nothing} -> fail "Expected to fail validation"
  _ -> pure ()

-- | Generate sometimes a value with given generator, bur more often just use
-- the given value.
orSometimes :: a -> Gen a -> Gen a
orSometimes a gen = frequency [(2, pure a), (1, gen)]
