module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (Any, State, label, show)
import Test.Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.List ((\\))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (mkTxOutDatumInline)
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.Contract.Mutation (addParticipationTokens)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), close, contest, decrement)
import Hydra.Chain.Direct.Tx (HeadObservation, headIdToCurrencySymbol, mkHeadId, mkHeadOutput, observeHeadTx)
import Hydra.Chain.Direct.Tx qualified as Tx
import Hydra.ContestationPeriod qualified as CP
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (MultiSignature, aggregate, sign)
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (Tx, adjustUTxO, genUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (partyToChain)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, number)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (genForParty)
import Test.Hydra.Fixture qualified as Fixture
import Test.QuickCheck (Property, Smart (..), checkCoverage, cover, elements, forAll, getPositive, listOf1, oneof, resize)
import Test.QuickCheck.Monadic (monadicIO)
import Test.QuickCheck.StateModel (
  ActionWithPolarity (..),
  Actions (..),
  Any (..),
  HasVariables (getAllVariables),
  LookUp,
  RunModel (..),
  StateModel (..),
  Step ((:=)),
  Var,
  VarContext,
  runActions,
 )
import Text.Pretty.Simple (pShowNoColor)
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
        & cover 5 (countContests steps >= 2) "has multiple contests"
        & cover 5 (containSomeSnapshots steps) "has some snapshots"
        & cover 5 (closeNonInitial steps) "close with non initial snapshots"
        & cover 10 (hasDecrement steps) "has decrements"
 where
  containSomeSnapshots =
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        ProduceSnapshots snapshots -> not $ null snapshots
        _ -> False

  hasFanout =
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        Fanout{} -> True
        _ -> False

  countContests =
    length
      . filter
        ( \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
            Contest{} -> True
            _ -> False
        )

  closeNonInitial =
    any $ \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
      Close{snapshotNumber} -> snapshotNumber > 0
      _ -> False

  hasDecrement =
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        Decrement{} -> True
        _ -> False

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  monadicIO $
    void (runActions actions)

-- * Model

data Model = Model
  { snapshots :: [SnapshotNumber]
  , headState :: State
  , utxoV :: Maybe (Var UTxO)
  -- ^ Last known, spendable UTxO. Use 'openHeadUTxO' if not defined.
  , alreadyContested :: [Actor]
  }
  deriving (Show)

data State
  = Open
  | Closed
  | Final
  deriving (Show, Eq)

data Actor = Alice | Bob | Carol
  deriving (Show, Eq)

instance StateModel Model where
  data Action Model a where
    ProduceSnapshots :: [SnapshotNumber] -> Action Model ()
    Decrement :: {actor :: Actor, snapshotNumber :: SnapshotNumber} -> Action Model UTxO
    Close :: {actor :: Actor, snapshotNumber :: SnapshotNumber} -> Action Model UTxO
    Contest :: {actor :: Actor, snapshotNumber :: SnapshotNumber} -> Action Model UTxO
    Fanout :: Action Model ()
    -- \| Helper action to identify the terminal state 'Final' and shorten
    -- traces using the 'precondition'.
    Stop :: Action Model ()

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState, snapshots, alreadyContested} =
    case headState of
      Open ->
        oneof $
          [ -- NOTE: non-continuous snapshot numbers are allowed in this model
            Some . ProduceSnapshots <$> listOf1 (getPositive <$> arbitrary)
          , do
              actor <- elements allActors
              snapshotNumber <- elements (0 : snapshots)
              pure $ Some $ Close{actor, snapshotNumber}
          ]
            <> maybeToList maybeGenDecrement
      Closed ->
        oneof $
          genFanout
            : maybeToList maybeGenContest
      Final -> pure $ Some Stop
   where
    maybeGenDecrement
      | null snapshots = Nothing
      | otherwise = Just $ do
          actor <- elements allActors
          snapshotNumber <- elements snapshots
          pure $ Some Decrement{actor, snapshotNumber}

    genFanout = pure $ Some Fanout

    possibleContesters = allActors \\ alreadyContested

    maybeGenContest
      | null possibleContesters || null snapshots = Nothing
      | otherwise = Just $ do
          actor <- elements possibleContesters
          snapshotNumber <- elements snapshots
          pure $ Some Contest{actor, snapshotNumber}

  initialState =
    Model
      { snapshots = []
      , headState = Open
      , utxoV = Nothing
      , alreadyContested = []
      }

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m Stop _ = m
  nextState m t result =
    case t of
      ProduceSnapshots snapshots -> m{snapshots = snapshots}
      Decrement{} ->
        m
          { headState = Open
          , utxoV = Just result
          -- TODO: filter snapshots
          }
      Close{snapshotNumber} ->
        m
          { headState = Closed
          , utxoV = Just result
          , snapshots = filter (> snapshotNumber) $ snapshots m
          , alreadyContested = []
          }
      Contest{actor, snapshotNumber} ->
        m
          { headState = Closed
          , utxoV = Just result
          , snapshots = filter (> snapshotNumber) $ snapshots m
          , alreadyContested = actor : alreadyContested m
          }
      Fanout -> m{headState = Final}

  precondition :: Model -> Action Model a -> Bool
  precondition Model{headState} = \case
    Stop -> headState /= Final
    Decrement{} -> headState == Open -- TODO: assert what to decrement still there
    Contest{snapshotNumber} -> headState == Closed && snapshotNumber /= 0
    _ -> True

instance HasVariables Model where
  getAllVariables = mempty

instance HasVariables (Action Model a) where
  getAllVariables = mempty

deriving instance Eq (Action Model a)
deriving instance Show (Action Model a)

instance RunModel Model IO where
  perform :: Model -> Action Model a -> LookUp IO -> IO a
  perform Model{utxoV, alreadyContested} action lookupVar = do
    case action of
      ProduceSnapshots _snapshots -> pure ()
      Decrement{actor, snapshotNumber} -> do
        let utxo = maybe openHeadUTxO lookupVar utxoV
        tx <- newDecrementTx utxo actor $ signedSnapshot snapshotNumber
        validateTx utxo tx
        observeTxMatching openHeadUTxO tx $ \case
          Tx.Decrement{} -> Just ()
          _ -> Nothing
        pure $ adjustUTxO tx utxo
      Close{actor, snapshotNumber} -> do
        let utxo = maybe openHeadUTxO lookupVar utxoV
        tx <- newCloseTx utxo actor $ confirmedSnapshot snapshotNumber
        validateTx utxo tx
        observeTxMatching utxo tx $ \case
          Tx.Close{} -> Just ()
          _ -> Nothing
        pure $ adjustUTxO tx utxo
      Contest{actor, snapshotNumber} -> do
        let utxo = maybe openHeadUTxO lookupVar utxoV
        tx <- newContestTx utxo actor $ confirmedSnapshot snapshotNumber
        validateTx utxo tx
        observation@Tx.ContestObservation{contesters} <-
          observeTxMatching utxo tx $ \case
            Tx.Contest obs -> Just obs
            _ -> Nothing
        let newContesters = actor : alreadyContested
        unless (length contesters == length newContesters) $
          failure . toString . unlines $
            fromString
              <$> [ "Expected contesters " <> show newContesters <> ", but observed only " <> show contesters
                  , toString $ pShowNoColor observation
                  , "Transaction: " <> renderTxWithUTxO utxo tx
                  ]
        pure $ adjustUTxO tx utxo
      Fanout -> pure ()
      Stop -> pure ()

-- * Fixtures and glue code

-- | List of all model actors corresponding to the fixtures used.
allActors :: [Actor]
allActors = [Alice, Bob, Carol]

-- | A "random" UTxO distribution for a given snapshot number. This always
-- contains one UTxO for alice, bob, and carol.
snapshotUTxO :: SnapshotNumber -> UTxO
snapshotUTxO n = (`generateWith` fromIntegral n) . resize 1 $ do
  aliceUTxO <- genUTxOFor (genVerificationKey `genForParty` Fixture.alice)
  bobUTxO <- genUTxOFor (genVerificationKey `genForParty` Fixture.bob)
  carolUTxO <- genUTxOFor (genVerificationKey `genForParty` Fixture.carol)
  pure $ aliceUTxO <> bobUTxO <> carolUTxO

-- | A model of a correctly signed snapshot. Given a snapshot number a snapshot
-- signed by all participants (alice, bob and carol) with some UTxO contained is
-- produced.
signedSnapshot :: SnapshotNumber -> (Snapshot Tx, MultiSignature (Snapshot Tx))
signedSnapshot number =
  (snapshot, signatures)
 where
  snapshot =
    Snapshot
      { headId = mkHeadId Fixture.testPolicyId
      , number
      , utxo = snapshotUTxO number
      , confirmed = []
      , utxoToDecommit = Nothing
      }

  signatures = aggregate [sign sk snapshot | sk <- [Fixture.aliceSk, Fixture.bobSk, Fixture.carolSk]]

-- | A model of a confirmed snapshot (either initial or later confirmed), based
-- on 'signedSnapshot'.
confirmedSnapshot :: SnapshotNumber -> ConfirmedSnapshot Tx
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
        , utxoHash = toBuiltin $ hashUTxO @Tx $ snapshotUTxO 0
        , contestationPeriod = CP.toChain Fixture.cperiod
        , headId = headIdToCurrencySymbol $ mkHeadId Fixture.testPolicyId
        , snapshotNumber = 0
        }

-- | Creates a decrement transaction using given utxo and given snapshot.
newDecrementTx :: HasCallStack => UTxO -> Actor -> (Snapshot Tx, MultiSignature (Snapshot Tx)) -> IO Tx
newDecrementTx utxo actor (snapshot, signatures) =
  either (failure . show) pure $
    decrement
      (actorChainContext actor)
      (mkHeadId Fixture.testPolicyId)
      Fixture.testHeadParameters
      utxo
      snapshot
      signatures

-- | Creates a transaction that closes 'openHeadUTxO' with given the snapshot.
-- NOTE: This uses fixtures for headId, parties (alice, bob, carol),
-- contestation period and also claims to close at time 0 resulting in a
-- contestation deadline of 0 + cperiod.
newCloseTx :: HasCallStack => UTxO -> Actor -> ConfirmedSnapshot Tx -> IO Tx
newCloseTx utxo actor snapshot =
  either (failure . show) pure $
    close
      (actorChainContext actor)
      utxo
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
newContestTx :: HasCallStack => UTxO -> Actor -> ConfirmedSnapshot Tx -> IO Tx
newContestTx spendableUTxO actor snapshot =
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

-- | Thin wrapper around 'evaluateTx' that fails with 'failure' if any of the
-- scripts/redeemers fail to evaluate.
validateTx :: (HasCallStack, MonadThrow m) => UTxO -> Tx -> m ()
validateTx utxo tx =
  case evaluateTx tx utxo of
    Left err ->
      failure $ show err
    Right redeemerReport ->
      when (any isLeft (Map.elems redeemerReport)) $
        failure . toString . unlines $
          fromString
            <$> [ "Transaction evaluation failed: " <> renderTxWithUTxO utxo tx
                , "Some redeemers failed: " <> show redeemerReport
                ]

-- | Expect to observe a transaction matching given predicate. This fails with
-- 'failure' if the predicate yields 'Nothing'.
observeTxMatching :: (HasCallStack, MonadThrow m) => UTxO -> Tx -> (HeadObservation -> Maybe a) -> m a
observeTxMatching utxo tx predicate = do
  let res = observeHeadTx Fixture.testNetworkId utxo tx
  case predicate res of
    Just a -> pure a
    Nothing -> failure $ "Observation result not matching expectation, got " <> show res
