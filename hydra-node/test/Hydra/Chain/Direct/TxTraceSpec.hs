module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (Any, State, label, show)
import Test.Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (SlotNo (..), mkTxOutDatumInline, renderUTxO, selectLovelace, txOutAddress, txOutValue)
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.Contract.Mutation (addParticipationTokens)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), close, contest, decrement, fanout)
import Hydra.Chain.Direct.Tx (HeadObservation, headIdToCurrencySymbol, mkHeadId, mkHeadOutput, observeHeadTx)
import Hydra.Chain.Direct.Tx qualified as Tx
import Hydra.ContestationPeriod qualified as CP
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (MultiSignature, aggregate, sign)
import Hydra.Ledger (hashUTxO, utxoFromTx)
import Hydra.Ledger.Cardano (Tx, adjustUTxO, genUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (partyToChain)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, number)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (genForParty)
import Test.Hydra.Fixture qualified as Fixture
import Test.QuickCheck (Property, Smart (..), checkCoverage, cover, elements, forAll, frequency, oneof, resize)
import Test.QuickCheck.Monadic (monadicIO)
import Test.QuickCheck.StateModel (
  ActionWithPolarity (..),
  Actions (..),
  Any (..),
  HasVariables (getAllVariables),
  LookUp,
  Polarity (PosPolarity),
  PostconditionM,
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
      Close{snapshotNumber} -> snapshotNumber > 0
      _ -> False

  hasDecrement =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Decrement{} -> polarity == PosPolarity
        _ -> False

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  monadicIO $
    void (runActions actions)

-- * Model

data Model = Model
  { headState :: State
  , lastResult :: Maybe (Var TxResult)
  , alreadyContested :: [Actor]
  }
  deriving (Show)

data State
  = Open {latestSnapshot :: SnapshotNumber}
  | Closed {latestSnapshot :: SnapshotNumber}
  | Final
  deriving (Show, Eq)

data Actor = Alice | Bob | Carol
  deriving (Show, Eq)

data TxResult = TxResult
  { newUTxO :: UTxO
  , tx :: Tx
  , validationError :: Maybe String
  , observation :: HeadObservation
  }

instance StateModel Model where
  data Action Model a where
    Decrement :: {actor :: Actor, snapshotNumber :: SnapshotNumber} -> Action Model TxResult
    Close :: {actor :: Actor, snapshotNumber :: SnapshotNumber} -> Action Model TxResult
    Contest :: {actor :: Actor, snapshotNumber :: SnapshotNumber} -> Action Model TxResult
    Fanout :: {snapshotNumber :: SnapshotNumber} -> Action Model TxResult
    -- \| Helper action to identify the terminal state 'Final' and shorten
    -- traces using the 'precondition'.
    Stop :: Action Model ()

  initialState =
    Model
      { headState = Open 0 -- TODO: move latestSnapshot out?
      , lastResult = Nothing
      , alreadyContested = []
      }

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState} =
    case headState of
      Open{} ->
        oneof
          [ do
              actor <- elements allActors
              snapshotNumber <- arbitrary
              pure $ Some $ Close{actor, snapshotNumber}
          , do
              actor <- elements allActors
              snapshotNumber <- arbitrary
              pure $ Some Decrement{actor, snapshotNumber}
          ]
      Closed{latestSnapshot} ->
        oneof
          [ do
              snapshotNumber <-
                frequency
                  [ (2, pure latestSnapshot)
                  , (1, arbitrary)
                  ]
              pure . Some $ Fanout{snapshotNumber}
          , do
              actor <- elements allActors
              snapshotNumber <- arbitrary
              pure $ Some Contest{actor, snapshotNumber}
          ]
      Final -> pure $ Some Stop

  precondition :: Model -> Action Model a -> Bool
  precondition Model{headState, alreadyContested} action =
    case (headState, action) of
      (Final, Stop) -> False
      (Open{latestSnapshot}, Decrement{snapshotNumber}) ->
        snapshotNumber > latestSnapshot
      -- TODO: assert what to decrement still there
      (Open{latestSnapshot}, Close{snapshotNumber}) ->
        snapshotNumber >= latestSnapshot
      (Open{}, Contest{}) -> False
      (Closed{latestSnapshot}, Contest{actor, snapshotNumber}) ->
        actor `notElem` alreadyContested && snapshotNumber > latestSnapshot
      (Open{}, Fanout{}) -> False
      (Closed{latestSnapshot}, Fanout{snapshotNumber}) ->
        snapshotNumber == latestSnapshot
      _ -> True

  validFailingAction :: Model -> Action Model a -> Bool
  validFailingAction Model{headState, alreadyContested} action =
    case (headState, action) of
      (Open{latestSnapshot}, Decrement{snapshotNumber}) -> snapshotNumber <= latestSnapshot
      (Open{latestSnapshot}, Close{snapshotNumber}) -> snapshotNumber < latestSnapshot
      (Closed{latestSnapshot}, Contest{actor, snapshotNumber}) -> snapshotNumber <= latestSnapshot || actor `elem` alreadyContested
      (Closed{latestSnapshot}, Fanout{snapshotNumber}) -> snapshotNumber /= latestSnapshot
      _ -> False

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m Stop _ = m
  nextState m t result =
    case t of
      Decrement{snapshotNumber} ->
        m
          { headState = Open snapshotNumber
          , lastResult = Just result
          }
      Close{snapshotNumber} ->
        m
          { headState = Closed snapshotNumber
          , lastResult = Just result
          , alreadyContested = []
          }
      Contest{actor, snapshotNumber} ->
        m
          { headState = Closed snapshotNumber
          , lastResult = Just result
          , alreadyContested = actor : alreadyContested m
          }
      Fanout{} -> m{headState = Final}

instance HasVariables Model where
  getAllVariables = mempty

instance HasVariables (Action Model a) where
  getAllVariables = mempty

deriving instance Eq (Action Model a)
deriving instance Show (Action Model a)

instance RunModel Model IO where
  perform :: Model -> Action Model a -> LookUp IO -> IO a
  perform Model{lastResult} action lookupVar = do
    case action of
      Decrement{actor, snapshotNumber} -> do
        let utxo = maybe openHeadUTxO (newUTxO . lookupVar) lastResult
        tx <- newDecrementTx utxo actor $ signedSnapshot snapshotNumber
        performTx utxo tx
      Close{actor, snapshotNumber} -> do
        let utxo = maybe openHeadUTxO (newUTxO . lookupVar) lastResult
        tx <- newCloseTx utxo actor $ confirmedSnapshot snapshotNumber
        performTx utxo tx
      Contest{actor, snapshotNumber} -> do
        let utxo = maybe openHeadUTxO (newUTxO . lookupVar) lastResult
        tx <- newContestTx utxo actor $ confirmedSnapshot snapshotNumber
        performTx utxo tx
      Fanout{snapshotNumber} -> do
        let utxo = maybe openHeadUTxO (newUTxO . lookupVar) lastResult
        tx <- newFanoutTx utxo Alice snapshotNumber
        performTx utxo tx
      Stop -> pure ()

  postcondition (modelBefore, modelAfter) action _lookup result = do
    counterexamplePost (show modelBefore)
    counterexamplePost (show action)
    case action of
      Decrement{} -> expectValid result $ \case
        Tx.Decrement{} -> pure True
        _ -> pure False
      Close{} -> expectValid result $ \case
        Tx.Close{} -> pure True
        _ -> pure False
      Contest{} -> expectValid result $ \case
        Tx.Contest Tx.ContestObservation{contesters} -> do
          counterexamplePost $ "Wrong contesters: expected " <> show (alreadyContested modelAfter) <> ", got " <> show contesters
          pure $ length contesters == length (alreadyContested modelAfter)
        _ -> pure False
      Fanout{snapshotNumber} -> do
        valid <- expectValid result $ \case
          Tx.Fanout{} -> pure True
          _ -> pure False
        correctlyFannedOut <- case result of
          TxResult{tx} -> do
            -- NOTE: Sort `[TxOut]` by the address and values. We want to make
            -- sure that the fanout outputs match what we had in the open Head
            -- exactly.
            let sorted = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o))) . toList
            let fannedOut = utxoFromTx tx
            counterexamplePost ("Fanned out UTxO does not match: " <> renderUTxO fannedOut)
            counterexamplePost ("SnapshotUTxO: " <> renderUTxO (snapshotUTxO snapshotNumber))
            pure $ sorted fannedOut == sorted (snapshotUTxO snapshotNumber)
        -- XXX: PostconditionM does not allow case specific counterexamples like Property(M)
        pure $ valid && correctlyFannedOut
      _ -> pure True

  postconditionOnFailure (modelBefore, _modelAfter) action _lookup result = do
    counterexamplePost (show modelBefore)
    counterexamplePost (show action)
    case action of
      Decrement{} -> expectInvalid result
      Close{} -> expectInvalid result
      Contest{} -> expectInvalid result
      _ -> pure True

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
      , utxoToDecommit = Nothing -- TODO: have pending decrements
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

-- | Creates a fanout transaction using given utxo. NOTE: This uses fixtures for
-- seedTxIn and contestation period. Consequently, the lower bound used is
-- precisely at the maximum deadline slot as if everyone contested.
newFanoutTx :: HasCallStack => UTxO -> Actor -> SnapshotNumber -> IO Tx
newFanoutTx spendableUTxO actor snapshotNumber =
  either (failure . show) pure $
    fanout
      (actorChainContext actor)
      spendableUTxO
      Fixture.testSeedInput
      (snapshotUTxO snapshotNumber)
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

-- | Perform a transaction by evaluating and observing it. This produces a
-- 'TxResult' that also contains the new UTxO and can be used to assert expected
-- success / failure.
performTx :: Monad m => UTxO -> Tx -> m TxResult
performTx utxo tx =
  pure
    TxResult
      { newUTxO = adjustUTxO tx utxo
      , tx
      , validationError
      , observation = observeHeadTx Fixture.testNetworkId utxo tx
      }
 where
  validationError =
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

-- | Assertion helper to check whether a 'TxResult' was valid and the expected
-- 'HeadObservation' could be made. To be used in 'postcondition'.
expectValid :: Monad m => TxResult -> (HeadObservation -> PostconditionM m Bool) -> PostconditionM m Bool
expectValid TxResult{validationError = Just err} _ =
  counterexamplePost err $> False
expectValid TxResult{observation} fn =
  counterexamplePost ("Wrong observation: " <> show observation) >> fn observation

-- | Assertion helper to check whether a 'TxResult' was invalid.
expectInvalid :: Monad m => TxResult -> PostconditionM m Bool
expectInvalid = \case
  TxResult{validationError = Nothing} -> False <$ counterexamplePost "Expected to fail validation"
  _ -> pure True
