-- | Stateful model-based testing of the transactions created by the "Direct"
-- chain modules.
--
-- FIXME: update description of tactics
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
import Cardano.Ledger.Coin (Coin (..))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Natural (naturalFromInteger, naturalToInteger)
import Hydra.Cardano.Api (
  SlotNo (..),
  lovelaceToValue,
  mkTxOutDatumInline,
  modifyTxOutValue,
  selectLovelace,
  throwError,
  txOutAddress,
  txOutValue,
 )
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.Contract.Mutation (addParticipationTokens)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), DecrementTxError, close, contest, decrement, fanout)
import Hydra.Chain.Direct.Tx (
  CloseTxError,
  ContestTxError,
  FanoutTxError,
  HeadObservation (NoHeadTx),
  headIdToCurrencySymbol,
  mkHeadId,
  mkHeadOutput,
  observeHeadTx,
 )
import Hydra.Chain.Direct.Tx qualified as Tx
import Hydra.ContestationPeriod qualified as CP
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (MultiSignature, aggregate, sign)
import Hydra.Ledger (hashUTxO, utxoFromTx)
import Hydra.Ledger.Cardano (Tx, adjustUTxO, genTxOut, genUTxO1)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (partyToChain)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber (..), number)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture qualified as Fixture
import Test.QuickCheck (Property, Smart (..), checkCoverage, choose, cover, elements, forAll, frequency, ioProperty, oneof, shuffle, sublistOf, withMaxSuccess, (===))
import Test.QuickCheck.Monadic (monadic)
import Test.QuickCheck.StateModel (
  ActionWithPolarity (..),
  Actions (..),
  Any (..),
  HasVariables (getAllVariables),
  Polarity (..),
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
  prop "all valid transactions" $ withMaxSuccess 500 prop_runActions
  prop "realWorldModelUTxO preserves addition" $ \u1 u2 ->
    realWorldModelUTxO (u1 <> u2) === (realWorldModelUTxO u1 <> realWorldModelUTxO u2)

prop_traces :: Property
prop_traces =
  forAll (arbitrary :: Gen (Actions Model)) $ \(Actions_ _ (Smart _ steps)) ->
    checkCoverage $
      True
        & cover 1 (null steps) "empty"
        & cover 10 (hasFanout steps) "reach fanout"
        & cover 1 (fanoutWithEmptyUTxO steps) "fanout with empty UTxO"
        & cover 5 (fanoutWithSomeUTxO steps) "fanout with some UTxO"
        & cover 0.5 (fanoutWithDecrement steps) "fanout with something to decrement"
        & cover 0.5 (fanoutWithSomeUTxOAndDecrement steps) "fanout with some UTxO and something to decrement"
        & cover 1 (countContests steps >= 2) "has multiple contests"
        & cover 5 (closeNonInitial steps) "close with non initial snapshots"
        & cover 5 (closeWithSomeUTxO steps) "close with some UTxO"
        & cover 0.5 (closeWithDecrement steps) "close with something to decrement"
        & cover 0.5 (closeWithSomeUTxOAndDecrement steps) "close with some UTxO and something to decrement"
        & cover 5 (hasDecrement steps) "has successful decrements"
        & cover 5 (hasManyDecrement steps) "has many successful decrements"
 where
  hasSnapshotUTxO snapshot = not . null $ snapshotUTxO snapshot

  hasNoSnapshotUTxO snapshot = null $ snapshotUTxO snapshot

  hasDecommitValue snapshot = sum (Map.elems (decommitUTxO snapshot)) > 0

  hasManyDecommits snapshot =
    let utxoToDecommit = Map.filter (> 0) (decommitUTxO snapshot)
     in size utxoToDecommit > 1

  hasFanout =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{} -> polarity == PosPolarity
        _ -> False

  fanoutWithEmptyUTxO =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{snapshot} ->
          polarity == PosPolarity
            && hasNoSnapshotUTxO snapshot
        _ -> False

  fanoutWithSomeUTxO =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{snapshot} ->
          polarity == PosPolarity
            && hasSnapshotUTxO snapshot
        _ -> False

  fanoutWithDecrement =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{snapshot} ->
          polarity == PosPolarity
            && hasDecommitValue snapshot
        _ -> False

  fanoutWithSomeUTxOAndDecrement =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{snapshot} ->
          polarity == PosPolarity
            && hasSnapshotUTxO snapshot
            && hasDecommitValue snapshot
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

  closeWithDecrement =
    any $ \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
      Close{snapshot} ->
        snapshot > 0
          && hasDecommitValue snapshot
      _ -> False

  closeWithSomeUTxO =
    any $ \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
      Close{snapshot} ->
        snapshot > 0
          && hasSnapshotUTxO snapshot
      _ -> False

  closeWithSomeUTxOAndDecrement =
    any $ \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
      Close{snapshot} ->
        snapshot > 0
          && hasSnapshotUTxO snapshot
          && hasDecommitValue snapshot
      _ -> False

  hasDecrement =
    all $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Decrement{snapshot} ->
          polarity == PosPolarity
            && hasDecommitValue snapshot
        _ -> False

  hasManyDecrement =
    all $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Decrement{snapshot} ->
          polarity == PosPolarity
            && hasDecommitValue snapshot
            && hasManyDecommits snapshot
        _ -> False

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  monadic runAppMProperty $ do
    -- print actions
    void (runActions actions)
 where
  runAppMProperty :: AppM Property -> Property
  runAppMProperty action = ioProperty $ do
    utxoV <- newIORef openHeadUTxO
    runReaderT (runAppM action) utxoV

-- * ============================== MODEL WORLD ==========================

data SingleUTxO = A | B | C | D | E
  deriving (Show, Eq, Ord, Enum, Generic)

instance Arbitrary SingleUTxO where
  arbitrary = genericArbitrary
  shrink = genericShrink

type ModelUTxO = Map SingleUTxO Natural

data Model = Model
  { headState :: State
  , latestSnapshot :: SnapshotNumber
  , alreadyContested :: [Actor]
  , utxoInHead :: ModelUTxO
  , decommitUTxOInHead :: ModelUTxO
  , snVersion :: Integer
  }
  deriving (Show)

-- | Model of a real snapshot which contains a 'SnapshotNumber` but also our
-- simplified form of 'UTxO'.
data ModelSnapshot = ModelSnapshot
  { snapshotNumber :: SnapshotNumber
  , snapshotUTxO :: ModelUTxO
  , decommitUTxO :: ModelUTxO
  , snapshotVersion :: Integer
  }
  deriving (Show, Eq, Ord, Generic)

instance Num ModelSnapshot where
  a + b = ModelSnapshot{snapshotNumber = snapshotNumber a + snapshotNumber b, snapshotUTxO = snapshotUTxO a <> snapshotUTxO b, decommitUTxO = decommitUTxO a <> decommitUTxO b, snapshotVersion = snapshotVersion a}
  _ - _ = error "undefined"
  _ * _ = error "undefined"
  abs _ = error "undefined"
  signum _ = error "undefined"
  fromInteger x = ModelSnapshot{snapshotNumber = UnsafeSnapshotNumber $ fromMaybe 0 $ integerToNatural x, snapshotUTxO = mempty, decommitUTxO = mempty, snapshotVersion = 0}

instance Arbitrary ModelSnapshot where
  arbitrary = genericArbitrary

  shrink = genericShrink

data State
  = Open
  | Closed
  | Final
  deriving (Show, Eq)

data Actor = Alice | Bob | Carol
  deriving (Show, Eq)

-- | Result of constructing and performing a transaction. Notably there are
-- three stages to this which can fail: construction, validation, and
-- observation. Results from all stages are needed to express post-conditions.
data TxResult = TxResult
  { constructedTx :: Either String Tx
  , spendableUTxO :: UTxO
  , validationError :: Maybe String
  , observation :: HeadObservation
  }
  deriving (Eq, Show)

initialAmount :: Natural
initialAmount = 10

balanceUTxOInHead :: Ord k => Map k Natural -> Map k Natural -> Map k Natural
balanceUTxOInHead currentUtxoInHead someUTxOToDecrement =
  let
    currentUtxoInHead' = fmap naturalToInteger currentUtxoInHead
    someUTxOToDecrement' = fmap (negate . naturalToInteger) someUTxOToDecrement
   in
    Map.map naturalFromInteger
      . Map.filter (> 0)
      . Map.map sum
      $ Map.unionWith
        (++)
        (Map.map (: []) currentUtxoInHead')
        (Map.map (: []) someUTxOToDecrement')

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
      , utxoInHead = fromList [(A, initialAmount)]
      , decommitUTxOInHead = Map.empty
      , snVersion = 0
      }

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState, latestSnapshot, utxoInHead, snVersion} =
    case headState of
      Open{} ->
        frequency $
          [
            ( 1
            , do
                actor <- elements allActors
                snapshot <- genSnapshot
                pure $ Some $ Close{actor, snapshot}
            )
          ]
            <> [ ( 10
                 , do
                    actor <- elements allActors
                    snapshot <- genSnapshot
                    pure $ Some Decrement{actor, snapshot}
                 )
               | -- XXX: We dont want to generate decrements if there is nothing in the head.
               not (null utxoInHead)
               ]
      Closed{} ->
        oneof $
          [ do
              snapshot <- genSnapshot
              pure $ Some $ Fanout{snapshot}
          ]
            <> [ do
                  actor <- elements allActors
                  snapshot <- genSnapshot
                  pure $ Some Contest{actor, snapshot}
               ]
      Final -> pure $ Some Stop
   where
    genSnapshot = do
      someUTxOToDecrement <- reduceValues =<< genSubModelOf utxoInHead
      let filteredSomeUTxOToDecrement = Map.filter (> 0) someUTxOToDecrement
      let balancedUTxOInHead = balanceUTxOInHead utxoInHead filteredSomeUTxOToDecrement
      let validSnapshot =
            ModelSnapshot
              { snapshotNumber = latestSnapshot
              , snapshotUTxO = balancedUTxOInHead
              , decommitUTxO = filteredSomeUTxOToDecrement
              , snapshotVersion = snVersion
              }
      oneof
        [ -- valid
          pure validSnapshot
        , -- unbalanced
          pure validSnapshot{snapshotUTxO = utxoInHead}
        , do
            -- old
            let snapshotNumber' = if latestSnapshot == 0 then 0 else latestSnapshot - 1
            pure validSnapshot{snapshotNumber = snapshotNumber'}
        , -- new
          pure validSnapshot{snapshotNumber = latestSnapshot + 1}
        , do
            -- shuffled
            someUTxOToDecrement' <- shuffleValues filteredSomeUTxOToDecrement
            pure validSnapshot{decommitUTxO = someUTxOToDecrement'}
        , do
            -- more in head
            utxoInHead' <- increaseValues utxoInHead
            pure validSnapshot{snapshotUTxO = utxoInHead'}
        , do
            -- more in decommit
            someUTxOToDecrement' <- increaseValues =<< genSubModelOf utxoInHead
            let balancedUTxOInHead' = balanceUTxOInHead utxoInHead someUTxOToDecrement'
            pure
              validSnapshot
                { snapshotUTxO = balancedUTxOInHead'
                , decommitUTxO = someUTxOToDecrement'
                }
        , arbitrary
        ]

    genSubModelOf :: ModelUTxO -> Gen ModelUTxO
    genSubModelOf model = do
      subset <- sublistOf (Map.toList model)
      return $ Map.fromList subset

    reduceValues :: ModelUTxO -> Gen ModelUTxO
    reduceValues = Map.traverseWithKey reduceValue
     where
      reduceValue :: SingleUTxO -> Natural -> Gen Natural
      reduceValue _ n = do
        let n' = naturalToInteger n
        reduction <- choose (0, n')
        let reduced = if n' < reduction then 0 else n' - reduction
        return (naturalFromInteger reduced)

    increaseValues :: ModelUTxO -> Gen ModelUTxO
    increaseValues = Map.traverseWithKey (\_ n -> pure (n + naturalFromInteger 1))

    shuffleValues :: ModelUTxO -> Gen ModelUTxO
    shuffleValues utxo = do
      let x = Map.keys utxo
      let y = Map.elems utxo
      x' <- shuffle x
      let shuffledUTxO = Map.fromList $ zip x' y
      pure shuffledUTxO

  -- Determine actions we want to perform and expect to work. If this is False,
  -- validFailingAction is checked too.
  precondition :: Model -> Action Model a -> Bool
  precondition Model{headState, latestSnapshot, alreadyContested, utxoInHead, decommitUTxOInHead} = \case
    Stop -> headState /= Final
    Decrement{snapshot} ->
      headState == Open
        && snapshotNumber snapshot > latestSnapshot
        -- XXX: you are decrementing from existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- XXX: your tx is balanced with the utxo in the head
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        && (not . null $ decommitUTxO snapshot)
    Close{snapshot} ->
      headState == Open
        && ( if snapshotNumber snapshot == 0
              then snapshotUTxO snapshot == initialUTxOInHead
              else snapshotNumber snapshot >= latestSnapshot
           )
        -- XXX: you are decrementing from existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- XXX: your tx is balanced with the utxo in the head
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
     where
      Model{utxoInHead = initialUTxOInHead} = initialState
    Contest{actor, snapshot} ->
      headState == Closed
        && actor `notElem` alreadyContested
        && snapshotNumber snapshot > latestSnapshot
        -- XXX: you are decrementing from existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- XXX: your tx is balanced with the utxo in the head
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
    Fanout{snapshot} ->
      headState == Closed
        && snapshotUTxO snapshot == utxoInHead
        && decommitUTxO snapshot == decommitUTxOInHead

  -- Determine actions we want to perform and want to see failing. If this is
  -- False, the action is discarded (e.g. it's invalid or we don't want to see
  -- it tried to perform).
  validFailingAction :: Model -> Action Model a -> Bool
  validFailingAction Model{headState, utxoInHead, decommitUTxOInHead} = \case
    Stop -> False
    -- Only filter non-matching states as we are not interested in these kind of
    -- verification failures.
    Decrement{snapshot} ->
      headState == Open
        -- XXX: Ignore unbalanced decrements.
        -- TODO: make them fail gracefully and test this?
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        -- XXX: Ignore decrements that work with non existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- XXX: Ignore decrement without something to decommit
        && (not . null $ decommitUTxO snapshot)
    Close{snapshot} ->
      headState == Open
        -- XXX: Ignore unbalanced close.
        -- TODO: make them fail gracefully and test this?
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        -- XXX: Ignore close that work with non existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
    Contest{snapshot} ->
      headState == Closed
        -- XXX: Ignore unbalanced close.
        -- TODO: make them fail gracefully and test this?
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        -- XXX: Ignore close that work with non existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
    Fanout{snapshot} ->
      headState == Closed
        -- XXX: Ignore fanouts which does not preserve the closing head
        && snapshotUTxO snapshot == utxoInHead
        && decommitUTxO snapshot == decommitUTxOInHead

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m t _result =
    case t of
      Stop -> m
      Decrement{snapshot} ->
        m
          { headState = Open
          , latestSnapshot = snapshotNumber snapshot
          , utxoInHead = balanceUTxOInHead (utxoInHead m) (decommitUTxO snapshot)
          , snVersion = snVersion m + 1
          }
      Close{snapshot} ->
        m
          { headState = Closed
          , latestSnapshot = snapshotNumber snapshot
          , alreadyContested = []
          , utxoInHead = snapshotUTxO snapshot
          , decommitUTxOInHead = decommitUTxO snapshot
          }
      Contest{actor, snapshot} ->
        m
          { headState = Closed
          , latestSnapshot = snapshotNumber snapshot
          , alreadyContested = actor : alreadyContested m
          , utxoInHead = snapshotUTxO snapshot
          , decommitUTxOInHead = decommitUTxO snapshot
          }
      Fanout{} -> m{headState = Final}

instance HasVariables Model where
  getAllVariables = mempty

instance HasVariables (Action Model a) where
  getAllVariables = mempty

deriving instance Eq (Action Model a)
deriving instance Show (Action Model a)

-- * ============================== REAL WORLD ==========================

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
  perform Model{snVersion} action _lookupVar = do
    case action of
      Decrement{actor, snapshot} ->
        performTx =<< newDecrementTx actor (signedSnapshot $ snapshot{snapshotVersion = snVersion})
      Close{actor, snapshot} ->
        performTx =<< newCloseTx actor (confirmedSnapshot $ snapshot{snapshotVersion = snVersion})
      Contest{actor, snapshot} ->
        performTx =<< newContestTx actor (confirmedSnapshot $ snapshot{snapshotVersion = snVersion})
      Fanout{snapshot} -> do
        performTx =<< newFanoutTx Alice (snapshot{snapshotVersion = snVersion})
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
          TxResult{constructedTx = Left err} -> fail $ "Failed to construct transaction: " <> err
          TxResult{constructedTx = Right tx} -> do
            -- NOTE: Sort `[TxOut]` by the address and values. We want to make
            -- sure that the fanout outputs match what we had in the open Head
            -- exactly.
            let sorted = sortOn (\o -> (txOutAddress o, selectLovelace (txOutValue o))) . toList
            let fannedOut = utxoFromTx tx
            -- counterexamplePost ("Fanned out UTxO does not match: " <> renderUTxO fannedOut)
            -- counterexamplePost ("SnapshotUTxO: " <> renderUTxO (snapshotUTxO snapshot))
            guard $ sorted fannedOut == sorted (fst $ generateUTxOFromModelSnapshot snapshot)

        expectValid result $ \case
          Tx.Fanout{} -> pure ()
          _ -> fail "Expected Fanout"
      _ -> pure ()

  postconditionOnFailure (modelBefore, _modelAfter) action _lookup result = runPostconditionM' $ do
    counterexample' (show modelBefore)
    counterexample' (show action)
    case action of
      Decrement{} -> either (const fulfilled) expectInvalid result
      Close{} -> either (const fulfilled) expectInvalid result
      Contest{} -> either (const fulfilled) expectInvalid result
      Fanout{} -> either (const fulfilled) expectInvalid result
      _ -> pure ()

-- | Perform a transaction by evaluating and observing it. This updates the
-- 'UTxO' in the 'AppM' if a transaction is valid and produces a 'TxResult' that
-- can be used to assert expected success / failure.
performTx :: Show err => Either err Tx -> AppM TxResult
performTx = \case
  Left err -> do
    utxo <- get
    pure
      TxResult
        { constructedTx = Left $ show err
        , spendableUTxO = utxo
        , validationError = Nothing
        , observation = NoHeadTx
        }
  Right tx -> do
    utxo <- get
    let validationError = getValidationError tx utxo
    when (isNothing validationError) $ do
      put $ adjustUTxO tx utxo
    let observation = observeHeadTx Fixture.testNetworkId utxo tx
    pure
      TxResult
        { constructedTx = Right tx
        , spendableUTxO = utxo
        , validationError
        , observation
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

-- | A "random" UTxO distribution for a given 'ModelSnapshot'.
generateUTxOFromModelSnapshot :: ModelSnapshot -> (UTxO, UTxO)
generateUTxOFromModelSnapshot snapshot =
  ( realWorldModelUTxO (snapshotUTxO snapshot)
  , realWorldModelUTxO (decommitUTxO snapshot)
  )

-- | Map a 'ModelUTxO' to a real-world 'UTxO'.
realWorldModelUTxO :: ModelUTxO -> UTxO
realWorldModelUTxO =
  Map.foldMapWithKey
    ( \k balance ->
        genUTxOWithBalance balance `generateWith` fromEnum k
    )
 where
  genUTxOWithBalance b =
    genUTxO1 (modifyTxOutValue (const $ lovelaceToValue (Coin $ naturalToInteger b)) <$> genTxOut)

-- | A correctly signed snapshot. Given a snapshot number a snapshot signed by
-- all participants (alice, bob and carol) with some UTxO contained is produced.
signedSnapshot :: ModelSnapshot -> (Snapshot Tx, MultiSignature (Snapshot Tx))
signedSnapshot ms =
  (snapshot, signatures)
 where
  (utxo, toDecommit) = generateUTxOFromModelSnapshot ms
  snapshot =
    Snapshot
      { headId = mkHeadId Fixture.testPolicyId
      , number = snapshotNumber ms
      , confirmed = []
      , utxo
      , utxoToDecommit = Just toDecommit
      , version = snapshotVersion ms
      }

  signatures = aggregate [sign sk snapshot | sk <- [Fixture.aliceSk, Fixture.bobSk, Fixture.carolSk]]

-- | A confirmed snapshot (either initial or later confirmed), based onTxTra
-- 'signedSnapshot'.
confirmedSnapshot :: ModelSnapshot -> ConfirmedSnapshot Tx
confirmedSnapshot modelSnapshot@ModelSnapshot{snapshotNumber} =
  case snapshotNumber of
    0 ->
      InitialSnapshot
        { -- -- NOTE: The close validator would not check headId on close with
          -- initial snapshot, but we need to provide it still.
          headId = mkHeadId Fixture.testPolicyId
        , initialUTxO = fst $ generateUTxOFromModelSnapshot modelSnapshot
        }
    _ -> ConfirmedSnapshot{snapshot, signatures}
     where
      (snapshot, signatures) = signedSnapshot modelSnapshot

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
      & modifyTxOutValue (<> foldMap txOutValue inHeadUTxO)

  openHeadDatum =
    mkTxOutDatumInline
      Head.Open
        { parties = partyToChain <$> [Fixture.alice, Fixture.bob, Fixture.carol]
        , utxoHash = toBuiltin $ hashUTxO inHeadUTxO
        , contestationPeriod = CP.toChain Fixture.cperiod
        , headId = headIdToCurrencySymbol $ mkHeadId Fixture.testPolicyId
        , snapshotNumber = 0
        , version = 0
        }

  inHeadUTxO = realWorldModelUTxO (utxoInHead initialState)

-- | Creates a decrement transaction using given utxo and given snapshot.
newDecrementTx :: Actor -> (Snapshot Tx, MultiSignature (Snapshot Tx)) -> AppM (Either DecrementTxError Tx)
newDecrementTx actor (snapshot, signatures) = do
  spendableUTxO <- get
  pure $
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
newCloseTx :: Actor -> ConfirmedSnapshot Tx -> AppM (Either CloseTxError Tx)
newCloseTx actor snapshot = do
  spendableUTxO <- get
  pure $
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
newContestTx :: Actor -> ConfirmedSnapshot Tx -> AppM (Either ContestTxError Tx)
newContestTx actor snapshot = do
  spendableUTxO <- get
  pure $
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
expectValid TxResult{validationError = Just err} _ = do
  counterexample' "Expected to pass validation"
  fail err
expectValid TxResult{observation} fn = do
  counterexample' $ "Wrong observation: " <> show observation
  fn observation

-- | Assertion helper to check whether a 'TxResult' was invalid.
expectInvalid :: Monad m => TxResult -> PostconditionM' m ()
expectInvalid = \case
  TxResult{validationError = Nothing, constructedTx, spendableUTxO} -> do
    counterexample' "Expected tx to fail validation"
    case constructedTx of
      Left err -> counterexample' $ "But construction failed with:" <> err
      Right tx -> do
        counterexample' $ renderTxWithUTxO spendableUTxO tx
    fail "But it did not fail"
  _ -> pure ()

-- | Generate sometimes a value with given generator, bur more often just use
-- the given value.
orArbitrary :: a -> Gen a -> Gen a
orArbitrary a gen = frequency [(1, pure a), (2, gen)]
