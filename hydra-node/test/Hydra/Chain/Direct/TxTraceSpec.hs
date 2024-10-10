{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Stateful model-based testing of the transactions created by the "Direct"
-- chain modules.
--
-- The model is focusing on transitions between Open and Closed states of the
-- head.
--
-- TODO: implement it this way
-- Given an initial UTxO, the model generates a plausible sequence of snapshots
-- that an honest node would approve. That is, the total balance of UTxO remains
-- constant and utxoToDecommit is only allowed to clear if the version is
-- incremented. All snapshots are correctly signed and UTxOs are simplified such
-- that their identity is A-E and value is just a number.
--
-- From this sequence of valid snapshots, possible Decrement and Close actions
-- are generated, along with occasional Contest and consequential Fanout.
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
  PaymentKey,
  SlotNo (..),
  VerificationKey,
  lovelaceToValue,
  mkTxOutDatumInline,
  modifyTxOutValue,
  selectLovelace,
  throwError,
  txOutAddress,
  txOutValue,
 )
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.State (
  ChainContext (..),
  CloseTxError,
  ContestTxError,
  DecrementTxError,
  FanoutTxError,
  close,
  contest,
  decrement,
  fanout,
 )
import Hydra.Chain.Direct.Tx (
  HeadObservation (NoHeadTx),
  observeHeadTx,
 )
import Hydra.Chain.Direct.Tx qualified as Tx
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano (Tx, adjustUTxO)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Crypto (MultiSignature, aggregate, sign)
import Hydra.Tx.HeadId (headIdToCurrencySymbol, mkHeadId)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (hashUTxO, utxoFromTx)
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, registryUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber (..), SnapshotVersion (..), number)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Tx.Fixture (alice, bob, carol, testNetworkId)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genForParty,
  genScriptRegistry,
  genTxOut,
  genUTxO1,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (addParticipationTokens)
import Test.QuickCheck (Property, Smart (..), Testable, checkCoverage, choose, cover, elements, frequency, ioProperty, oneof, shuffle, sublistOf, (===))
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
  prop "realWorldModelUTxO preserves addition" $ \u1 u2 ->
    realWorldModelUTxO (u1 <> u2) === (realWorldModelUTxO u1 <> realWorldModelUTxO u2)
  prop "generates interesting transaction traces" $ \actions ->
    checkCoverage $ coversInterestingActions actions True
  prop "all valid transactions" prop_runActions

coversInterestingActions :: Testable p => Actions Model -> p -> Property
coversInterestingActions (Actions_ _ (Smart _ steps)) p =
  p
    & cover 1 (null steps) "empty"
    & cover 50 (hasSomeSnapshots steps) "has some snapshots"
    & cover 5 (hasDecrement steps) "has decrements"
    & cover 1 (countContests steps >= 2) "has multiple contests"
    & cover 5 (closeNonInitial steps) "close with non initial snapshots"
    & cover 10 (hasFanout steps) "reach fanout"
    & cover 0.5 (fanoutWithEmptyUTxO steps) "fanout with empty UTxO"
    & cover 1 (fanoutWithSomeUTxO steps) "fanout with some UTxO"
    & cover 0.5 (fanoutWithDelta steps) "fanout with additional UTxO to distribute"
 where
  hasSomeSnapshots =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        NewSnapshot{} ->
          polarity == PosPolarity
        _ -> False

  hasUTxOToDecommit snapshot = not . null $ decommitUTxO snapshot

  hasFanout =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{} -> polarity == PosPolarity
        _ -> False

  fanoutWithEmptyUTxO =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{utxo} ->
          polarity == PosPolarity
            && null utxo
        _ -> False

  fanoutWithSomeUTxO =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{utxo} ->
          polarity == PosPolarity
            && not (null utxo)
        _ -> False

  fanoutWithDelta =
    any $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Fanout{deltaUTxO} ->
          polarity == PosPolarity
            && not (null deltaUTxO)
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
    all $
      \(_ := ActionWithPolarity{polarAction, polarity}) -> case polarAction of
        Decrement{snapshot} ->
          polarity == PosPolarity
            && hasUTxOToDecommit snapshot
        _ -> False

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  coversInterestingActions actions
    . monadic runAppMProperty
    $ do
      print actions
      void (runActions actions)
 where
  runAppMProperty :: AppM Property -> Property
  runAppMProperty action = ioProperty $ do
    localState <- newIORef openHeadUTxO
    runReaderT (runAppM action) localState

-- * ============================== MODEL WORLD ==========================

data SingleUTxO = A | B | C | D | E
  deriving (Show, Eq, Ord, Enum, Generic)

instance Arbitrary SingleUTxO where
  arbitrary = genericArbitrary
  shrink = genericShrink

type ModelUTxO = Map SingleUTxO Natural

data Model = Model
  { headState :: State
  , knownSnapshots :: [ModelSnapshot]
  -- ^ List of off-chain snapshots, from most recent to oldest.
  , currentVersion :: SnapshotVersion
  , closedSnapshotNumber :: SnapshotNumber
  , alreadyContested :: [Actor]
  , utxoInHead :: ModelUTxO
  , pendingDecommitUTxO :: ModelUTxO
  }
  deriving (Show)

latestSnapshotNumber :: [ModelSnapshot] -> SnapshotNumber
latestSnapshotNumber = \case
  (s : _) -> s.number
  _ -> 0

-- | Model of a real snapshot which contains a 'SnapshotNumber` but also our
-- simplified form of 'UTxO'.
data ModelSnapshot = ModelSnapshot
  { version :: SnapshotVersion
  , number :: SnapshotNumber
  , snapshotUTxO :: ModelUTxO
  , decommitUTxO :: ModelUTxO
  }
  deriving (Show, Eq, Ord, Generic)

instance Num ModelSnapshot where
  _ + _ = error "undefined"
  _ - _ = error "undefined"
  _ * _ = error "undefined"
  abs _ = error "undefined"
  signum _ = error "undefined"
  fromInteger x =
    ModelSnapshot
      { version = UnsafeSnapshotVersion 0
      , number = UnsafeSnapshotNumber $ fromMaybe 0 $ integerToNatural x
      , snapshotUTxO = mempty
      , decommitUTxO = mempty
      }

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

balanceUTxOInHead :: Ord k => Map k Natural -> Map k Natural -> Map k Natural
balanceUTxOInHead currentUtxoInHead someUTxOToDecrement =
  currentUtxoInHead `Map.difference` someUTxOToDecrement

instance StateModel Model where
  data Action Model a where
    NewSnapshot :: {newSnapshot :: ModelSnapshot} -> Action Model ()
    Decrement :: {actor :: Actor, snapshot :: ModelSnapshot} -> Action Model TxResult
    Close :: {actor :: Actor, snapshot :: ModelSnapshot} -> Action Model TxResult
    Contest :: {actor :: Actor, snapshot :: ModelSnapshot} -> Action Model TxResult
    Fanout :: {utxo :: ModelUTxO, deltaUTxO :: ModelUTxO} -> Action Model TxResult
    -- \| Helper action to identify the terminal state 'Final' and shorten
    -- traces using the 'precondition'.
    Stop :: Action Model ()

  initialState =
    Model
      { headState = Open
      , knownSnapshots = []
      , currentVersion = 0
      , closedSnapshotNumber = 0
      , alreadyContested = []
      , utxoInHead = fromList $ map (,10) [A, B, C]
      , pendingDecommitUTxO = Map.empty
      }

  -- FIXME: 1.5k discards on 100 runs

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState, knownSnapshots, currentVersion, utxoInHead, pendingDecommitUTxO} =
    case headState of
      Open{} ->
        oneof $
          [Some . NewSnapshot <$> genSnapshot]
            <> [ do
                  actor <- elements allActors
                  snapshot <- elements knownSnapshots
                  pure $ Some Decrement{actor, snapshot}
               | not (null knownSnapshots) -- XXX: DRY this check
               ]
            <> [ do
                  actor <- elements allActors
                  snapshot <- elements knownSnapshots
                  pure $ Some $ Close{actor, snapshot = snapshot}
               | not (null knownSnapshots)
               ]
      Closed{} ->
        frequency $
          [
            ( 1
            , do
                -- Fanout with the currently known model state.
                pure $
                  Some $
                    Fanout
                      { utxo = utxoInHead
                      , deltaUTxO = pendingDecommitUTxO
                      }
            )
          ]
            <> [ ( 10
                 , do
                    actor <- elements allActors
                    snapshot <- elements knownSnapshots
                    pure $ Some Contest{actor, snapshot}
                 )
               | not (null knownSnapshots)
               ]
      Final -> pure $ Some Stop
   where
    -- TODO: Generate a snapshot an honest node would sign given the current model state.
    genSnapshot = do
      someUTxOToDecrement <- reduceValues =<< genSubModelOf utxoInHead
      let filteredSomeUTxOToDecrement = Map.filter (> 0) someUTxOToDecrement
      let balancedUTxOInHead = balanceUTxOInHead utxoInHead filteredSomeUTxOToDecrement

      let validSnapshot =
            ModelSnapshot
              { version = currentVersion
              , number = latestSnapshotNumber knownSnapshots + 1
              , snapshotUTxO = balancedUTxOInHead
              , decommitUTxO = filteredSomeUTxOToDecrement
              }
      -- TODO: check whether these cases are met
      -- oneof
      --   [ -- valid
      --     pure validSnapshot
      --   , -- unbalanced
      --     pure validSnapshot{snapshotUTxO = utxoInHead}
      --   , do
      --       -- old
      --       let number' = if closedSnapshotNumber == 0 then 0 else closedSnapshotNumber - 1
      --       pure (validSnapshot :: ModelSnapshot){number = number'}
      --   , -- new
      --     pure (validSnapshot :: ModelSnapshot){number = closedSnapshotNumber + 1}
      --   , do
      --       -- shuffled
      --       someUTxOToDecrement' <- shuffleValues filteredSomeUTxOToDecrement
      --       pure validSnapshot{decommitUTxO = someUTxOToDecrement'}
      --   , do
      --       -- more in head
      --       utxoInHead' <- increaseValues utxoInHead
      --       pure validSnapshot{snapshotUTxO = utxoInHead'}
      --   , do
      --       -- more in decommit
      --       someUTxOToDecrement' <- increaseValues =<< genSubModelOf utxoInHead
      --       let balancedUTxOInHead' = balanceUTxOInHead utxoInHead someUTxOToDecrement'
      --       pure
      --         validSnapshot
      --           { snapshotUTxO = balancedUTxOInHead'
      --           , decommitUTxO = someUTxOToDecrement'
      --           }
      --   , -- decommit all
      --     pure validSnapshot{snapshotUTxO = mempty, decommitUTxO = utxoInHead}
      --   , arbitrary
      --   ]
      pure validSnapshot

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
  precondition Model{headState, knownSnapshots, closedSnapshotNumber, alreadyContested, utxoInHead, pendingDecommitUTxO, currentVersion} = \case
    Stop -> headState /= Final
    NewSnapshot{newSnapshot} ->
      newSnapshot.version == currentVersion
        && newSnapshot.number > latestSnapshotNumber knownSnapshots
    Decrement{snapshot} ->
      headState == Open
        && snapshot.version == currentVersion
        -- you are decrementing from existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- your tx is balanced with the utxo in the head
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
    Close{snapshot} ->
      headState == Open
        && ( if snapshot.number == 0
              then snapshotUTxO snapshot == initialUTxOInHead
              else
                snapshot.version `elem` (currentVersion : [currentVersion - 1 | currentVersion > 0])
           )
        -- you are decrementing from existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- your tx is balanced with the utxo in the head
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
     where
      Model{utxoInHead = initialUTxOInHead} = initialState
    Contest{actor, snapshot} ->
      headState == Closed
        && actor `notElem` alreadyContested
        && snapshot.version `elem` (currentVersion : [currentVersion - 1 | currentVersion > 0])
        && snapshot.number > closedSnapshotNumber
        -- you are decrementing from existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
        -- your tx is balanced with the utxo in the head
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
    Fanout{utxo, deltaUTxO} ->
      headState == Closed
        && utxo == utxoInHead
        && deltaUTxO == pendingDecommitUTxO

  -- Determine actions we want to perform and want to see failing. If this is
  -- False, the action is discarded (e.g. it's invalid or we don't want to see
  -- it tried to perform).
  validFailingAction :: Model -> Action Model a -> Bool
  validFailingAction Model{headState, utxoInHead, currentVersion} = \case
    Stop -> False
    NewSnapshot{} -> False
    -- Only filter non-matching states as we are not interested in these kind of
    -- verification failures.
    Decrement{snapshot} ->
      headState == Open
        && snapshot.version /= currentVersion
        -- Ignore unbalanced decrements.
        -- TODO: make them fail gracefully and test this?
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        -- Ignore decrements that work with non existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
    Close{snapshot} ->
      headState == Open
        && ( snapshot.number == 0
              || snapshot.version `elem` (currentVersion : [currentVersion - 1 | currentVersion > 0])
           )
        -- Ignore unbalanced close.
        -- TODO: make them fail gracefully and test this?
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        -- Ignore close that work with non existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
    Contest{snapshot} ->
      headState == Closed
        -- Ignore unbalanced close.
        -- TODO: make them fail gracefully and test this?
        && sum (decommitUTxO snapshot) + sum (snapshotUTxO snapshot) == sum utxoInHead
        -- Ignore close that work with non existing utxo in the head
        && all (`elem` Map.keys utxoInHead) (Map.keys (decommitUTxO snapshot) <> Map.keys (snapshotUTxO snapshot))
    Fanout{} ->
      headState == Closed

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m@Model{currentVersion} t _result =
    case t of
      Stop -> m
      NewSnapshot{newSnapshot} ->
        m{knownSnapshots = newSnapshot : m.knownSnapshots}
      Decrement{snapshot} ->
        m
          { headState = Open
          , currentVersion = m.currentVersion + 1
          , utxoInHead = balanceUTxOInHead (utxoInHead m) (decommitUTxO snapshot)
          }
      Close{snapshot} ->
        m
          { headState = Closed
          , closedSnapshotNumber = snapshot.number
          , alreadyContested = []
          , utxoInHead = snapshotUTxO snapshot
          , pendingDecommitUTxO = if currentVersion == snapshot.version then decommitUTxO snapshot else mempty
          }
      Contest{actor, snapshot} ->
        m
          { headState = Closed
          , closedSnapshotNumber = snapshot.number
          , alreadyContested = actor : alreadyContested m
          , utxoInHead = snapshotUTxO snapshot
          , pendingDecommitUTxO = decommitUTxO snapshot
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

-- XXX: Most of the heavy-lifting here is similar to what
-- quickcheck-contractmodel does. We could consider using that package and
-- define a corresponding RunModel using our tx construction / evaluation hooks
-- only.
instance RunModel Model AppM where
  perform Model{currentVersion} action _lookupVar = do
    case action of
      Decrement{actor, snapshot} -> do
        let (s, signatures) = signedSnapshot snapshot
        tx <- newDecrementTx actor ConfirmedSnapshot{snapshot = s, signatures}
        performTx tx
      Close{actor, snapshot} -> do
        tx <- newCloseTx actor currentVersion (confirmedSnapshot snapshot)
        performTx tx
      Contest{actor, snapshot} -> do
        tx <- newContestTx actor currentVersion (confirmedSnapshot snapshot)
        performTx tx
      Fanout{utxo, deltaUTxO} -> do
        tx <- newFanoutTx Alice utxo deltaUTxO
        performTx tx
      NewSnapshot{} -> pure ()
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
      Fanout{utxo, deltaUTxO} -> do
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
            guard $ sorted fannedOut == sorted (realWorldModelUTxO utxo <> realWorldModelUTxO deltaUTxO)

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
performTx result =
  case result of
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
      , version = ms.version
      , number = ms.number
      , confirmed = []
      , utxo
      , utxoToCommit = Nothing
      , utxoToDecommit = Just toDecommit
      }

  signatures = aggregate [sign sk snapshot | sk <- [Fixture.aliceSk, Fixture.bobSk, Fixture.carolSk]]

-- | A confirmed snapshot (either initial or later confirmed), based onTxTra
-- 'signedSnapshot'.
confirmedSnapshot :: ModelSnapshot -> ConfirmedSnapshot Tx
confirmedSnapshot modelSnapshot@ModelSnapshot{number} =
  case number of
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
      & addParticipationTokens [alicePVk, bobPVk, carolPVk]
      & modifyTxOutValue (<> foldMap txOutValue inHeadUTxO)

  openHeadDatum =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { parties = partyToChain <$> [Fixture.alice, Fixture.bob, Fixture.carol]
          , utxoHash = toBuiltin $ hashUTxO inHeadUTxO
          , contestationPeriod = CP.toChain Fixture.cperiod
          , headId = headIdToCurrencySymbol $ mkHeadId Fixture.testPolicyId
          , version = 0
          }

  inHeadUTxO = realWorldModelUTxO (utxoInHead initialState)

-- | Creates a decrement transaction using given utxo and given snapshot.
newDecrementTx :: Actor -> ConfirmedSnapshot Tx -> AppM (Either DecrementTxError Tx)
newDecrementTx actor snapshot = do
  spendableUTxO <- get
  pure $
    decrement
      (actorChainContext actor)
      spendableUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.testHeadParameters
      snapshot

-- | Creates a transaction that closes 'openHeadUTxO' with given the snapshot.
-- NOTE: This uses fixtures for headId, parties (alice, bob, carol),
-- contestation period and also claims to close at time 0 resulting in a
-- contestation deadline of 0 + cperiod.
newCloseTx :: Actor -> SnapshotVersion -> ConfirmedSnapshot Tx -> AppM (Either CloseTxError Tx)
newCloseTx actor openVersion snapshot = do
  spendableUTxO <- get
  pure $
    close
      (actorChainContext actor)
      spendableUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.testHeadParameters
      openVersion
      snapshot
      lowerBound
      upperBound
 where
  lowerBound = 0

  upperBound = (0, posixSecondsToUTCTime 0)

-- | Creates a contest transaction using given utxo and contesting with given
-- snapshot. NOTE: This uses fixtures for headId, contestation period and also
-- claims to contest at time 0.
newContestTx :: Actor -> SnapshotVersion -> ConfirmedSnapshot Tx -> AppM (Either ContestTxError Tx)
newContestTx actor openVersion snapshot = do
  spendableUTxO <- get
  pure $
    contest
      (actorChainContext actor)
      spendableUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.cperiod
      openVersion
      snapshot
      currentTime
 where
  currentTime = (0, posixSecondsToUTCTime 0)

-- | Creates a fanout transaction using given utxo. NOTE: This uses fixtures for
-- seedTxIn and contestation period. Consequently, the lower bound used is
-- precisely at the maximum deadline slot as if everyone contested.
newFanoutTx :: Actor -> ModelUTxO -> ModelUTxO -> AppM (Either FanoutTxError Tx)
newFanoutTx actor utxo deltaUTxO = do
  spendableUTxO <- get
  pure $
    fanout
      (actorChainContext actor)
      spendableUTxO
      Fixture.testSeedInput
      (realWorldModelUTxO utxo)
      -- Model world has no 'Maybe ModelUTxO', but real world does.
      (if null deltaUTxO then Nothing else Just $ realWorldModelUTxO deltaUTxO)
      deadline
 where
  CP.UnsafeContestationPeriod contestationPeriod = Fixture.cperiod
  deadline = SlotNo $ fromIntegral contestationPeriod * fromIntegral (length allActors)

-- | Cardano payment keys for 'alice', 'bob', and 'carol'.
alicePVk, bobPVk, carolPVk :: VerificationKey PaymentKey
alicePVk = genVerificationKey `genForParty` alice
bobPVk = genVerificationKey `genForParty` bob
carolPVk = genVerificationKey `genForParty` carol

-- | Fixture for the chain context of a model 'Actor' on 'testNetworkId'. Uses a generated 'ScriptRegistry'.
actorChainContext :: Actor -> ChainContext
actorChainContext actor =
  ChainContext
    { networkId = testNetworkId
    , ownVerificationKey =
        case actor of
          Alice -> alicePVk
          Bob -> bobPVk
          Carol -> carolPVk
    , ownParty =
        case actor of
          Alice -> alice
          Bob -> bob
          Carol -> carol
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
expectValid TxResult{observation, constructedTx, spendableUTxO} fn = do
  case constructedTx of
    Left err -> counterexample' $ "But construction failed with: " <> err
    Right tx -> do
      counterexample' $ renderTxWithUTxO spendableUTxO tx
  counterexample' $ "Wrong observation: " <> show observation
  fn observation

-- | Assertion helper to check whether a 'TxResult' was invalid.
expectInvalid :: Monad m => TxResult -> PostconditionM' m ()
expectInvalid = \case
  TxResult{validationError = Nothing, constructedTx, spendableUTxO} -> do
    counterexample' "Expected tx to fail validation"
    case constructedTx of
      Left err -> counterexample' $ "But construction failed with: " <> err
      Right tx -> do
        counterexample' $ renderTxWithUTxO spendableUTxO tx
    fail "But it did not fail"
  _ -> pure ()

-- | Generate sometimes a value with given generator, bur more often just use
-- the given value.
orSometimes :: a -> Gen a -> Gen a
orSometimes a gen = frequency [(1, pure a), (2, gen)]
