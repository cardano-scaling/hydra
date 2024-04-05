module Hydra.Chain.Direct.TxTraceSpec where

import Hydra.Prelude hiding (Any, State, label, show)
import Test.Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (mkTxOutDatumInline)
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.Contract.Mutation (addParticipationTokens)
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), close, contest)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol, mkHeadId, mkHeadOutput, observeHeadTx)
import Hydra.Chain.Direct.Tx qualified as Tx
import Hydra.ContestationPeriod qualified as CP
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (aggregate, sign)
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (Tx, adjustUTxO, genUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Party (partyToChain)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, number)
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Fixture (genForParty)
import Test.Hydra.Fixture qualified as Fixture
import Test.QuickCheck (Property, Smart (..), checkCoverage, cover, elements, forAll, oneof, resize)
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
  mkVar,
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
        & cover 5 (countContests steps >= 2) "has multiple contests"
        & cover 5 (containSomeSnapshots steps) "has some snapshots"
        & cover 5 (closeNonInitial steps) "close with non initial snapshots"
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
    any $
      \(_ := ActionWithPolarity{polarAction}) -> case polarAction of
        Close snapshotNumber -> snapshotNumber > 0
        _ -> False

prop_runActions :: Actions Model -> Property
prop_runActions actions =
  monadicIO $ do
    print actions
    void (runActions actions)

-- * Model

data Model = Model
  { snapshots :: [SnapshotNumber]
  , headState :: State
  , utxoV :: Var UTxO
  -- ^ Last known, spendable UTxO.
  }
  deriving (Show)

data State
  = Open
  | Closed
  | Final
  deriving (Show, Eq)

instance StateModel Model where
  data Action Model a where
    ProduceSnapshots :: [SnapshotNumber] -> Action Model ()
    Close :: SnapshotNumber -> Action Model UTxO
    Contest :: SnapshotNumber -> Action Model ()
    Fanout :: Action Model ()
    -- \| Helper action to identify the terminal state 'Final' and shorten
    -- traces using the 'precondition'.
    Stop :: Action Model ()

  arbitraryAction :: VarContext -> Model -> Gen (Any (Action Model))
  arbitraryAction _lookup Model{headState, snapshots} =
    case headState of
      Open ->
        oneof
          [ -- TODO: non-continuous snapshot numbers
            Some . ProduceSnapshots <$> arbitrary
          , Some . Close <$> elements (0 : snapshots)
          ]
      Closed ->
        oneof
          [ -- NOTE: Adding 0 to have elements always pass, but precondition
            -- would disallow contest with 0.
            Some . Contest <$> elements (0 : snapshots) -- FIXME: needs to depend on currently closed snapshot
          , pure $ Some Fanout
          ]
      Final -> pure $ Some Stop

  -- TODO: shrinkAction to have small snapshots?

  initialState =
    Model
      { snapshots = []
      , headState = Open
      , utxoV = mkVar 1 -- TODO: what does '1' mean here?
      }

  nextState :: Model -> Action Model a -> Var a -> Model
  nextState m Stop _ = m
  nextState m t result =
    case t of
      ProduceSnapshots snapshots -> m{snapshots = snapshots}
      Close{} -> m{headState = Closed, utxoV = result}
      Contest{} -> m{headState = Closed}
      Fanout -> m{headState = Final}

  precondition :: Model -> Action Model a -> Bool
  precondition Model{headState = Final} Stop = False
  precondition Model{headState} (Contest sn) = headState == Closed && sn /= 0
  precondition _ _ = True

instance HasVariables Model where
  getAllVariables = mempty

instance HasVariables (Action Model a) where
  getAllVariables = mempty

deriving instance Eq (Action Model a)
deriving instance Show (Action Model a)

instance RunModel Model IO where
  perform :: Model -> Action Model a -> LookUp IO -> IO a
  perform Model{utxoV} action lookupVar = do
    putStrLn $ "performing action: " <> show action

    case action of
      ProduceSnapshots _snapshots -> pure ()
      Close snapshotNumber -> do
        tx <- newCloseTx $ correctlySignedSnapshot snapshotNumber
        validateTx openHeadUTxO tx
        case observeHeadTx Fixture.testNetworkId openHeadUTxO tx of
          Tx.Close{} -> pure () -- TODO: check more things here (or in postcondition)?
          observation -> failure $ "Expected Close observation, but got " <> show observation

        pure $ adjustUTxO tx openHeadUTxO
      Contest snapshotNumber -> do
        -- NOTE: Should not happen anymore
        when (snapshotNumber == 0) $
          failure "Cannot contest initial snapshot"

        let utxo = lookupVar utxoV
        tx <- newContestTx utxo $ correctlySignedSnapshot snapshotNumber
        validateTx utxo tx
        pure ()
      Fanout -> pure ()
      Stop -> pure ()

-- * Fixtures and glue code

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
correctlySignedSnapshot :: SnapshotNumber -> ConfirmedSnapshot Tx
correctlySignedSnapshot = \case
  0 ->
    InitialSnapshot
      { -- -- NOTE: The close validator would not check headId on close with
        -- initial snapshot, but we need to provide it still.
        headId = mkHeadId Fixture.testPolicyId
      , initialUTxO = snapshotUTxO 0
      }
  number -> ConfirmedSnapshot{snapshot, signatures}
   where
    snapshot =
      Snapshot
        { headId = mkHeadId Fixture.testPolicyId
        , number
        , utxo = snapshotUTxO number
        , confirmed = []
        }

    signatures = aggregate [sign sk snapshot | sk <- [Fixture.aliceSk, Fixture.bobSk, Fixture.carolSk]]

-- | UTxO of the open head on-chain.
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
        }

-- Re-use Direct.State-level functions with fixtures for the time being.
newCloseTx :: HasCallStack => ConfirmedSnapshot Tx -> IO Tx
newCloseTx snapshot =
  either (failure . show) pure $
    close
      aliceChainContext
      openHeadUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.testHeadParameters
      snapshot
      lowerBound
      upperBound
 where
  lowerBound = 0

  upperBound = (0, posixSecondsToUTCTime 0)

newContestTx :: HasCallStack => UTxO -> ConfirmedSnapshot Tx -> IO Tx
newContestTx spendableUTxO snapshot =
  either (failure . show) pure $
    contest
      aliceChainContext
      spendableUTxO
      (mkHeadId Fixture.testPolicyId)
      Fixture.cperiod
      snapshot
      currentTime
 where
  currentTime = (0, posixSecondsToUTCTime 0)

-- | Fixture for the chain context of 'alice' on 'testNetworkId'. Uses a generated 'ScriptRegistry'.
-- TODO: move to Hydra.Chain.Direct.Fixture / into a testlib
aliceChainContext :: ChainContext
aliceChainContext =
  ChainContext
    { networkId = Fixture.testNetworkId
    , ownVerificationKey = Fixture.alicePVk
    , ownParty = Fixture.alice
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
