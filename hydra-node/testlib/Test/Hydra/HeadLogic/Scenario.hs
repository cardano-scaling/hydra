{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Explicit fixtures, scheduling inputs and the versioned replay document for
-- the bounded Simple-ledger snapshot lab. No exploration or rule execution.
module Test.Hydra.HeadLogic.Scenario where

import Data.Aeson (defaultOptions, genericParseJSON, withObject, (.:))
import Data.List ((!!))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    finalizedCommit,
    finalizedDecommit,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  HeadState (Open),
  OpenState (..),
  SeenSnapshot (NoSeenSnapshot, SeenSnapshot, snapshot),
 )
import Hydra.Ledger.Simple (SimpleTx (SimpleTx))
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state)
import Hydra.Tx (txId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (aggregateInOrder, sign)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.Node.Fixture (
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
  cperiod,
  dperiod,
  testEnvironment,
  testHeadId,
  testHeadSeed,
 )

-- | There is one optional duplicate, one optional delay/release, and (only in
-- the fault demonstration) one corrupted acknowledgement per path.
data Delivery = Delivery
  { senderIndex :: Int
  , recipientIndex :: Int
  , remainingTTL :: TTL
  , message :: Message SimpleTx
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR Delivery where toCBOR = genericToCBOR

data Action = Deliver Delivery | Retry Delivery | Delay Delivery | Release Delivery | Duplicate Delivery | Corrupt Delivery
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data World = World
  { nodes :: [NodeState SimpleTx]
  , pending :: [Delivery]
  , retries :: [Delivery]
  , delayed :: [Delivery]
  , duplicateUsed :: Bool
  , delayUsed :: Bool
  , corruptionUsed :: Bool
  , confirmations :: [Snapshot SimpleTx]
  , originSnapshot :: Snapshot SimpleTx
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance ToCBOR World where toCBOR = genericToCBOR

data Limits = Limits {maxDeliveries :: Int, maxStates :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultLimits :: Limits
defaultLimits = Limits 24 100000

data SearchReport = SearchReport
  { limits :: Limits
  , exploredStates :: Int
  , depthCutoffs :: Int
  , truncated :: Bool
  , failure :: Maybe Text
  , path :: [Action]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

scenarioParties :: [Party]
scenarioParties = [alice, bob, carol]

environment :: Int -> Environment
environment n = testEnvironment{party = scenarioParties !! n, signingKey = [aliceSk, bobSk, carolSk] !! n}

transactionOne, transactionTwo :: SimpleTx
transactionOne = SimpleTx 1 (Set.singleton 1) (Set.singleton 2)
transactionTwo = SimpleTx 2 (Set.singleton 2) (Set.singleton 3)

initialSnapshot :: Snapshot SimpleTx
initialSnapshot =
  Snapshot
    { headId = testHeadId
    , version = 0
    , number = 0
    , confirmed = []
    , utxo = Set.singleton 1
    , utxoToCommit = Nothing
    , depositTxId = Nothing
    , utxoToDecommit = Nothing
    , accumulator = Accumulator.buildFromUTxO @SimpleTx (Set.singleton 1)
    , appliedAccumulator = Accumulator.buildFromUTxO @SimpleTx (Set.singleton 1)
    }

initialNode :: NodeState SimpleTx
initialNode =
  NodeInSync
    { headState =
        Open
          OpenState
            { parameters = HeadParameters cperiod dperiod scenarioParties
            , chainState = 0
            , headId = testHeadId
            , headSeed = testHeadSeed
            , coordinatedHeadState =
                CoordinatedHeadState
                  { localUTxO = Set.singleton 1
                  , localTxs = mempty
                  , allTxs = mempty
                  , confirmedSnapshot = ConfirmedSnapshot initialSnapshot (aggregateInOrder (Map.fromList [(p, sign sk initialSnapshot) | (p, sk) <- zip scenarioParties [aliceSk, bobSk, carolSk]]) scenarioParties)
                  , seenSnapshot = NoSeenSnapshot
                  , currentDepositTxId = Nothing
                  , decommitTx = Nothing
                  , version = 0
                  , finalizedCommit = Nothing
                  , finalizedDecommit = Nothing
                  }
            }
    , deposits = mempty
    , chainPointTime = ChainPointTime 0 initialChainTime 0
    }

initialWorld :: World
initialWorld = legacyInitialWorldV1

-- | Frozen v1 seed; do not change this fixture when adding scenario families.
legacyInitialWorldV1 :: World
legacyInitialWorldV1 =
  World
    (replicate 3 initialNode)
    [Delivery 0 recipient 2 (ReqTx tx) | tx <- [transactionOne, transactionTwo], recipient <- [0 .. 2]]
    []
    []
    False
    False
    False
    []
    initialSnapshot

openState :: NodeState SimpleTx -> OpenState SimpleTx
openState st = case st.headState of
  Open os -> os
  _ -> error "explorer scenario leaves the open phase"

data ExecutionMode = VerifySignatures | BypassVerification
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Disturbances = ValidMessagesOnly | AllowInvalidAcknowledgement
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SearchOrder = DeliveriesFirst | RetriesFirst | BreadthFirst | RetryWindows
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ScenarioFamily = DependentTransactions | DuplicateTransaction | DelayedDependency
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Exploration = Exploration
  { family :: ScenarioFamily
  , searchOrder :: SearchOrder
  , executionMode :: ExecutionMode
  , disturbances :: Disturbances
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultExploration :: Exploration
defaultExploration = Exploration DependentTransactions DeliveriesFirst VerifySignatures ValidMessagesOnly

initialFor :: ScenarioFamily -> World
initialFor = \case
  DependentTransactions -> initialWorld
  DuplicateTransaction -> initialWorld{pending = [Delivery 0 recipient 2 (ReqTx transactionOne) | recipient <- [0 .. 2]]}
  DelayedDependency -> initialWorld{pending = [Delivery 0 recipient 2 (ReqTx tx) | tx <- [transactionTwo, transactionOne], recipient <- [0 .. 2]]}

-- | Version 2 records its entire starting state and separates the execution
-- mode from the input schedule. Version 1 explicitly uses the immutable v1 seed.
data Scenario = Scenario
  { schemaVersion :: Int
  , exploration :: Exploration
  , startingWorld :: World
  , scenarioLimits :: Limits
  , schedule :: [Action]
  , primitiveTapes :: [[Debug.QueryRecord]]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Scenario where
  parseJSON value =
    withObject
      "HeadLogic scenario"
      ( \o -> do
          version <- o .: "schemaVersion"
          case version :: Int of
            1 -> do
              bypass <- o .: "faultEnabled"
              let mode = if bypass then BypassVerification else VerifySignatures
              Scenario 2 defaultExploration{executionMode = mode, disturbances = AllowInvalidAcknowledgement} legacyInitialWorldV1
                <$> o .: "scenarioLimits"
                <*> o .: "schedule"
                <*> o .: "primitiveTapes"
            2 -> genericParseJSON defaultOptions value
            _ -> fail "unsupported scenario version"
      )
      value

-- | The lab supports three known signing identities, an open head, and no
-- settlement actions. Reject unsupported files before indexing or executing.
validateWorld :: World -> Either Text ()
validateWorld world = do
  unless (length world.nodes == length scenarioParties) $ Left "scenario needs three nodes"
  forM_ world.nodes $ \node -> case node.headState of
    Open os -> do
      unless (os.parameters.parties == scenarioParties) $ Left "unsupported scenario signing identities"
      unless (os.headId == world.originSnapshot.headId) $ Left "scenario head IDs differ"
      let c = os.coordinatedHeadState
      unless (null node.deposits && isNothing c.currentDepositTxId && isNothing c.decommitTx && isNothing c.finalizedCommit && isNothing c.finalizedDecommit) $ Left "scenario excludes settlement actions"
      unless (c.version == world.originSnapshot.version) $ Left "scenario excludes version transitions"
      validateSnapshot world.originSnapshot
      validateSnapshot $ getSnapshot c.confirmedSnapshot
      case c.seenSnapshot of
        SeenSnapshot{snapshot} -> validateSnapshot snapshot
        _ -> pure ()
    _ -> Left "scenario supports only open heads"
  forM_ world.confirmations validateSnapshot
  forM_ (world.pending <> world.retries <> world.delayed) $ \d -> do
    unless (d.senderIndex >= 0 && d.senderIndex < 3 && d.recipientIndex >= 0 && d.recipientIndex < 3) $ Left "scenario party index out of range"
    case d.message of
      ReqTx{} -> pure ()
      ReqSn _ _ _ Nothing Nothing -> pure ()
      AckSn{} -> pure ()
      _ -> Left "message outside simple snapshot scenario"
 where
  validateSnapshot snapshot = do
    unless (snapshot.headId == world.originSnapshot.headId && snapshot.version == world.originSnapshot.version) $ Left "snapshot outside scenario head/version"
    unless (isNothing snapshot.utxoToCommit && isNothing snapshot.utxoToDecommit && isNothing snapshot.depositTxId) $ Left "scenario excludes incremental snapshots"

actionLabel :: Action -> Text
actionLabel = \case
  Deliver d -> describe "deliver" d
  Retry d -> describe "retry" d
  Delay d -> describe "delay" d
  Release d -> describe "release" d
  Duplicate d -> describe "duplicate" d
  Corrupt d -> describe "corrupt acknowledgement" d
 where
  describe verb d = verb <> " " <> messageLabel d.message <> " from party " <> show d.senderIndex <> " to " <> show d.recipientIndex <> " (TTL " <> show d.remainingTTL <> ")"
  messageLabel = \case
    ReqTx tx -> "ReqTx " <> show (txId tx)
    ReqSn version number txs _ _ -> "ReqSn " <> show version <> "/" <> show number <> " txs " <> show txs
    AckSn _ number -> "AckSn " <> show number
    _ -> "unsupported message"
