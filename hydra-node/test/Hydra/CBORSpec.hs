{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for the 'ToCBOR' / 'FromCBOR' codecs of hydra-node types.
--
-- Three layers of protection:
--
--   * Unit tests for 'genericToCBOR' / 'genericFromCBOR' pinning down the
--     constructor-name-tagged format they produce (including that newtypes
--     and single-constructor records carry the tag).
--
--   * Roundtrip properties keeping encoder/decoder pairs in sync: adding a
--     constructor without a matching codec (or with fields decoded in the
--     wrong order) fails here.
--
--   * Golden tests locking the concrete byte-level formats, one sample per
--     constructor. These catch changes that roundtrip properties cannot see,
--     e.g. reordering fields in a data declaration of a generically derived
--     codec, or symmetric encoder+decoder drift. If one fails, the change
--     breaks decoding of persisted data (hydra.db) or the API wire format;
--     only delete and regenerate a golden file as a deliberate, documented
--     format change.
module Hydra.CBORSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Write (toStrictByteString)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (
  DraftCommitTxRequest,
  DraftCommitTxResponse,
  OperationTimedOut,
  SideLoadSnapshotRequest (..),
  SubmitL2TxRequest,
  SubmitL2TxResponse,
  SubmitTxRequest,
  TransactionSubmitted,
 )
import Hydra.API.ServerOutput (
  ApiMessage,
  ClientMessage (..),
  DecommitInvalidReason,
  FanoutProgressMode,
  Greetings,
  HeadStatus,
  InvalidInput,
  NetworkInfo,
  ServerOutput,
  TimedServerOutput,
 )
import Hydra.Cardano.Api (ChainPoint (..), NetworkId (..), NetworkMagic (..))
import Hydra.Cardano.Api.Gen ()
import Hydra.Chain (ChainEvent, OnChainTx, PostChainTx, PostTxError)
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.Chain.Direct.State (ChainStateAt)
import Hydra.HeadLogic.Error (RequirementFailure, SideLoadRequirementFailure)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.State (ClosedState (..), CoordinatedHeadState (..), FanoutMode, HeadState, SeenSnapshot, closedStateCBORTag, closedStateCBORTagV1, coordinatedHeadStateCBORTag, coordinatedHeadStateCBORTagV1)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (ValidationError (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleChainState, SimpleTx)
import Hydra.Network (Connectivity, Host, NodeId, ProtocolVersion, StallReason, WhichEtcd)
import Hydra.Network.Authenticate (Signed)
import Hydra.Network.Message (Message)
import Hydra.Node.ApiTransactionTimeout (ApiTransactionTimeout)
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime, Deposit, DepositStatus, NodeState (..), SyncedStatus, TrackedDeposit, nodeCatchingUpCBORTag, nodeCatchingUpCBORTagV1, nodeCatchingUpCBORTagV2, nodeInSyncCBORTag, nodeInSyncCBORTagV1, nodeInSyncCBORTagV2, pendingDeposits, trackedFromPending)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)
import Hydra.Tx (ConfirmedSnapshot, HeadId, HeadParameters, HeadSeed, Party, Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Snapshot (snapshotCBORTag, snapshotCBORTagV1)
import Hydra.Tx.Snapshot qualified as Snapshot
import Hydra.UserValidationRules (PolicyViolation (..))
import Test.Hydra.API.ClientInput ()
import Test.Hydra.API.HTTPServer ()
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.CBOR (genGoldenSample, genGoldenSamples, goldenCBOR, roundtripCBOR)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Ledger ()
import Test.Hydra.Ledger.Simple ()
import Test.Hydra.Network ()
import Test.Hydra.Network.Authenticate ()
import Test.Hydra.Network.Message ()
import Test.Hydra.Node.ApiTransactionTimeout ()
import Test.Hydra.Node.Environment ()
import Test.Hydra.Node.State ()
import Test.Hydra.Node.UnsyncedPeriod ()
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck (resize, suchThat)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ConstructorArbitraryPair (..), ToADTArbitrary, toADTArbitrary)

-- * ToADTArbitrary instances for per-constructor golden samples

instance ToADTArbitrary (ClientMessage Tx)
instance ToADTArbitrary PolicyViolation
instance ToADTArbitrary (TimedServerOutput Tx)
instance ToADTArbitrary InvalidInput
instance ToADTArbitrary HeadStatus
instance ToADTArbitrary FanoutProgressMode
instance ToADTArbitrary NetworkInfo
instance ToADTArbitrary (DecommitInvalidReason Tx)
instance ToADTArbitrary (ConfirmedSnapshot Tx)
instance ToADTArbitrary (PostChainTx Tx)
instance ToADTArbitrary (OnChainTx Tx)
instance ToADTArbitrary (PostTxError Tx)
instance ToADTArbitrary (ChainEvent Tx)
instance ToADTArbitrary (HeadState Tx)
instance ToADTArbitrary (SeenSnapshot Tx)
instance ToADTArbitrary (FanoutMode Tx)
instance ToADTArbitrary (NodeState Tx)
instance ToADTArbitrary SyncedStatus
instance ToADTArbitrary StallReason
instance ToADTArbitrary DepositStatus
instance ToADTArbitrary (Deposit Tx)
instance ToADTArbitrary Connectivity
instance ToADTArbitrary WhichEtcd
instance ToADTArbitrary (RequirementFailure Tx)
instance ToADTArbitrary (SideLoadRequirementFailure Tx)

-- * Test types for the generic codec

data GenericCBORSum
  = GenNullary
  | GenPositional Int Text
  | GenRecord {genA :: Word64, genB :: [Int], genC :: Maybe Text}
  deriving stock (Eq, Show, Generic)

instance ToCBOR GenericCBORSum where
  toCBOR = genericToCBOR

instance FromCBOR GenericCBORSum where
  fromCBOR = genericFromCBOR

newtype GenericCBORNewtype = GenericCBORNewtype Int
  deriving stock (Eq, Show, Generic)

instance ToCBOR GenericCBORNewtype where
  toCBOR = genericToCBOR

instance FromCBOR GenericCBORNewtype where
  fromCBOR = genericFromCBOR

spec :: Spec
spec = parallel $ do
  describe "genericToCBOR / genericFromCBOR" $ do
    it "encodes a nullary constructor as just its name tag" $
      serialize' GenNullary `shouldBe` serialize' ("GenNullary" :: Text)

    it "encodes the name tag followed by fields in declaration order" $
      serialize' (GenPositional 42 "hi")
        `shouldBe` toStrictByteString
          (toCBOR ("GenPositional" :: Text) <> toCBOR (42 :: Int) <> toCBOR ("hi" :: Text))

    it "encodes record fields in declaration order" $
      serialize' GenRecord{genA = 7, genB = [1, 2], genC = Just "x"}
        `shouldBe` toStrictByteString
          ( toCBOR ("GenRecord" :: Text)
              <> toCBOR (7 :: Word64)
              <> toCBOR ([1, 2] :: [Int])
              <> toCBOR (Just ("x" :: Text))
          )

    it "tags newtypes with their constructor name too" $
      serialize' (GenericCBORNewtype 7)
        `shouldBe` toStrictByteString (toCBOR ("GenericCBORNewtype" :: Text) <> toCBOR (7 :: Int))

    it "roundtrips all constructor shapes" $ do
      decodeFull' (serialize' GenNullary) `shouldBe` Right GenNullary
      decodeFull' (serialize' (GenPositional 42 "hi")) `shouldBe` Right (GenPositional 42 "hi")
      decodeFull' (serialize' GenRecord{genA = 7, genB = [1, 2], genC = Nothing})
        `shouldBe` Right GenRecord{genA = 7, genB = [1, 2], genC = Nothing}
      decodeFull' (serialize' (GenericCBORNewtype 7)) `shouldBe` Right (GenericCBORNewtype 7)

    it "fails decoding an unknown tag naming the type" $
      case decodeFull' @GenericCBORSum (serialize' ("Bogus" :: Text)) of
        Left err -> show err `shouldContain` "is not a proper CBOR-encoded GenericCBORSum"
        Right v -> expectationFailure $ "unexpectedly decoded: " <> show v

  describe "API types" $ do
    roundtripCBOR $ Proxy @(ClientInput Tx)
    roundtripCBOR $ Proxy @(ServerOutput Tx)
    roundtripCBOR $ Proxy @(TimedServerOutput Tx)
    roundtripCBOR $ Proxy @(ClientMessage Tx)
    roundtripCBOR $ Proxy @(Greetings Tx)
    roundtripCBOR $ Proxy @InvalidInput
    roundtripCBOR $ Proxy @(ApiMessage Tx)
    roundtripCBOR $ Proxy @HeadStatus
    roundtripCBOR $ Proxy @NetworkInfo
    roundtripCBOR $ Proxy @(DraftCommitTxRequest Tx)
    roundtripCBOR $ Proxy @(DraftCommitTxResponse Tx)
    roundtripCBOR $ Proxy @(SubmitTxRequest Tx)
    roundtripCBOR $ Proxy @TransactionSubmitted
    roundtripCBOR $ Proxy @(SideLoadSnapshotRequest Tx)
    roundtripCBOR $ Proxy @(SubmitL2TxRequest Tx)
    roundtripCBOR $ Proxy @SubmitL2TxResponse
    roundtripCBOR $ Proxy @OperationTimedOut

  -- 'Snapshot' gained 'depositTxId' between two released layouts. Its fields are
  -- a bare concatenation, so only the leading tag tells them apart and a node
  -- must keep decoding the older one to replay an event log written before the
  -- field existed.
  describe "Snapshot layouts" $ do
    let snapshot = generateWith (resize 5 arbitrary) 42 :: Snapshot Tx
        Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, utxoToDecommit} = snapshot

    it "writes the current layout under a tag of its own" $
      serialize' snapshot `shouldSatisfy` BS.isPrefixOf (serialize' snapshotCBORTag)

    it "decodes the layout written before depositTxId existed" $ do
      let legacy =
            toStrictByteString $
              toCBOR snapshotCBORTagV1
                <> toCBOR headId
                <> toCBOR version
                <> toCBOR number
                <> toCBOR confirmed
                <> toCBOR utxo
                <> toCBOR utxoToCommit
                <> toCBOR utxoToDecommit
      decodeFull' legacy `shouldBe` Right snapshot{Snapshot.depositTxId = Nothing}

  -- 'CoordinatedHeadState' gained 'finalizedCommit' and 'finalizedDecommit'
  -- between two released layouts, same situation as 'Snapshot' above.
  describe "CoordinatedHeadState layouts" $ do
    let chs = generateWith (resize 3 arbitrary) 42 :: CoordinatedHeadState Tx
        CoordinatedHeadState{localUTxO, localTxs, allTxs, confirmedSnapshot, seenSnapshot, currentDepositTxId, decommitTx, version} = chs

    it "writes the current layout under a tag of its own" $
      serialize' chs `shouldSatisfy` BS.isPrefixOf (serialize' coordinatedHeadStateCBORTag)

    it "decodes the layout written before finalizedCommit/finalizedDecommit existed" $ do
      let legacy =
            toStrictByteString $
              toCBOR coordinatedHeadStateCBORTagV1
                <> toCBOR localUTxO
                <> toCBOR localTxs
                <> toCBOR allTxs
                <> toCBOR confirmedSnapshot
                <> toCBOR seenSnapshot
                <> toCBOR currentDepositTxId
                <> toCBOR decommitTx
                <> toCBOR version
      decodeFull' legacy `shouldBe` Right chs{finalizedCommit = Nothing, finalizedDecommit = Nothing}

  -- 'ClosedState' gained 'unconfirmedSnapshot' between two released layouts,
  -- same situation as 'Snapshot' above. The legacy layout is the one the
  -- generic encoding wrote: the constructor name followed by the fields.
  describe "ClosedState layouts" $ do
    let closedState = generateWith (resize 3 arbitrary) 42 :: ClosedState Tx
        ClosedState{parameters, confirmedSnapshot, contestationDeadline, readyToFanoutSent, chainState, headId, headSeed, version} = closedState

    it "writes the current layout under a tag of its own" $
      serialize' closedState `shouldSatisfy` BS.isPrefixOf (serialize' closedStateCBORTag)

    it "decodes the layout written before unconfirmedSnapshot existed" $ do
      let legacy =
            toStrictByteString $
              toCBOR closedStateCBORTagV1
                <> toCBOR parameters
                <> toCBOR confirmedSnapshot
                <> toCBOR contestationDeadline
                <> toCBOR readyToFanoutSent
                <> toCBOR chainState
                <> toCBOR headId
                <> toCBOR headSeed
                <> toCBOR version
      decodeFull' legacy `shouldBe` Right closedState{unconfirmedSnapshot = Nothing}

  describe "NodeState layouts" $ do
    let sample = generateWith (resize 3 arbitrary) 42 :: NodeState Tx
    forM_
      [ (nodeInSyncCBORTag, nodeInSyncCBORTagV2, nodeInSyncCBORTagV1, NodeInSync)
      , (nodeCatchingUpCBORTag, nodeCatchingUpCBORTagV2, nodeCatchingUpCBORTagV1, NodeCatchingUp)
      ]
      $ \(tag, tagV2, tagV1, mkNodeState) -> describe (toString tag) $ do
        let nodeState = mkNodeState (headState sample) (deposits sample) (chainPointTime sample) Nothing
            encodeWithoutPolicy :: Text -> Encoding -> Encoding
            encodeWithoutPolicy layoutTag encodeDeposits =
              toCBOR layoutTag
                <> toCBOR (headState nodeState)
                <> encodeDeposits
                <> toCBOR (chainPointTime nodeState)

        it "writes the current layout under a tag of its own" $
          serialize' nodeState `shouldSatisfy` BS.isPrefixOf (serialize' tag)

        it "decodes the layout written before deposit lifecycle tracking existed" $
          decodeFull' (toStrictByteString $ encodeWithoutPolicy tagV1 $ toCBOR $ pendingDeposits nodeState)
            `shouldBe` Right nodeState{deposits = trackedFromPending (pendingDeposits nodeState)}

        it "decodes the layout written before policy bindings existed" $
          decodeFull' (toStrictByteString $ encodeWithoutPolicy tagV2 $ toCBOR $ deposits nodeState)
            `shouldBe` Right nodeState

        it "does not consume the field following a legacy state" $ do
          let legacy = encodeWithoutPolicy tagV2 (toCBOR $ deposits nodeState)
              bytes = toStrictByteString $ toCBOR ("LegacyWrapper" :: Text) <> legacy <> toCBOR (42 :: Int)
          decodeFull' bytes `shouldBe` Right (LegacyWrapper nodeState 42)

        it "rejects a current layout with a missing policy field" $
          decodeFull' @(NodeState Tx) (toStrictByteString $ encodeWithoutPolicy tag $ toCBOR $ deposits nodeState)
            `shouldSatisfy` isLeft

        it "decodes JSON without a policy binding" $
          Aeson.fromJSON (withoutField "headPolicies" $ toJSON nodeState)
            `shouldBe` Aeson.Success nodeState

        forM_ [Nothing, Just (generateWith arbitrary 42)] $ \binding ->
          it ("roundtrips a policy binding of " <> show binding) $ do
            let bound = nodeState{headPolicies = binding}
            decodeFull' (serialize' bound) `shouldBe` Right bound
            Aeson.fromJSON (toJSON bound) `shouldBe` Aeson.Success bound

  describe "Environment layouts" $ do
    let env = generateWith (resize 3 arbitrary) 42 :: Environment
        withoutPolicy tag =
          toCBOR (tag :: Text)
            <> toCBOR (party env)
            <> toCBOR (otherParties env)
            <> toCBOR (participants env)
            <> toCBOR (contestationPeriod env)
            <> toCBOR (depositPeriod env)
            <> toCBOR (depositActivation env)
            <> toCBOR (unsyncedPeriod env)
            <> toCBOR (configuredPeers env)

    it "writes the current layout under a tag of its own" $
      serialize' env `shouldSatisfy` BS.isPrefixOf (serialize' ("Environment2" :: Text))

    it "decodes the layout written before configuredPolicy existed" $
      decodeFull' (toStrictByteString $ withoutPolicy "Environment")
        `shouldBe` Right env{configuredPolicy = Nothing}

    it "does not consume the field following a legacy environment" $ do
      let bytes = toStrictByteString $ toCBOR ("LegacyWrapper" :: Text) <> withoutPolicy "Environment" <> toCBOR (42 :: Int)
      decodeFull' bytes `shouldBe` Right (LegacyWrapper env{configuredPolicy = Nothing} 42)

    it "rejects a current layout with a missing policy field" $
      decodeFull' @Environment (toStrictByteString $ withoutPolicy "Environment2")
        `shouldSatisfy` isLeft

    it "decodes JSON without a configured policy" $
      Aeson.fromJSON (withoutField "configuredPolicy" $ toJSON env)
        `shouldBe` Aeson.Success env{configuredPolicy = Nothing}

    forM_ [Nothing, Just (generateWith arbitrary 42)] $ \policy ->
      it ("roundtrips a configured policy of " <> show policy) $ do
        let configured = env{configuredPolicy = policy}
        decodeFull' (serialize' configured) `shouldBe` Right configured
        Aeson.fromJSON (toJSON configured) `shouldBe` Aeson.Success configured

  describe "goldens before policy bindings" $ do
    legacyCBOR (Proxy @Environment) "golden/legacy-policy/Environment.cbor"
    legacyCBOR (Proxy @(NodeState Tx)) "golden/legacy-policy/NodeState.cbor"
    legacyCBOR (Proxy @(StateEvent Tx)) "golden/legacy-policy/StateEvent.cbor"
    legacyCBOR (Proxy @(Greetings Tx)) "golden/legacy-policy/Greetings.cbor"
    legacyCBOR (Proxy @(ServerOutput Tx)) "golden/legacy-policy/ServerOutput.cbor"

  describe "protocol types" $ do
    roundtripCBOR $ Proxy @(Snapshot Tx)
    roundtripCBOR $ Proxy @(ConfirmedSnapshot Tx)
    roundtripCBOR $ Proxy @(PostChainTx Tx)
    roundtripCBOR $ Proxy @(OnChainTx Tx)
    roundtripCBOR $ Proxy @(PostTxError Tx)
    roundtripCBOR $ Proxy @(ChainEvent Tx)
    roundtripCBOR $ Proxy @ChainStateAt
    roundtripCBOR $ Proxy @(HeadState Tx)
    roundtripCBOR $ Proxy @(SeenSnapshot Tx)
    roundtripCBOR $ Proxy @(NodeState Tx)
    roundtripCBOR $ Proxy @(Deposit Tx)
    roundtripCBOR $ Proxy @(TrackedDeposit Tx)
    roundtripCBOR $ Proxy @Environment
    roundtripCBOR $ Proxy @Connectivity
    roundtripCBOR $ Proxy @HeadId
    roundtripCBOR $ Proxy @HeadSeed
    roundtripCBOR $ Proxy @OnChainId
    roundtripCBOR $ Proxy @Party
    roundtripCBOR $ Proxy @HeadParameters
    roundtripCBOR $ Proxy @ContestationPeriod
    roundtripCBOR $ Proxy @DepositPeriod
    roundtripCBOR $ Proxy @SnapshotNumber
    roundtripCBOR $ Proxy @SnapshotVersion
    roundtripCBOR $ Proxy @ChainSlot
    roundtripCBOR $ Proxy @(Signature (Snapshot Tx))
    roundtripCBOR $ Proxy @(MultiSignature (Snapshot Tx))
    roundtripCBOR $ Proxy @(Signed (Message Tx))
    roundtripCBOR $ Proxy @NodeId
    roundtripCBOR $ Proxy @ProtocolVersion
    roundtripCBOR $ Proxy @Host
    roundtripCBOR $ Proxy @ValidationError
    roundtripCBOR $ Proxy @PolicyViolation
    roundtripCBOR $ Proxy @ApiTransactionTimeout
    roundtripCBOR $ Proxy @UnsyncedPeriod
    roundtripCBOR $ Proxy @ChainPointTime
    roundtripCBOR $ Proxy @SimpleTx
    roundtripCBOR $ Proxy @SimpleChainState
    roundtripCBOR $ Proxy @NetworkId
    roundtripCBOR $ Proxy @NetworkMagic
    roundtripCBOR $ Proxy @ChainPoint

  describe "persisted types" $ do
    roundtripCBOR $ Proxy @(StateChanged Tx)
    roundtripCBOR $ Proxy @(StateEvent Tx)
    -- Locks the on-disk format of hydra.db events: one sample per
    -- 'StateChanged' constructor, stored as raw CBOR.
    goldenCBOR "StateEvent Tx" "golden/StateEvent.cbor" genGoldenStateEvents

  -- One golden per CBOR-encoded type, one sample per constructor. Locks the
  -- byte-level formats of the hydra.db event payloads and the API/network
  -- wire messages.
  describe "golden formats" $ do
    goldenCBOR "ClientInput Tx" "golden/ClientInput.cbor" (genGoldenSamples @(ClientInput Tx))
    goldenCBOR "ServerOutput Tx" "golden/ServerOutput.cbor" (genGoldenSamples @(ServerOutput Tx))
    goldenCBOR "TimedServerOutput Tx" "golden/TimedServerOutput.cbor" (genGoldenSamples @(TimedServerOutput Tx))
    goldenCBOR "ClientMessage Tx" "golden/ClientMessage.cbor" (genGoldenSamples @(ClientMessage Tx))
    goldenCBOR "Greetings Tx" "golden/Greetings.cbor" (genGoldenSamples @(Greetings Tx))
    goldenCBOR "InvalidInput" "golden/InvalidInput.cbor" (genGoldenSamples @InvalidInput)
    goldenCBOR "HeadStatus" "golden/HeadStatus.cbor" (genGoldenSamples @HeadStatus)
    goldenCBOR "FanoutProgressMode" "golden/FanoutProgressMode.cbor" (genGoldenSamples @FanoutProgressMode)
    goldenCBOR "NetworkInfo" "golden/NetworkInfo.cbor" (genGoldenSamples @NetworkInfo)
    goldenCBOR "DecommitInvalidReason Tx" "golden/DecommitInvalidReason.cbor" (genGoldenSamples @(DecommitInvalidReason Tx))
    goldenCBOR "Message Tx" "golden/Message.cbor" (genGoldenSamples @(Message Tx))
    goldenCBOR "ConfirmedSnapshot Tx" "golden/ConfirmedSnapshot.cbor" (genGoldenSamples @(ConfirmedSnapshot Tx))
    goldenCBOR "PostChainTx Tx" "golden/PostChainTx.cbor" (genGoldenSamples @(PostChainTx Tx))
    goldenCBOR "OnChainTx Tx" "golden/OnChainTx.cbor" (genGoldenSamples @(OnChainTx Tx))
    goldenCBOR "PostTxError Tx" "golden/PostTxError.cbor" (genGoldenSamples @(PostTxError Tx))
    goldenCBOR "ChainEvent Tx" "golden/ChainEvent.cbor" (genGoldenSamples @(ChainEvent Tx))
    goldenCBOR "HeadState Tx" "golden/HeadState.cbor" (genGoldenSamples @(HeadState Tx))
    goldenCBOR "SeenSnapshot Tx" "golden/SeenSnapshot.cbor" (genGoldenSamples @(SeenSnapshot Tx))
    goldenCBOR "FanoutMode Tx" "golden/FanoutMode.cbor" (genGoldenSamples @(FanoutMode Tx))
    goldenCBOR "NodeState Tx" "golden/NodeState.cbor" (genGoldenSamples @(NodeState Tx))
    goldenCBOR "SyncedStatus" "golden/SyncedStatus.cbor" (genGoldenSamples @SyncedStatus)
    goldenCBOR "StallReason" "golden/StallReason.cbor" (genGoldenSamples @StallReason)
    goldenCBOR "DepositStatus" "golden/DepositStatus.cbor" (genGoldenSamples @DepositStatus)
    goldenCBOR "Deposit Tx" "golden/Deposit.cbor" (genGoldenSamples @(Deposit Tx))
    goldenCBOR "Connectivity" "golden/Connectivity.cbor" (genGoldenSamples @Connectivity)
    goldenCBOR "WhichEtcd" "golden/WhichEtcd.cbor" (genGoldenSamples @WhichEtcd)
    goldenCBOR "RequirementFailure Tx" "golden/RequirementFailure.cbor" (genGoldenSamples @(RequirementFailure Tx))
    goldenCBOR "SideLoadRequirementFailure Tx" "golden/SideLoadRequirementFailure.cbor" (genGoldenSamples @(SideLoadRequirementFailure Tx))
    -- Single-constructor domain types (tagged newtypes and records), sampled
    -- through their own 'Arbitrary' instances.
    goldenCBOR "HeadId" "golden/HeadId.cbor" (genGoldenSample @HeadId)
    goldenCBOR "HeadSeed" "golden/HeadSeed.cbor" (genGoldenSample @HeadSeed)
    goldenCBOR "OnChainId" "golden/OnChainId.cbor" (genGoldenSample @OnChainId)
    goldenCBOR "Party" "golden/Party.cbor" (genGoldenSample @Party)
    goldenCBOR "HeadParameters" "golden/HeadParameters.cbor" (genGoldenSample @HeadParameters)
    goldenCBOR "ContestationPeriod" "golden/ContestationPeriod.cbor" (genGoldenSample @ContestationPeriod)
    goldenCBOR "DepositPeriod" "golden/DepositPeriod.cbor" (genGoldenSample @DepositPeriod)
    goldenCBOR "SnapshotNumber" "golden/SnapshotNumber.cbor" (genGoldenSample @SnapshotNumber)
    goldenCBOR "SnapshotVersion" "golden/SnapshotVersion.cbor" (genGoldenSample @SnapshotVersion)
    goldenCBOR "ChainSlot" "golden/ChainSlot.cbor" (genGoldenSample @ChainSlot)
    goldenCBOR "Signature (Snapshot Tx)" "golden/Signature.cbor" (genGoldenSample @(Signature (Snapshot Tx)))
    goldenCBOR "MultiSignature (Snapshot Tx)" "golden/MultiSignature.cbor" (genGoldenSample @(MultiSignature (Snapshot Tx)))
    goldenCBOR "Snapshot Tx" "golden/Snapshot.cbor" (genGoldenSample @(Snapshot Tx))
    goldenCBOR "Signed (Message Tx)" "golden/Signed.cbor" (genGoldenSample @(Signed (Message Tx)))
    goldenCBOR "NodeId" "golden/NodeId.cbor" (genGoldenSample @NodeId)
    goldenCBOR "ProtocolVersion" "golden/ProtocolVersion.cbor" (genGoldenSample @ProtocolVersion)
    goldenCBOR "Host" "golden/Host.cbor" (genGoldenSample @Host)
    goldenCBOR "ValidationError" "golden/ValidationError.cbor" (genGoldenSample @ValidationError)
    goldenCBOR "PolicyValidationError" "golden/PolicyValidationError.cbor" (map PolicyValidationError <$> genGoldenSamples @PolicyViolation)
    goldenCBOR "SnapshotRequestRejectedByPolicy" "golden/SnapshotRequestRejectedByPolicy.cbor" $ do
      headId <- arbitrary
      transactionId <- arbitrary
      depositId <- arbitrary
      pure
        [ SnapshotRequestRejectedByPolicy headId 1 tx deposit (MaxUTxOCountExceeded 2 1) :: ClientMessage Tx
        | tx <- [Nothing, Just transactionId]
        , deposit <- [Nothing, Just depositId]
        ]
    goldenCBOR "PolicyAssetsViolation" "golden/PolicyAssetsViolation.cbor" $
      filter (\case PolicyAssetsViolation{} -> True; _ -> False) <$> genGoldenSamples @PolicyViolation
    goldenCBOR "ApiTransactionTimeout" "golden/ApiTransactionTimeout.cbor" (genGoldenSample @ApiTransactionTimeout)
    goldenCBOR "UnsyncedPeriod" "golden/UnsyncedPeriod.cbor" (genGoldenSample @UnsyncedPeriod)
    goldenCBOR "ChainPointTime" "golden/ChainPointTime.cbor" (genGoldenSample @ChainPointTime)
    goldenCBOR "ChainStateAt" "golden/ChainStateAt.cbor" (genGoldenSample @ChainStateAt)
    goldenCBOR "Environment" "golden/Environment.cbor" (genGoldenSample @Environment)
    goldenCBOR "NetworkMagic" "golden/NetworkMagic.cbor" (genGoldenSample @NetworkMagic)
    -- Multi-constructor orphans from hydra-cardano-api, enumerated explicitly
    -- ('ToADTArbitrary' needs field-wise 'Arbitrary' instances these types
    -- do not have).
    goldenCBOR "NetworkId" "golden/NetworkId.cbor" genGoldenNetworkIds
    goldenCBOR "ChainPoint" "golden/ChainPoint.cbor" genGoldenChainPoints

-- | One sample per 'NetworkId' constructor.
genGoldenNetworkIds :: Gen [NetworkId]
genGoldenNetworkIds = do
  magic <- arbitrary
  pure [Hydra.Cardano.Api.Mainnet, Testnet magic]

-- | One sample per 'ChainPoint' constructor.
genGoldenChainPoints :: Gen [ChainPoint]
genGoldenChainPoints = do
  point <- arbitrary `suchThat` (/= ChainPointAtGenesis)
  pure [ChainPointAtGenesis, point]

-- | One 'StateEvent' per 'StateChanged' constructor, in declaration order:
-- 'ToADTArbitrary' enumerates the constructors generically, so coverage of
-- every constructor holds by construction and new constructors are included
-- automatically. Samples are generated small (resized): the golden file
-- locks tags and field order, which small values exercise just as well.
genGoldenStateEvents :: Gen [StateEvent Tx]
genGoldenStateEvents = do
  ADTArbitrary{adtCAPs} <- resize 5 $ toADTArbitrary (Proxy @(StateChanged Tx))
  forM (zip [0 ..] adtCAPs) $ \(i, ConstructorArbitraryPair{capArbitrary}) ->
    StateEvent i capArbitrary <$> arbitrary

-- | A trailing field detects legacy decoders consuming bytes from their parent.
data LegacyWrapper a = LegacyWrapper a Int
  deriving stock (Eq, Show, Generic)

instance FromCBOR a => FromCBOR (LegacyWrapper a) where
  fromCBOR = genericFromCBOR

withoutField :: Aeson.Key -> Aeson.Value -> Aeson.Value
withoutField key = \case
  Aeson.Object fields -> Aeson.Object $ KeyMap.delete key fields
  value -> value

-- | Old fixtures must decode, but re-encoding uses the new versioned layouts.
legacyCBOR :: forall a. (FromCBOR a, Show a) => Proxy a -> FilePath -> Spec
legacyCBOR _ path =
  it ("decodes " <> path) $ do
    bytes <- readFileBS path
    decodeFull' @[a] bytes `shouldSatisfy` isRight
