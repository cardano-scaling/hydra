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
import Codec.CBOR.Write (toStrictByteString)
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
  ClientMessage,
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
import Hydra.HeadLogic.State (FanoutMode, HeadState, SeenSnapshot)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger (ValidationError)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleChainState, SimpleTx)
import Hydra.Network (Connectivity, Host, NodeId, ProtocolVersion, WhichEtcd)
import Hydra.Network.Authenticate (Signed)
import Hydra.Network.Message (Message)
import Hydra.Node.ApiTransactionTimeout (ApiTransactionTimeout)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (ChainPointTime, Deposit, DepositStatus, NodeState, SyncedStatus)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)
import Hydra.Tx (ConfirmedSnapshot, HeadId, HeadParameters, HeadSeed, Party, Snapshot, SnapshotNumber, SnapshotVersion)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.OnChainId (OnChainId)
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
