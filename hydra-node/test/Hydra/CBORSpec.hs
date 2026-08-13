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
import Hydra.API.ServerOutput (
  ClientMessage,
  DecommitInvalidReason,
  FanoutProgressMode,
  Greetings,
  HeadStatus,
  InvalidInput (..),
  NetworkInfo,
  ServerOutput,
  TimedServerOutput,
 )
import Hydra.Chain (ChainEvent, OnChainTx, PostChainTx, PostTxError)
import Hydra.Chain.Direct.State (ChainStateAt)
import Hydra.HeadLogic.Error (RequirementFailure, SideLoadRequirementFailure)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.State (FanoutMode, HeadState, SeenSnapshot)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Network (Connectivity, WhichEtcd)
import Hydra.Network.Message (Message)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (Deposit, DepositStatus, NodeState, SyncedStatus)
import Hydra.Tx (ConfirmedSnapshot, Snapshot)
import Test.Hydra.API.ClientInput ()
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.CBOR (genGoldenSamples, goldenCBOR, roundtripCBOR)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Network.Message ()
import Test.Hydra.Node.Environment ()
import Test.QuickCheck (resize)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ConstructorArbitraryPair (..), ToADTArbitrary, toADTArbitrary)

instance Arbitrary InvalidInput where
  arbitrary = InvalidInput <$> arbitrary <*> arbitrary

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
    roundtripCBOR $ Proxy @HeadStatus
    roundtripCBOR $ Proxy @NetworkInfo

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
