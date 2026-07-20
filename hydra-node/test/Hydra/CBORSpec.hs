{-# OPTIONS_GHC -Wno-orphans #-}

-- | Roundtrip tests for all the hand-written 'ToCBOR' / 'FromCBOR' codecs.
--
-- This is the CI guard that keeps encoder/decoder pairs in sync: adding a
-- constructor to any of these types without a matching codec (or with fields
-- decoded in the wrong order) fails here.
module Hydra.CBORSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.ClientInput (ClientInput)
import Hydra.API.ServerOutput (
  ClientMessage,
  Greetings,
  HeadStatus,
  InvalidInput (..),
  NetworkInfo,
  ServerOutput,
  TimedServerOutput,
 )
import Hydra.Chain (ChainEvent, OnChainTx, PostChainTx, PostTxError)
import Hydra.Chain.Direct.State (ChainStateAt)
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.State (HeadState, SeenSnapshot)
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.Network (Connectivity)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (Deposit, NodeState)
import Hydra.Tx (ConfirmedSnapshot, Snapshot)
import Test.Hydra.API.ClientInput ()
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.CBOR (goldenCBOR, roundtripCBOR)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.HeadLogic.Outcome ()
import Test.Hydra.HeadLogic.StateEvent ()
import Test.Hydra.Node.Environment ()
import Test.QuickCheck (resize)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ConstructorArbitraryPair (..), toADTArbitrary)

instance Arbitrary InvalidInput where
  arbitrary = InvalidInput <$> arbitrary <*> arbitrary

spec :: Spec
spec = parallel $ do
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
