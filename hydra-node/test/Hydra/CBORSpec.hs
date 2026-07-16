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
import Hydra.HeadLogic.State (HeadState, SeenSnapshot)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Network (Connectivity)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (Deposit, NodeState)
import Hydra.Tx (ConfirmedSnapshot, Snapshot)
import Test.Hydra.API.ClientInput ()
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.CBOR (roundtripCBOR)
import Test.Hydra.Chain.Direct.State ()
import Test.Hydra.Node.Environment ()

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
