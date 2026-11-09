{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Concrete `Hydra.Network` stack used in a hydra-node.
--
-- This module provides a `withNetwork` function which is the composition of several layers in order to provide various capabilities:
--
--   * `withAuthentication` handles messages' authentication and signature verification
--   * `withEtcdNetwork` uses an 'etcd' cluster to implement reliable broadcast
--
-- The following diagram details the various types of messages each layer is
-- exchanging with its predecessors and successors.
--
-- @
--
--           ▲
--           │                        │
--       Authenticated msg           msg
--           │                        │
--           │                        │
-- ┌─────────┼────────────────────────▼──────┐
-- │                                         │
-- │               Authenticate              │
-- │                                         │
-- └─────────▲────────────────────────┼──────┘
--           │                        │
--           │                        │
--          msg                      msg
--           │                        │
-- ┌─────────┼────────────────────────▼──────┐
-- │                                         │
-- │                   Etcd                  │
-- │                                         │
-- └─────────▲────────────────────────┼──────┘
--           │                        │
--           │        (bytes)         │
--           │                        ▼
--
-- @
module Hydra.Node.Network (
  NetworkConfiguration (..),
  withNetwork,
  NetworkLog (..),
) where

import Hydra.Prelude hiding (fromList, replicate)

import Cardano.Binary (decodeFull', serialize')
import Control.Concurrent.Class.MonadSTM (retry)
import Control.Lens ((^.), (^..))
import Control.Tracer (Tracer)
import Crypto.Random (getRandomBytes)
import Hydra.Network (NetworkComponent, NetworkConfiguration (..), ProtocolVersion (..))
import Hydra.Network.Authenticate (AuthLog, Authenticated, Signed (..), withAuthentication)
import Hydra.Network.Etcd (EtcdLog, catchEtcdError, withEtcdNetworkSetup)
import Hydra.Network.Message (Message)
import Hydra.Network.Policy (PolicyFingerprint, PolicyLog, PolicyStore (..), withPolicyAgreement)
import Hydra.Tx (IsTx, Party)
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (.~))
import Network.GRPC.Etcd (KV)

-- | Starts the network layer of a node, passing configured `Network` to its continuation.
withNetwork ::
  forall tx.
  IsTx tx =>
  -- | Tracer to use for logging messages.
  Tracer IO NetworkLog ->
  -- | The network configuration
  NetworkConfiguration ->
  Maybe PolicyFingerprint ->
  -- | Produces a `NetworkComponent` that can send `msg` and consumes `Authenticated` @msg@.
  NetworkComponent IO (Authenticated (Message tx)) (Message tx) ()
withNetwork tracer conf fingerprint callback action = do
  session <- encodeBase16 <$> (getRandomBytes 32 :: IO ByteString)
  let setup conn runNetwork =
        raceLabelled_
          ( "policy-agreement"
          , withPolicyAgreement
              (contramap PolicyAgreement tracer)
              signingKey
              otherParties
              fingerprint
              session
              (policyStore conn)
              (atomically retry)
          )
          ("network", runNetwork)
  withAuthentication
    (contramap Authenticate tracer)
    signingKey
    otherParties
    (withEtcdNetworkSetup setup (contramap Etcd tracer) currentNetworkProtocolVersion conf)
    callback
    action
 where
  NetworkConfiguration{signingKey, otherParties} = conf

-- | One declaration per party. Old values are harmless: startup requires a
-- signed acknowledgement of a fresh random session, not a saved fingerprint.
policyStore :: Connection -> PolicyStore IO
policyStore conn =
  PolicyStore
    { publish = \declaration@Signed{party} ->
        retryPolicyStore $
          void $
            nonStreaming conn (rpc @(Protobuf KV "put")) $
              defMessage & #key .~ key party & #value .~ serialize' declaration
    , load = retryPolicyStore $ do
        response <-
          nonStreaming conn (rpc @(Protobuf KV "range")) $
            defMessage & #key .~ "policy/" & #rangeEnd .~ "policy0"
        pure
          [ declaration
          | entry <- response ^.. #kvs . traverse
          , Right declaration@Signed{party} <- [decodeFull' $ entry ^. #value]
          , entry ^. #key == key party
          ]
    }
 where
  key :: Party -> ByteString
  key party = "policy/" <> encodeUtf8 (encodeBase16 $ serialize' party)

  retryPolicyStore :: IO a -> IO a
  retryPolicyStore action = catchEtcdError action $ \_ -> threadDelay 1 >> retryPolicyStore action

-- | The latest hydra network protocol version. Used to identify
-- incompatibilities ahead of time.
--
-- Version 2: broadcast values carry a CBOR list of messages per etcd
-- revision (batched broadcast) instead of a single message.
currentNetworkProtocolVersion :: ProtocolVersion
currentNetworkProtocolVersion = ProtocolVersion 2

-- * Tracing

data NetworkLog
  = Authenticate AuthLog
  | Etcd EtcdLog
  | PolicyAgreement PolicyLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
