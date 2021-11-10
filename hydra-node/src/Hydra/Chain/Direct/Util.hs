{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Util where

import Hydra.Prelude

import Cardano.Api hiding (AlonzoEra, Block, SigningKey, VerificationKey)
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Ledger.Crypto (DSIGN, StandardCrypto)
import Control.Tracer (nullTracer)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Byron.Ledger.Config (CodecConfig (..))
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Block (
  AlonzoEra,
  CodecConfig (..),
 )
import Ouroboros.Consensus.Network.NodeToClient (
  ClientCodecs,
  clientCodecs,
 )
import Ouroboros.Consensus.Node.NetworkProtocolVersion (
  SupportedNetworkProtocolVersion (..),
 )
import Ouroboros.Consensus.Shelley.Ledger.Config (CodecConfig (..))
import Ouroboros.Network.NodeToClient (
  LocalAddress (..),
  NetworkConnectTracers (..),
  NetworkServerTracers (..),
  NodeToClientVersionData (..),
  combineVersions,
  simpleSingletonVersions,
 )
import Ouroboros.Network.Protocol.Handshake.Version (Versions)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

--
-- Types
--

type Block = CardanoBlock StandardCrypto

type Era = AlonzoEra StandardCrypto
type VerificationKey = Crypto.VerKeyDSIGN (DSIGN StandardCrypto)
type SigningKey = Crypto.SignKeyDSIGN (DSIGN StandardCrypto)

--
-- Tracers
--

nullConnectTracers :: NetworkConnectTracers LocalAddress NodeToClientVersion
nullConnectTracers =
  NetworkConnectTracers
    { nctMuxTracer = nullTracer
    , nctHandshakeTracer = nullTracer
    }

nullServerTracers :: NetworkServerTracers LocalAddress NodeToClientVersion
nullServerTracers =
  NetworkServerTracers
    { nstMuxTracer = nullTracer
    , nstHandshakeTracer = nullTracer
    , nstErrorPolicyTracer = nullTracer
    , nstAcceptPolicyTracer = nullTracer
    }

--
--  Versions
--

nodeToClientVLatest :: NodeToClientVersion
nodeToClientVLatest =
  fst $ Map.findMax $ supportedNodeToClientVersions proxy
 where
  proxy = Proxy @(CardanoBlock StandardCrypto)

versions ::
  NetworkMagic ->
  (NodeToClientVersion -> app) ->
  Versions NodeToClientVersion NodeToClientVersionData app
versions magic app =
  combineVersions
    [ simpleSingletonVersions v (NodeToClientVersionData magic) (app v)
    | v <- [nodeToClientVLatest]
    ]

--
-- Codecs
--

defaultCodecs ::
  MonadST m =>
  NodeToClientVersion ->
  ClientCodecs Block m
defaultCodecs nodeToClientV =
  clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
 where
  supportedVersions = supportedNodeToClientVersions (Proxy @Block)
  cfg = CardanoCodecConfig byron shelley allegra mary alonzo
   where
    byron = ByronCodecConfig epochSlots
    shelley = ShelleyCodecConfig
    allegra = ShelleyCodecConfig
    mary = ShelleyCodecConfig
    alonzo = ShelleyCodecConfig

  -- Fixed epoch slots used in the ByronCodecConfig.
  --
  -- TODO(SN): ^^^ This will make codecs fail on non-standard testnets and we
  -- should check with networking whether we can opt-out / ignore blocks from
  -- Byron instead of configuring this everywhere
  epochSlots :: EpochSlots
  epochSlots = EpochSlots 432000

readKeyPair :: FilePath -> IO (VerificationKey, SigningKey)
readKeyPair keyPath = do
  PaymentSigningKey sk <-
    readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) keyPath
  let vk = deriveVerKeyDSIGN sk
  pure (vk, sk)

readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType =
  either (fail . show) pure <=< readFileTextEnvelope asType
