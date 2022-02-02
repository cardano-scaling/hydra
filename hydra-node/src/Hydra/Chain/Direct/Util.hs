{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.Util where

import Hydra.Prelude

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Ledger.Crypto (DSIGN)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Tracer (nullTracer)
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Hydra.Cardano.Api hiding (AlonzoEra, Block, SigningKey, VerificationKey)
import qualified Hydra.Cardano.Api as Shelley
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
import Ouroboros.Network.Block (Point (..))
import Ouroboros.Network.NodeToClient (
  LocalAddress (..),
  NetworkConnectTracers (..),
  NetworkServerTracers (..),
  NodeToClientVersionData (..),
  combineVersions,
  simpleSingletonVersions,
 )
import Ouroboros.Network.Protocol.Handshake.Version (Versions)
import Plutus.V1.Ledger.Api (BuiltinByteString, Data, ToData (toBuiltinData), toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (oneof)

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
    | v <- [nodeToClientVLatest, pred nodeToClientVLatest]
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

readKeyPair :: FilePath -> IO (Shelley.VerificationKey PaymentKey, Shelley.SigningKey PaymentKey)
readKeyPair keyPath = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) keyPath
  pure (getVerificationKey sk, sk)

readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType =
  either (fail . show) pure <=< readFileTextEnvelope asType

readVerificationKey :: FilePath -> IO (Shelley.VerificationKey PaymentKey)
readVerificationKey = readFileTextEnvelopeThrow (Shelley.AsVerificationKey Shelley.AsPaymentKey)

--
-- Avoid Orphan instances for Point & logging
--

newtype SomePoint = SomePoint (Point Block)
  deriving stock (Eq, Generic)
  deriving newtype (Show)

instance Arbitrary SomePoint where
  arbitrary =
    SomePoint
      <$> oneof
        [ pure $ Point Origin
        ]

--
-- Helpers
--

-- | A simple retrying function with a constant delay. Retries only if the given
-- predicate evaluates to 'True'.
--
-- Better coupled with a 'timeout' function.
retry ::
  forall e m a.
  (MonadCatch m, MonadDelay m, Exception e) =>
  (e -> Bool) ->
  m a ->
  m a
retry predicate action =
  catchIf predicate action $ \_ ->
    threadDelay 0.5 >> retry predicate action
 where
  catchIf f a b = a `catch` \e -> if f e then b e else throwIO e

-- | Marker datum used to identify payment UTXO
markerDatum :: Data
markerDatum = toData $ toBuiltinData ("Hydra Head Payment" :: BuiltinByteString)

-- | Hash of the markerDatum
markerDatumHash :: Hash ScriptData
markerDatumHash =
  hashScriptData $ Shelley.fromPlutusData markerDatum

-- | Determine whether a 'TxOut' is marked to be used for paying Hydra Head transactions
isMarkedOutput :: TxOut CtxUTxO -> Bool
isMarkedOutput = \case
  (TxOut _ _ (TxOutDatumHash ha)) -> ha == markerDatumHash
  _ -> False
