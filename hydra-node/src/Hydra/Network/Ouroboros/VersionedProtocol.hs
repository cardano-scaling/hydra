module Hydra.Network.Ouroboros.VersionedProtocol where

import Hydra.Prelude

import Codec.CBOR.Term qualified as CBOR
import Data.Text qualified as T
import GHC.Natural (naturalFromInteger, naturalToInteger)
import GHC.Num (integerToInt)
import Hydra.Network (Host (..))
import Hydra.Network.Message (HydraVersionedProtocolNumber (..))

-- import Network.TypedProtocol.Pipelined ()
import Ouroboros.Network.CodecCBORTerm (CodecCBORTerm (..))
import Ouroboros.Network.Protocol.Handshake.Codec (VersionDataCodec, cborTermVersionDataCodec)
import Ouroboros.Network.Protocol.Handshake.Version (Accept (..), Acceptable, Queryable, acceptableVersion, queryVersion)

hydraVersionedProtocolCodec :: CodecCBORTerm (String, Maybe Int) HydraVersionedProtocolNumber
hydraVersionedProtocolCodec = CodecCBORTerm{encodeTerm, decodeTerm}
 where
  encodeTerm :: HydraVersionedProtocolNumber -> CBOR.Term
  encodeTerm x = CBOR.TInt $ integerToInt . naturalToInteger $ hydraVersionedProtocolNumber x

  decodeTerm :: CBOR.Term -> Either (String, Maybe Int) HydraVersionedProtocolNumber
  decodeTerm (CBOR.TInt x) = Right $ MkHydraVersionedProtocolNumber (naturalFromInteger (toInteger x))
  decodeTerm _ = Left ("unknown tag", Nothing)

type HydraVersionedProtocolData :: Type
data HydraVersionedProtocolData = MkHydraVersionedProtocolData
  deriving stock (Eq, Show, Generic, Ord)

instance Acceptable HydraVersionedProtocolData where
  acceptableVersion
    MkHydraVersionedProtocolData
    MkHydraVersionedProtocolData = Accept MkHydraVersionedProtocolData

instance Queryable HydraVersionedProtocolData where
  queryVersion MkHydraVersionedProtocolData = False

hydraVersionedProtocolDataCodec ::
  VersionDataCodec
    CBOR.Term
    HydraVersionedProtocolNumber
    HydraVersionedProtocolData
hydraVersionedProtocolDataCodec =
  cborTermVersionDataCodec
    (const CodecCBORTerm{encodeTerm, decodeTerm})
 where
  encodeTerm :: HydraVersionedProtocolData -> CBOR.Term
  encodeTerm MkHydraVersionedProtocolData = CBOR.TNull

  decodeTerm :: CBOR.Term -> Either Text HydraVersionedProtocolData
  decodeTerm CBOR.TNull = Right MkHydraVersionedProtocolData
  decodeTerm t = Left $ T.pack $ "unexpected term: " ++ show t

type HydraNetworkConfig :: Type
data HydraNetworkConfig = HydraNetworkConfig
  { protocolVersion :: HydraVersionedProtocolNumber
  , localHost :: Host
  , remoteHosts :: [Host]
  }
  deriving stock (Eq, Show, Generic)
