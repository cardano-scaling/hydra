{-# OPTIONS_GHC -Wno-orphans #-}

module Log where

import Hydra.Prelude

import Data.Aeson (Value (..), object, (.=))
import Hydra.Network.Ouroboros (
  encodeTraceSendRecvFireForget,
 )
import Hydra.Network.Ouroboros.Type (FireForget)
import Network.Socket (
  AddrInfo (..),
  AddrInfoFlag,
  Family,
  ProtocolNumber,
  SockAddr (..),
  SocketType (..),
 )
import Ouroboros.Network.Driver (TraceSendRecv)

data NetLog msg
  = ConnectingTo {address :: AddrInfo}
  | ConnectedTo {address :: AddrInfo}
  | TraceSendRecv {sendRecv :: TraceSendRecv (FireForget msg)}
  | Injecting {message :: msg}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance ToJSON msg => ToJSON (TraceSendRecv (FireForget msg)) where
  toJSON = toJSON . encodeTraceSendRecvFireForget

instance ToJSON AddrInfoFlag where
  toJSON = String . show

instance ToJSON Family where
  toJSON = String . show

instance ToJSON SocketType where
  toJSON = String . show

instance ToJSON ProtocolNumber where
  toJSON = Number . fromIntegral . fromEnum

instance ToJSON SockAddr where
  toJSON = \case
    SockAddrInet p h -> object ["port" .= p, "host" .= h]
    SockAddrInet6 p f h s ->
      object
        [ "port" .= p
        , "host" .= h
        , "flowInfo" .= f
        , "scopeId" .= s
        ]
    SockAddrUnix f -> object ["file" .= f]

instance ToJSON AddrInfo where
  toJSON AddrInfo{addrFlags, addrFamily, addrSocketType, addrProtocol, addrAddress, addrCanonName} =
    object
      [ "addrFlags" .= addrFlags
      , "addrFamily" .= addrFamily
      , "addrSocketType" .= addrSocketType
      , "addrProtocol" .= addrProtocol
      , "addrAddress" .= addrAddress
      , "addrCanonName" .= addrCanonName
      ]
