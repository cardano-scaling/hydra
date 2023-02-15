{-# LANGUAGE TypeApplications #-}

module Hydra.API.Network where

import Prelude

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.IP (toIPv4w)
import Data.String (IsString)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Word (Word16)
import GHC.Generics (Generic)
import Network.Socket (PortNumber)
import Test.QuickCheck (Arbitrary (..), elements, listOf, suchThat)
import Text.Read (readMaybe)

-- * Types used by concrete implementations

newtype NodeId = NodeId {nodeId :: Text}
  deriving newtype (Eq, Show, IsString, Read, Ord, ToJSON, FromJSON)

instance Arbitrary NodeId where
  arbitrary =
    NodeId . pack <$> suchThat (listOf (elements ['a' .. 'z'])) (not . null)

-- return $ NodeId $ pack c

instance FromCBOR NodeId where
  fromCBOR = NodeId <$> fromCBOR

instance ToCBOR NodeId where
  toCBOR NodeId{nodeId} = toCBOR nodeId

-- ** Host

-- REVIEW(SN): This is also used in hydra-tui
data Host = Host
  { hostname :: Text
  , port :: PortNumber
  }
  deriving (Ord, Generic, Eq)

instance Show Host where
  show = showHost

instance Read Host where
  readsPrec _ s = case readHost s of
    Just h -> [(h, "")]
    Nothing -> []

instance ToJSON Host where
  toJSON Host{hostname, port} =
    object
      [ "hostname" .= hostname
      , "port" .= toInteger port
      ]

instance FromJSON Host where
  parseJSON = withObject "Host" $ \o -> do
    hostname <- o .: "hostname"
    port <- fromInteger <$> o .: "port"
    pure $ Host{hostname, port}

instance Arbitrary Host where
  arbitrary = do
    ip <- toIPv4w <$> arbitrary
    Host (Text.pack $ show ip) <$> (fromIntegral @Word16 <$> arbitrary)

instance ToCBOR Host where
  toCBOR Host{hostname, port} =
    mconcat
      [ toCBOR hostname
      , toCBOR $ toInteger port
      ]

instance FromCBOR Host where
  fromCBOR =
    Host
      <$> fromCBOR
      <*> (fromInteger <$> fromCBOR)

showHost :: Host -> String
showHost Host{hostname, port} =
  unpack hostname <> ":" <> show port

readHost :: MonadFail m => String -> m Host
readHost s =
  case break (== ':') s of
    (h, ':' : p) -> Host (pack h) <$> readPort p
    _ -> fail $ "readHost: missing : in " <> s

readPort :: MonadFail m => String -> m PortNumber
readPort s =
  case readMaybe s of
    Nothing -> fail "cannot read port"
    Just n
      | n >= minPort && n <= maxPort -> pure $ fromInteger n
      | otherwise ->
        fail $
          "readPort: " <> show n <> " not within valid port range: ("
            <> show minPort
            <> ", "
            <> show maxPort
            <> ")"
 where
  maxPort = fromIntegral (maxBound :: Word16)
  minPort = fromIntegral (minBound :: Word16)
