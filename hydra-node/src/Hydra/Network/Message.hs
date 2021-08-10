{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.Message where

import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Hydra.Ledger (Party, Signed, Tx, UTxO)
import Hydra.Prelude
import Hydra.Snapshot (Snapshot, SnapshotNumber)

-- NOTE(SN): Every message comes from a 'Party', we might want to move it out of
-- here into the 'NetworkEvent'
data Message tx
  = ReqTx Party tx
  | ReqSn Party SnapshotNumber [tx]
  | AckSn Party (Signed (Snapshot tx)) SnapshotNumber
  | Connected Party
  | Disconnected Party
  deriving stock (Generic, Eq, Show)

instance Arbitrary tx => Arbitrary (Message tx) where
  arbitrary = genericArbitrary

instance (ToJSON tx, ToJSON (UTxO tx), Tx tx) => ToJSON (Message tx) where
  toJSON = \case
    (ReqTx party tx) ->
      object ["tag" .= s "reqTx", "party" .= party, "transaction" .= tx]
    (ReqSn party sn txs) ->
      object ["tag" .= s "reqSn", "party" .= party, "snapshot" .= sn, "transactions" .= txs]
    (AckSn party sig nat) ->
      object ["tag" .= s "ackSn", "party" .= party, "signature" .= sig, "snapshotNumber" .= nat]
    (Connected party) ->
      object ["tag" .= s "connected", "party" .= party]
    (Disconnected party) ->
      object ["tag" .= s "disconnected", "party" .= party]
   where
    s = Aeson.String

instance (FromJSON tx, FromJSON (UTxO tx), Tx tx) => FromJSON (Message tx) where
  parseJSON = withObject "Message" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      "reqTx" -> ReqTx <$> obj .: "party" <*> obj .: "transaction"
      "reqSn" -> ReqSn <$> obj .: "party" <*> obj .: "snapshot" <*> obj .: "transactions"
      "ackSn" -> AckSn <$> obj .: "party" <*> obj .: "signature" <*> obj .: "snapshotNumber"
      "connected" -> Connected <$> obj .: "party"
      "disconnected" -> Disconnected <$> obj .: "party"
      t -> fail $ "unknown tag" <> t

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (Message tx) where
  toCBOR = \case
    ReqTx party tx -> toCBOR ("ReqTx" :: Text) <> toCBOR party <> toCBOR tx
    ReqSn party sn txs -> toCBOR ("ReqSn" :: Text) <> toCBOR party <> toCBOR sn <> toCBOR txs
    AckSn party sig sn -> toCBOR ("AckSn" :: Text) <> toCBOR party <> toCBOR sig <> toCBOR sn
    Connected host -> toCBOR ("Connected" :: Text) <> toCBOR host
    Disconnected host -> toCBOR ("Disconnected" :: Text) <> toCBOR host

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (Message tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR <*> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "Connected" -> Connected <$> fromCBOR
      "Disconnected" -> Disconnected <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

getParty :: Message msg -> Party
getParty =
  \case
    (ReqTx p _) -> p
    (ReqSn p _ _) -> p
    (AckSn p _ _) -> p
    (Connected p) -> p
    (Disconnected p) -> p
