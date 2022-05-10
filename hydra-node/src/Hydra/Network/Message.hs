{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.Message where

import Hydra.Prelude

import qualified Hydra.Crypto as Hydra
import Hydra.Ledger (IsTx, UTxOType)
import Hydra.Network (Host)
import Hydra.Party (Party)
import Hydra.Snapshot (Snapshot, SnapshotNumber)

-- NOTE(SN): Every message comes from a 'Party', we might want to move it out of
-- here into the 'NetworkEvent'
data Message tx
  = ReqTx {party :: Party, transaction :: tx}
  | ReqSn {party :: Party, snapshotNumber :: SnapshotNumber, transactions :: [tx]}
  | AckSn {party :: Party, signed :: Hydra.Signature (Snapshot tx), snapshotNumber :: SnapshotNumber}
  | Connected {peer :: Host}
  | Disconnected {peer :: Host}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance IsTx tx => Arbitrary (Message tx) where
  arbitrary = genericArbitrary

instance (ToCBOR tx, ToCBOR (UTxOType tx)) => ToCBOR (Message tx) where
  toCBOR = \case
    ReqTx party tx -> toCBOR ("ReqTx" :: Text) <> toCBOR party <> toCBOR tx
    ReqSn party sn txs -> toCBOR ("ReqSn" :: Text) <> toCBOR party <> toCBOR sn <> toCBOR txs
    AckSn party sig sn -> toCBOR ("AckSn" :: Text) <> toCBOR party <> toCBOR sig <> toCBOR sn
    Connected host -> toCBOR ("Connected" :: Text) <> toCBOR host
    Disconnected host -> toCBOR ("Disconnected" :: Text) <> toCBOR host

instance (FromCBOR tx, FromCBOR (UTxOType tx)) => FromCBOR (Message tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR <*> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "Connected" -> Connected <$> fromCBOR
      "Disconnected" -> Disconnected <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"
