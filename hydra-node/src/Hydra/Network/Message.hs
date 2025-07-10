{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.Message where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation, getSignableRepresentation)
import Hydra.Network (Connectivity)
import Hydra.Tx (
  IsTx (TxIdType),
  Party,
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
  UTxOType,
 )
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.IsTx (ArbitraryIsTx)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

data NetworkEvent msg
  = ConnectivityEvent Connectivity
  | ReceivedMessage {sender :: Party, msg :: msg}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving anyclass (FromJSON)

instance ToCBOR msg => ToCBOR (NetworkEvent msg) where
  toCBOR = \case
    ConnectivityEvent connectivity -> toCBOR ("ConnectivityEvent" :: Text) <> toCBOR connectivity
    ReceivedMessage sender msg -> toCBOR ("ReceivedMessage" :: Text) <> toCBOR sender <> toCBOR msg

instance FromCBOR msg => FromCBOR (NetworkEvent msg) where
  fromCBOR =
    fromCBOR >>= \case
      ("ConnectivityEvent" :: Text) -> ConnectivityEvent <$> fromCBOR
      "ReceivedMessage" -> ReceivedMessage <$> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded NetworkEvent"

instance Arbitrary msg => Arbitrary (NetworkEvent msg) where
  arbitrary = genericArbitrary

data Message tx
  = ReqTx {transaction :: tx}
  | ReqSn
      { snapshotVersion :: SnapshotVersion
      , snapshotNumber :: SnapshotNumber
      , transactionIds :: [TxIdType tx]
      , decommitTx :: Maybe tx
      , depositTxId :: Maybe (TxIdType tx)
      }
  | AckSn
      { signed :: Signature (Snapshot tx)
      , snapshotNumber :: SnapshotNumber
      }
  | ReqDec {transaction :: tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Message tx)
deriving stock instance IsTx tx => Show (Message tx)
deriving anyclass instance IsTx tx => ToJSON (Message tx)
deriving anyclass instance IsTx tx => FromJSON (Message tx)

instance ArbitraryIsTx tx => Arbitrary (Message tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => ToADTArbitrary (Message tx)

instance (ToCBOR tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx)) => ToCBOR (Message tx) where
  toCBOR = \case
    ReqTx tx -> toCBOR ("ReqTx" :: Text) <> toCBOR tx
    ReqSn sv sn txs decommitTx incrementUTxO -> toCBOR ("ReqSn" :: Text) <> toCBOR sv <> toCBOR sn <> toCBOR txs <> toCBOR decommitTx <> toCBOR incrementUTxO
    AckSn sig sn -> toCBOR ("AckSn" :: Text) <> toCBOR sig <> toCBOR sn
    ReqDec utxo -> toCBOR ("ReqDec" :: Text) <> toCBOR utxo

instance (FromCBOR tx, FromCBOR (UTxOType tx), FromCBOR (TxIdType tx)) => FromCBOR (Message tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR
      "ReqDec" -> ReqDec <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance IsTx tx => SignableRepresentation (Message tx) where
  getSignableRepresentation = serialize'
