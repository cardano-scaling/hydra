{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.Message where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation, getSignableRepresentation)
import Hydra.Chain.ChainState (ChainPointType, IsChainState)
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

data NetworkEvent msg
  = ConnectivityEvent Connectivity
  | ReceivedMessage {sender :: Party, msg :: msg}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
      , pendingDepositVersion :: Maybe SnapshotVersion
      , pendingDecommitVersion :: Maybe SnapshotVersion
      }
  | ReqDec {transaction :: tx}
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainPointType tx)) => Eq (Message tx)
deriving stock instance (IsTx tx, Show (ChainPointType tx)) => Show (Message tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainPointType tx)) => ToJSON (Message tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainPointType tx)) => FromJSON (Message tx)

instance (IsChainState tx, ToCBOR tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx), ToCBOR (ChainPointType tx)) => ToCBOR (Message tx) where
  toCBOR = \case
    ReqTx tx -> toCBOR ("ReqTx" :: Text) <> toCBOR tx
    ReqSn sv sn txs decommitTx incrementUTxO -> toCBOR ("ReqSn" :: Text) <> toCBOR sv <> toCBOR sn <> toCBOR txs <> toCBOR decommitTx <> toCBOR incrementUTxO
    AckSn sig sn pdv pddv -> toCBOR ("AckSn" :: Text) <> toCBOR sig <> toCBOR sn <> toCBOR pdv <> toCBOR pddv
    ReqDec utxo -> toCBOR ("ReqDec" :: Text) <> toCBOR utxo

instance (IsChainState tx, FromCBOR tx, FromCBOR (UTxOType tx), FromCBOR (TxIdType tx), FromCBOR (ChainPointType tx)) => FromCBOR (Message tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "ReqDec" -> ReqDec <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance (IsChainState tx, ToCBOR tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx), ToCBOR (ChainPointType tx)) => SignableRepresentation (Message tx) where
  getSignableRepresentation = serialize'
