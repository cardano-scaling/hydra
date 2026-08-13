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
      }
  | ReqDec {transaction :: tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Message tx)
deriving stock instance IsTx tx => Show (Message tx)
deriving anyclass instance IsTx tx => ToJSON (Message tx)
deriving anyclass instance IsTx tx => FromJSON (Message tx)

instance (ToCBOR tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx)) => ToCBOR (Message tx) where
  toCBOR = genericToCBOR

instance (FromCBOR tx, FromCBOR (UTxOType tx), FromCBOR (TxIdType tx)) => FromCBOR (Message tx) where
  fromCBOR = genericFromCBOR

instance IsTx tx => SignableRepresentation (Message tx) where
  getSignableRepresentation = serialize'
