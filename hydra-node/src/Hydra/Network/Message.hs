{-# LANGUAGE UndecidableInstances #-}

module Hydra.Network.Message where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation, getSignableRepresentation)
import Hydra.Cardano.Api (AsType (..), deserialiseFromRawBytes, serialiseToRawBytes)
import Hydra.Network (Connectivity)
import Hydra.Tx (
  IsTx (TxIdType),
  ParameterUpdate,
  Party,
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
  UTxOType,
 )
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.OnChainId (AsType (..), OnChainId)

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
      , parameterUpdate :: Maybe ParameterUpdate
      -- ^ Pending parameter change to multi-sign together with this snapshot.
      -- See 'Hydra.Tx.ParameterUpdate'.
      }
  | AckSn
      { signed :: Signature (Snapshot tx)
      , snapshotNumber :: SnapshotNumber
      }
  | ReqDec {transaction :: tx}
  | -- | Pre-confirmation broadcast that a 'Party' is requesting to leave the
    -- open head. Mirrors 'ReqDec' for decommit. Finalization happens via
    -- 'ReqSn'/'AckSn' on a snapshot whose 'parameterUpdate' is set.
    --
    -- The leaver's 'OnChainId' is carried alongside the 'Party' so that
    -- every node can build the same 'ParameterUpdate' / 'OnChainParameterUpdate'
    -- bytes (and, in particular, the same signable snapshot) without needing
    -- access to the leaver's 'Environment'.
    ReqLeave {leavingParty :: Party, leavingOnChainId :: OnChainId}
  | -- | Pre-confirmation broadcast that a new 'Party' (carrying their
    -- 'OnChainId') is being proposed for inclusion in the head. Broadcast
    -- by one of the existing parties (the inviter); finalization happens
    -- via 'ReqSn'/'AckSn' on a snapshot whose 'parameterUpdate' is
    -- 'AddParty'. The joining party themselves must also sign that
    -- snapshot — see the speculative-accept rule in 'Authenticate'.
    ReqAddParty {joiningParty :: Party, joiningOnChainId :: OnChainId}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Message tx)
deriving stock instance IsTx tx => Show (Message tx)
deriving anyclass instance IsTx tx => ToJSON (Message tx)
deriving anyclass instance IsTx tx => FromJSON (Message tx)

instance (ToCBOR tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx)) => ToCBOR (Message tx) where
  toCBOR = \case
    ReqTx tx -> toCBOR ("ReqTx" :: Text) <> toCBOR tx
    ReqSn sv sn txs decommitTx incrementUTxO parameterUpdate ->
      toCBOR ("ReqSn" :: Text)
        <> toCBOR sv
        <> toCBOR sn
        <> toCBOR txs
        <> toCBOR decommitTx
        <> toCBOR incrementUTxO
        <> toCBOR parameterUpdate
    AckSn sig sn -> toCBOR ("AckSn" :: Text) <> toCBOR sig <> toCBOR sn
    ReqDec utxo -> toCBOR ("ReqDec" :: Text) <> toCBOR utxo
    ReqLeave p oid -> toCBOR ("ReqLeave" :: Text) <> toCBOR p <> toCBOR (serialiseToRawBytes oid)
    ReqAddParty p oid -> toCBOR ("ReqAddParty" :: Text) <> toCBOR p <> toCBOR (serialiseToRawBytes oid)

instance (FromCBOR tx, FromCBOR (UTxOType tx), FromCBOR (TxIdType tx)) => FromCBOR (Message tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR
      "ReqDec" -> ReqDec <$> fromCBOR
      "ReqLeave" -> do
        p <- fromCBOR
        oidBytes <- fromCBOR
        case deserialiseFromRawBytes AsOnChainId oidBytes of
          Right oid -> pure $ ReqLeave p oid
          Left err -> fail $ "ReqLeave: invalid OnChainId: " <> show err
      "ReqAddParty" -> do
        p <- fromCBOR
        oidBytes <- fromCBOR
        case deserialiseFromRawBytes AsOnChainId oidBytes of
          Right oid -> pure $ ReqAddParty p oid
          Left err -> fail $ "ReqAddParty: invalid OnChainId: " <> show err
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance IsTx tx => SignableRepresentation (Message tx) where
  getSignableRepresentation = serialize'
