module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Tx (ConfirmedSnapshot, IsTx (..), TxIdType)

data ClientInput tx
  = Init
  | NewTx {transaction :: tx}
  | Recover {recoverTxId :: TxIdType tx}
  | Decommit {decommitTx :: tx}
  | Close
  | SafeClose
  | Contest
  | Fanout
  | SideLoadSnapshot {snapshot :: ConfirmedSnapshot tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)

instance IsTx tx => ToCBOR (ClientInput tx) where
  toCBOR = \case
    Init -> toCBOR ("Init" :: Text)
    NewTx{transaction} -> toCBOR ("NewTx" :: Text) <> toCBOR transaction
    Recover{recoverTxId} -> toCBOR ("Recover" :: Text) <> toCBOR recoverTxId
    Decommit{decommitTx} -> toCBOR ("Decommit" :: Text) <> toCBOR decommitTx
    Close -> toCBOR ("Close" :: Text)
    SafeClose -> toCBOR ("SafeClose" :: Text)
    Contest -> toCBOR ("Contest" :: Text)
    Fanout -> toCBOR ("Fanout" :: Text)
    SideLoadSnapshot{snapshot} -> toCBOR ("SideLoadSnapshot" :: Text) <> toCBOR snapshot

instance IsTx tx => FromCBOR (ClientInput tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("Init" :: Text) -> pure Init
      "NewTx" -> NewTx <$> fromCBOR
      "Recover" -> Recover <$> fromCBOR
      "Decommit" -> Decommit <$> fromCBOR
      "Close" -> pure Close
      "SafeClose" -> pure SafeClose
      "Contest" -> pure Contest
      "Fanout" -> pure Fanout
      "SideLoadSnapshot" -> SideLoadSnapshot <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded ClientInput"
