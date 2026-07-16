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
  | -- | Fan out a user-selected subset of the closed head's UTxO. Unlike
    -- 'Fanout' (which drains the whole set automatically), this distributes only
    -- 'utxoToFanout' and then waits for the next 'PartialFanout' command. Once a
    -- partial fanout has started, 'Fanout' is no longer accepted; the user keeps
    -- issuing 'PartialFanout' until the head is drained, at which point the node
    -- automatically produces the final fanout that burns the head tokens.
    PartialFanout {utxoToFanout :: UTxOType tx}
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
    PartialFanout{utxoToFanout} -> toCBOR ("PartialFanout" :: Text) <> toCBOR utxoToFanout
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
      "PartialFanout" -> PartialFanout <$> fromCBOR
      "SideLoadSnapshot" -> SideLoadSnapshot <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded ClientInput"
