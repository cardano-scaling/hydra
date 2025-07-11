{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Tx (ConfirmedSnapshot, IsTx (..), TxIdType)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

data ClientInput tx
  = Init
  | Abort
  | NewTx {transaction :: tx}
  | Recover {recoverTxId :: TxIdType tx}
  | Decommit {decommitTx :: tx}
  | Close
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
    Abort -> toCBOR ("Abort" :: Text)
    NewTx tx -> toCBOR ("NewTx" :: Text) <> toCBOR tx
    Recover txid -> toCBOR ("Recover" :: Text) <> toCBOR txid
    Decommit tx -> toCBOR ("Decommit" :: Text) <> toCBOR tx
    Close -> toCBOR ("Close" :: Text)
    Contest -> toCBOR ("Contest" :: Text)
    Fanout -> toCBOR ("Fanout" :: Text)
    SideLoadSnapshot snapshot -> toCBOR ("SideLoadSnapshot" :: Text) <> toCBOR snapshot

instance IsTx tx => FromCBOR (ClientInput tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("Init" :: Text) -> pure Init
      ("Abort" :: Text) -> pure Abort
      ("NewTx" :: Text) -> NewTx <$> fromCBOR
      ("Recover" :: Text) -> Recover <$> fromCBOR
      ("Decommit" :: Text) -> Decommit <$> fromCBOR
      ("Close" :: Text) -> pure Close
      ("Contest" :: Text) -> pure Contest
      ("Fanout" :: Text) -> pure Fanout
      ("SideLoadSnapshot" :: Text) -> SideLoadSnapshot <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance (Arbitrary tx, Arbitrary (TxIdType tx), Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init -> []
    Abort -> []
    NewTx tx -> NewTx <$> shrink tx
    Recover tx -> Recover <$> shrink tx
    Decommit tx -> Decommit <$> shrink tx
    Close -> []
    Contest -> []
    Fanout -> []
    SideLoadSnapshot sn -> SideLoadSnapshot <$> shrink sn

instance (Arbitrary tx, Arbitrary (TxIdType tx), Arbitrary (UTxOType tx), IsTx tx) => ToADTArbitrary (ClientInput tx)
