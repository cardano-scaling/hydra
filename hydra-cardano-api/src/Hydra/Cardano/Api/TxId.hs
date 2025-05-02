{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.TxId where

import Hydra.Cardano.Api.Prelude

import Cardano.Crypto.Hash.Class qualified as CC
import Cardano.Ledger.Hashes qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger

-- * Type Conversions

-- | Convert a cardano-api 'TxId' into a cardano-ledger 'TxId'.
toLedgerTxId :: TxId -> Ledger.TxId
toLedgerTxId (TxId h) =
  Ledger.TxId (Ledger.unsafeMakeSafeHash (CC.castHash h))
