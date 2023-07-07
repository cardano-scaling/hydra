{-# OPTIONS_GHC -Wno-orphans #-}
module Hydra.Cardano.Api.TxId where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Binary (ToCBOR(..), FromCBOR (..))

-- missing CBOR instances

instance ToCBOR TxId where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR TxId where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes AsTxId bs of
      Left err -> fail (show err)
      Right v -> pure v

-- * Type Conversions

-- | Convert a cardano-api 'TxId' into a cardano-ledger 'TxId'.
toLedgerTxId :: TxId -> Ledger.TxId StandardCrypto
toLedgerTxId (TxId h) =
  Ledger.TxId (Ledger.unsafeMakeSafeHash (CC.castHash h))

-- | Convert a cardano-ledger 'TxId' into a cardano-api 'TxId'.
fromLedgerTxId :: Ledger.TxId StandardCrypto -> TxId
fromLedgerTxId (Ledger.TxId h) =
  TxId (CC.castHash (Ledger.extractHash h))
