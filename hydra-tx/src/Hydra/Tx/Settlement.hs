{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Evidence needed to repeat an increment or decrement after a rollback.
-- Unlike a full snapshot, this does not retain the head UTxO or accumulators.
-- It is internal settlement evidence, not a replacement for the full snapshot
-- accepted by SideLoadSnapshot or used for close, contest and fanout.
module Hydra.Tx.Settlement where

import Hydra.Prelude

import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import Data.ByteString.Base16 qualified as Base16
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, confirmedSignatures, getSnapshot, hashCommitOutputs)

-- | Keep the pending outputs, whose values and order the transaction builders
-- need, but replace the two full accumulators with their signed hashes.
-- Strict fields ensure no hash thunk keeps an old accumulator alive.
data SettlementEvidence tx = SettlementEvidence
  { headId :: HeadId
  , version :: SnapshotVersion
  , number :: SnapshotNumber
  , utxoToCommit :: Maybe (UTxOType tx)
  , utxoToDecommit :: Maybe (UTxOType tx)
  , depositTxId :: Maybe (TxIdType tx)
  , accumulatorHash :: ByteString
  , appliedAccumulatorHash :: ByteString
  , signatures :: MultiSignature (Snapshot tx)
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (SettlementEvidence tx)
deriving stock instance IsTx tx => Show (SettlementEvidence tx)

snapshotEvidence :: Snapshot tx -> MultiSignature (Snapshot tx) -> SettlementEvidence tx
snapshotEvidence Snapshot{headId, version, number, utxoToCommit, utxoToDecommit, depositTxId, accumulator, appliedAccumulator} signatures =
  SettlementEvidence
    { headId
    , version
    , number
    , utxoToCommit
    , utxoToDecommit
    , depositTxId
    , accumulatorHash = Accumulator.getAccumulatorHash accumulator
    , appliedAccumulatorHash = Accumulator.getAccumulatorHash appliedAccumulator
    , signatures
    }

settlementEvidence :: IsTx tx => ConfirmedSnapshot tx -> SettlementEvidence tx
settlementEvidence confirmed = snapshotEvidence (getSnapshot confirmed) (confirmedSignatures confirmed)

instance IsTx tx => ToJSON (SettlementEvidence tx) where
  toJSON SettlementEvidence{headId, version, number, utxoToCommit, utxoToDecommit, depositTxId, accumulatorHash, appliedAccumulatorHash, signatures} =
    object
      [ "headId" .= headId
      , "version" .= version
      , "number" .= number
      , "utxoToCommit" .= utxoToCommit
      , "utxoToDecommit" .= utxoToDecommit
      , "depositTxId" .= depositTxId
      , "accumulatorHash" .= String (decodeUtf8 (Base16.encode accumulatorHash))
      , "appliedAccumulatorHash" .= String (decodeUtf8 (Base16.encode appliedAccumulatorHash))
      , "signatures" .= signatures
      ]

instance IsTx tx => FromJSON (SettlementEvidence tx) where
  parseJSON value = compact value <|> (settlementEvidence <$> parseJSON value)
   where
    compact = withObject "SettlementEvidence" $ \obj -> do
      headId <- obj .: "headId"
      version <- obj .: "version"
      number <- obj .: "number"
      utxoToCommit <- obj .: "utxoToCommit"
      utxoToDecommit <- obj .: "utxoToDecommit"
      depositTxId <- obj .: "depositTxId"
      accumulatorHash <- obj .: "accumulatorHash" >>= decodeBase16
      appliedAccumulatorHash <- obj .: "appliedAccumulatorHash" >>= decodeBase16
      signatures <- obj .: "signatures"
      pure SettlementEvidence{headId, version, number, utxoToCommit, utxoToDecommit, depositTxId, accumulatorHash, appliedAccumulatorHash, signatures}

instance IsTx tx => ToCBOR (SettlementEvidence tx) where
  toCBOR = genericToCBOR

-- Accept the previous ConfirmedSnapshot payload in retained settlements and
-- persisted PostChainTx values. New writes use the distinct constructor tag.
instance IsTx tx => FromCBOR (SettlementEvidence tx) where
  fromCBOR = do
    tag <- fromCBOR
    case (tag :: Text) of
      "SettlementEvidence" -> SettlementEvidence <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR
      "ConfirmedSnapshot" -> snapshotEvidence <$> fromCBOR <*> fromCBOR
      "InitialSnapshot" -> settlementEvidence . InitialSnapshot <$> fromCBOR
      _ -> fail $ "Unknown settlement evidence tag: " <> show tag

commitOutputsHash :: forall tx. IsTx tx => SettlementEvidence tx -> ByteString
commitOutputsHash SettlementEvidence{utxoToCommit, depositTxId} = hashCommitOutputs @tx utxoToCommit depositTxId
