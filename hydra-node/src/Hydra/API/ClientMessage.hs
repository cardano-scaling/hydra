{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientMessage where

import Hydra.Prelude

import Codec.CBOR.Decoding (Decoder)
import Data.Aeson (defaultOptions, genericParseJSON, genericToJSON, omitNothingFields)
import Hydra.API.ClientInput (ClientInput)
import Hydra.Chain (PostChainTx, PostTxError)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic.Error (SideLoadRequirementFailure)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ValidationError)
import Hydra.Network (StallReason)
import Hydra.Tx (IsTx (..))

data DecommitInvalidReason tx
  = DecommitTxInvalid {localUTxO :: UTxOType tx, validationError :: ValidationError}
  | DecommitAlreadyInFlight {otherDecommitTxId :: TxIdType tx}
  | -- | A deposit (commit) is in flight: the decommit cannot be recorded until it
    -- finalises or is recovered. Clients may need to recover the deposit before
    -- another decommit is possible ('currentDepositTxId' clears on 'CommitFinalized'
    -- and 'DepositRecovered').
    DepositInFlight {depositTxId :: TxIdType tx, commitUTxO :: UTxOType tx}
  deriving stock (Generic)

deriving stock instance (Eq (TxIdType tx), Eq (UTxOType tx)) => Eq (DecommitInvalidReason tx)
deriving stock instance (Show (TxIdType tx), Show (UTxOType tx)) => Show (DecommitInvalidReason tx)

instance (ToJSON (TxIdType tx), ToJSON (UTxOType tx)) => ToJSON (DecommitInvalidReason tx) where
  toJSON = genericToJSON defaultOptions

instance (FromJSON (TxIdType tx), FromJSON (UTxOType tx)) => FromJSON (DecommitInvalidReason tx) where
  parseJSON = genericParseJSON defaultOptions

instance IsTx tx => ToCBOR (DecommitInvalidReason tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (DecommitInvalidReason tx) where
  fromCBOR = genericFromCBOR

-- | Individual messages as produced by the 'Hydra.HeadLogic' in
-- the 'ClientEffect'.
data ClientMessage tx
  = CommandFailed {clientInput :: ClientInput tx, state :: HeadState tx}
  | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  | RejectedInputBecauseUnsynced {clientInput :: ClientInput tx, drift :: NominalDiffTime}
  | -- | A 'NewTx' or 'Decommit' was refused because the node's outbound
    -- broadcast queue has been backed up for a while: the message it would
    -- produce cannot be handed to the network. Chain and protocol processing
    -- are unaffected, and 'Close', 'Contest' and 'Fanout' always work. Retry
    -- once the network recovers.
    --
    -- 'stallReason' tells apart the two causes 'pendingBroadcasts' alone
    -- cannot: 'NoProgress' means the network really is unreachable, while
    -- 'BacklogFull' can mean a healthy network simply being outrun by a fast
    -- client.
    RejectedInputBecauseBroadcastStalled {clientInput :: ClientInput tx, pendingBroadcasts :: Natural, stallReason :: StallReason}
  | SideLoadSnapshotRejected {clientInput :: ClientInput tx, requirementFailure :: SideLoadRequirementFailure tx}
  deriving stock (Eq, Show, Generic)

instance IsChainState tx => ToJSON (ClientMessage tx) where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True
        }

instance IsChainState tx => FromJSON (ClientMessage tx) where
  parseJSON =
    genericParseJSON
      defaultOptions
        { omitNothingFields = True
        }

instance IsChainState tx => ToCBOR (ClientMessage tx) where
  toCBOR = genericToCBOR

instance IsChainState tx => FromCBOR (ClientMessage tx) where
  fromCBOR = fromCBOR >>= decodeClientMessageBody

-- | Decode a 'ClientMessage' given its already-decoded constructor tag (used
-- for tag-based dispatch in 'ApiMessage').
decodeClientMessageBody :: IsChainState tx => Text -> Decoder s (ClientMessage tx)
decodeClientMessageBody = \case
  "CommandFailed" -> CommandFailed <$> fromCBOR <*> fromCBOR
  "PostTxOnChainFailed" -> PostTxOnChainFailed <$> fromCBOR <*> fromCBOR
  "RejectedInputBecauseUnsynced" -> RejectedInputBecauseUnsynced <$> fromCBOR <*> fromCBOR
  "RejectedInputBecauseBroadcastStalled" -> RejectedInputBecauseBroadcastStalled <$> fromCBOR <*> fromCBOR <*> fromCBOR
  "SideLoadSnapshotRejected" -> SideLoadSnapshotRejected <$> fromCBOR <*> fromCBOR
  tag -> fail $ show tag <> " is not a proper CBOR-encoded ClientMessage"
