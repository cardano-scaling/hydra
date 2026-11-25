-- | Persisted deposit data. Machine continuations are reconstructed from these
-- fields and never form part of the JSON or CBOR representation.
module Hydra.Node.Deposit where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx (..))

-- | A deposit tracked by the protocol. The 'DepositStatus' determines whether
-- it may be used for an incremental commit or not.
data Deposit tx = Deposit
  { headId :: HeadId
  , deposited :: UTxOType tx
  , created :: UTCTime
  , deadline :: UTCTime
  , status :: DepositStatus
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Deposit tx)
deriving stock instance IsTx tx => Show (Deposit tx)
deriving anyclass instance IsTx tx => ToJSON (Deposit tx)
deriving anyclass instance IsTx tx => FromJSON (Deposit tx)

instance IsTx tx => ToCBOR (Deposit tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (Deposit tx) where
  fromCBOR = genericFromCBOR

data DepositStatus = Inactive | Active | Expired
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR DepositStatus where
  toCBOR = genericToCBOR

instance FromCBOR DepositStatus where
  fromCBOR = genericFromCBOR

-- | A deposit with its L1 lifecycle slots. Deposits are L1-derived state, so a
-- rollback must rewind the view (the node rewinds its deposit collection): a deposit recorded after
-- the rolled-back slot vanishes (its deposit transaction was erased), and a
-- consumption after it is undone (the erased increment or recover resurfaces
-- the deposit). Forward re-observation of the new chain then converges the view
-- again. Only L1-derived state may rewind like this; L2 state (snapshots,
-- signatures) never rolls back.
data TrackedDeposit tx = TrackedDeposit
  { deposit :: Deposit tx
  , recordedAt :: ChainSlot
  -- ^ Slot at which the deposit transaction was observed.
  , consumedAt :: Maybe ChainSlot
  -- ^ Slot at which a consuming transaction (increment or recover) was
  -- observed, if any. A consumed deposit is no longer pending, but is retained
  -- for the retention horizon so a rollback can resurface it.
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (TrackedDeposit tx)
deriving stock instance IsTx tx => Show (TrackedDeposit tx)
deriving anyclass instance IsTx tx => ToJSON (TrackedDeposit tx)
deriving anyclass instance IsTx tx => FromJSON (TrackedDeposit tx)

instance IsTx tx => ToCBOR (TrackedDeposit tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (TrackedDeposit tx) where
  fromCBOR = genericFromCBOR
