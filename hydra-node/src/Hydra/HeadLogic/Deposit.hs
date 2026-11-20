{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.Deposit where

import Hydra.Chain.ChainState (ChainSlot)
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx (..))

data DepositStatus = Inactive | Active | Expired
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR DepositStatus where
  toCBOR = genericToCBOR

instance FromCBOR DepositStatus where
  fromCBOR = genericFromCBOR

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

data TrackedDeposit tx = TrackedDeposit
  { deposit :: Deposit tx
  , recordedAt :: ChainSlot
  , consumedAt :: Maybe ChainSlot
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

record ::
  ChainSlot ->
  Deposit tx ->
  Maybe (TrackedDeposit tx) ->
  Maybe (TrackedDeposit tx)
record recordedAt deposit _ = Just TrackedDeposit{deposit, recordedAt, consumedAt = Nothing}

update ::
  Deposit tx ->
  Maybe (TrackedDeposit tx) ->
  Maybe (TrackedDeposit tx)
update deposit = fmap $ \tracked -> tracked{deposit}

consume ::
  ChainSlot ->
  Maybe (TrackedDeposit tx) ->
  Maybe (TrackedDeposit tx)
consume slot = fmap $ \tracked -> tracked{consumedAt = Just slot}

rollback ::
  ChainSlot ->
  Maybe (TrackedDeposit tx) ->
  Maybe (TrackedDeposit tx)
rollback slot = (>>= rewind)
 where
  rewind tracked
    | tracked.recordedAt > slot = Nothing
    | otherwise = Just tracked{consumedAt = mfilter (<= slot) tracked.consumedAt}
