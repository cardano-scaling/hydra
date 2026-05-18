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
  | -- | Request to leave the open head. A node may only request its own
    -- departure (the off-chain handler validates this). Triggers a
    -- 'ReqLeave' broadcast which, once unanimously signed into a snapshot,
    -- is finalized by an 'UpdateParameters' L1 transaction. See issue
    -- #1813 (dynamic-head-participants).
    Leave
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)
