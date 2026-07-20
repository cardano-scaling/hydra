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
