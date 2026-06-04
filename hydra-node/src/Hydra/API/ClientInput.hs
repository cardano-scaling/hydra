module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Tx (ConfirmedSnapshot, IsTx (..), Party, TxIdType)
import Hydra.Tx.OnChainId (OnChainId)

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
  | -- | Propose a new party to join the open head. Sent by one of the
    -- existing parties (the inviter). Triggers a 'ReqAddParty' broadcast
    -- which, once unanimously signed (including by the joining party
    -- themselves), is finalized by an 'UpdateParameters' L1 transaction.
    -- See issue #1813 (dynamic-head-participants, Phase 2).
    --
    -- 'joiningHost' is the network host ('hostname:port') the new party's
    -- hydra-node listens on. Existing nodes use it to run 'etcdctl member
    -- add' against their L2 network mesh once 'JoinFinalized' fires.
    AddParticipant {joiningParty :: Party, joiningOnChainId :: OnChainId, joiningHost :: Text}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)
