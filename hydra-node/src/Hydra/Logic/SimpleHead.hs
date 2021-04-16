module Hydra.Logic.SimpleHead where

import Cardano.Prelude

data Event
  = NewTxFromClient

  | NewSnFromSomewhere

  | ReqTxFromPeer

  | AckTxFromPeer
  | ConfTxFromPeer

  | ReqSnFromPeer
  | AckSnFromPeer
  | ConfSnFromPeer

data State = State
  { ledger :: Ledger
  , transactions :: Transactions
  , snapshots :: Snapshots
  }

data Ledger
data Transactions
data Snapshots

data Effect
  = MulticastReqTx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State -> Maybe (State, [Effect]))

update :: State -> Event -> (State, [Effect])
update = panic "TODO"
