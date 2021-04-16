module Hydra.Logic.SimpleHead where

import Cardano.Prelude hiding (State)

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

mkState :: State
mkState = State Ledger Transaction Snapshots

data Ledger = Ledger
data Transactions = Transaction
data Snapshots = Snapshots

data Effect
  = MulticastReqTx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State -> Maybe (State, [Effect]))

update :: State -> Event -> (State, [Effect])
update st NewTxFromClient = (st, [MulticastReqTx])
update _ _ = panic "TODO"
