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
  deriving (Eq, Show)

mkState :: State
mkState = State Ledger Transaction Snapshots

data Ledger = Ledger deriving (Eq, Show)
data Transactions = Transaction deriving (Eq, Show)
data Snapshots = Snapshots deriving (Eq, Show)

data Effect
  = MulticastReqTx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State -> Maybe (State, [Effect]))

update :: State -> Event -> (State, [Effect])
update st NewTxFromClient = (st, [MulticastReqTx])
update _ _ = panic "TODO"
