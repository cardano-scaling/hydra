module Hydra.Logic.SimpleHead where

import Cardano.Prelude hiding (State)

import Hydra.Ledger (Ledger)

data Event
  = ReqTxFromPeer
  | AckTxFromPeer
  | ConfTxFromPeer
  | ReqSnFromPeer
  | AckSnFromPeer
  | ConfSnFromPeer

data State tx = State
  { confirmedLedger :: Ledger tx
  , transactions :: Transactions
  , snapshots :: Snapshots
  }

mkState :: Ledger tx -> State tx
mkState ledger = State ledger Transaction Snapshots

data Transactions = Transaction deriving (Eq, Show)
data Snapshots = Snapshots deriving (Eq, Show)

data Effect tx
  = MulticastReqTx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State tx -> Maybe (State tx, [Effect tx]))

update :: State tx -> Event -> (State tx, [Effect tx])
update _ _ = panic "TODO"
