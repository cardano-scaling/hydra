module Hydra.Logic.SimpleHead where

import Cardano.Prelude hiding (State)

import Hydra.Ledger (
  LedgerState (LedgerState),
 )

data Event
  = ReqTxFromPeer
  | AckTxFromPeer
  | ConfTxFromPeer
  | ReqSnFromPeer
  | AckSnFromPeer
  | ConfSnFromPeer

data State tx = State
  { confirmedLedger :: LedgerState tx
  , transactions :: Transactions
  , snapshots :: Snapshots
  }
  deriving (Eq, Show)

mkState :: State tx
mkState = State LedgerState Transaction Snapshots

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
