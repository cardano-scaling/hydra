{-# LANGUAGE UndecidableInstances #-}

module Hydra.Logic.SimpleHead where

import           Cardano.Prelude         hiding ( State )

import           Hydra.Ledger                   ( LedgerState )

data Event tx
  = NewTxFromClient tx
  | ReqTxFromPeer
  | AckTxFromPeer
  | ConfTxFromPeer
  | ReqSnFromPeer
  | AckSnFromPeer
  | ConfSnFromPeer

data State tx = State
  { confirmedLedger :: LedgerState tx
  , transactions    :: Transactions
  , snapshots       :: Snapshots
  }

deriving instance Eq (LedgerState tx) => Eq (State tx)
deriving instance Show (LedgerState tx) => Show (State tx)

mkState :: LedgerState tx -> State tx
mkState mkLedgerState = State mkLedgerState Transaction Snapshots

data Transactions = Transaction
  deriving (Eq, Show)
data Snapshots = Snapshots
  deriving (Eq, Show)

data Effect tx
  = MulticastReqTx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State tx -> Maybe (State tx, [Effect tx]))

update :: State tx -> Event tx -> (State tx, [Effect tx])
update st = \case
  NewTxFromClient _tx -> (st, [MulticastReqTx])
  _                   -> panic "SimpleHead.TODO"
