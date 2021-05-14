{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Logic.SimpleHead where

import Cardano.Prelude hiding (State)

import Hydra.Ledger (Ledger, LedgerState, applyTransaction)

data Event tx
  = NewTxFromClient tx
  | ReqTxFromPeer tx
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

deriving instance Eq (LedgerState tx) => Eq (State tx)
deriving instance Show (LedgerState tx) => Show (State tx)

mkState :: LedgerState tx -> State tx
mkState mkLedgerState = State mkLedgerState Transaction Snapshots

data Transactions = Transaction
  deriving (Eq, Show)
data Snapshots = Snapshots
  deriving (Eq, Show)

data Effect tx
  = MulticastReqTx tx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State tx -> Maybe (State tx, [Effect tx]))

update :: Show tx => Ledger tx -> State tx -> Event tx -> (State tx, [Effect tx])
update ledger st = \case
  NewTxFromClient tx -> (st, [MulticastReqTx tx])
  ReqTxFromPeer tx ->
    trace @Text ("applying transaction " <> show tx) $
      case applyTransaction ledger (confirmedLedger st) tx of
        Left err -> panic $ "applying invalid transaction " <> show tx <> " to ledger: " <> show err
        Right newLedgerState -> (st{confirmedLedger = newLedgerState}, [])
  _ -> panic "SimpleHead.TODO"
