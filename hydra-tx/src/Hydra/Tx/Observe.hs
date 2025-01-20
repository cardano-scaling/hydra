{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Observe hydra transactions
module Hydra.Tx.Observe (
  module Hydra.Tx.Observe,
  module Hydra.Tx.Init,
  module Hydra.Tx.Abort,
  module Hydra.Tx.Commit,
  module Hydra.Tx.CollectCom,
  module Hydra.Tx.Decrement,
  module Hydra.Tx.Deposit,
  module Hydra.Tx.Increment,
  module Hydra.Tx.Recover,
  module Hydra.Tx.Close,
  module Hydra.Tx.Contest,
  module Hydra.Tx.Fanout,
) where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (toList)

import Hydra.Tx.Abort (AbortObservation (..), observeAbortTx)
import Hydra.Tx.Close (CloseObservation (..), observeCloseTx)
import Hydra.Tx.CollectCom (CollectComObservation (..), observeCollectComTx)
import Hydra.Tx.Commit (CommitObservation (..), observeCommitTx)
import Hydra.Tx.Contest (ContestObservation (..), observeContestTx)
import Hydra.Tx.Decrement (DecrementObservation (..), observeDecrementTx)
import Hydra.Tx.Deposit (DepositObservation (..), observeDepositTx)
import Hydra.Tx.Fanout (FanoutObservation (..), observeFanoutTx)
import Hydra.Tx.Increment (IncrementObservation (..), observeIncrementTx)
import Hydra.Tx.Init (InitObservation (..), NotAnInitReason (..), observeInitTx)
import Hydra.Tx.Recover (RecoverObservation (..), observeRecoverTx)

-- * Observe Hydra Head transactions

-- | Generalised type for arbitrary Head observations on-chain.
data HeadObservation
  = NoHeadTx
  | Init InitObservation
  | Abort AbortObservation
  | Commit CommitObservation
  | CollectCom CollectComObservation
  | Deposit DepositObservation
  | Recover RecoverObservation
  | Increment IncrementObservation
  | Decrement DecrementObservation
  | Close CloseObservation
  | Contest ContestObservation
  | Fanout FanoutObservation
  deriving stock (Eq, Show, Generic)

-- | Observe any Hydra head transaction.
observeHeadTx :: NetworkId -> UTxO -> Tx -> HeadObservation
observeHeadTx networkId utxo tx =
  fromMaybe NoHeadTx $
    either (const Nothing) (Just . Init) (observeInitTx tx)
      <|> Abort <$> observeAbortTx utxo tx
      <|> Commit <$> observeCommitTx networkId utxo tx
      <|> CollectCom <$> observeCollectComTx utxo tx
      <|> Deposit <$> observeDepositTx networkId tx
      <|> Recover <$> observeRecoverTx networkId utxo tx
      <|> Increment <$> observeIncrementTx utxo tx
      <|> Decrement <$> observeDecrementTx utxo tx
      <|> Close <$> observeCloseTx utxo tx
      <|> Contest <$> observeContestTx utxo tx
      <|> Fanout <$> observeFanoutTx utxo tx
