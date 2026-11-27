{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Tx.IsTx.Core where

import Hydra.Prelude

import Data.Aeson (FromJSONKey, ToJSONKey)

-- | Types of transactions that can be used by the Head protocol. The associated
-- types and methods of this type class represent the whole interface of what
-- the Head protocol needs from a transaction. This ensure the off-chain
-- protocol stays fairly independent of a concrete transaction type.
class
  ( Eq tx
  , Show tx
  , Typeable tx
  , FromCBOR tx
  , ToCBOR tx
  , FromJSON tx
  , ToJSON tx
  , --
    Eq (TxIdType tx)
  , Ord (TxIdType tx)
  , Show (TxIdType tx)
  , Typeable (TxIdType tx)
  , FromJSON (TxIdType tx)
  , ToJSON (TxIdType tx)
  , FromCBOR (TxIdType tx)
  , ToCBOR (TxIdType tx)
  , FromJSONKey (TxIdType tx)
  , ToJSONKey (TxIdType tx)
  , --
    Eq (TxOutType tx)
  , Ord (TxOutType tx)
  , Show (TxOutType tx)
  , ToJSON (TxOutType tx)
  , FromJSON (TxOutType tx)
  , --
    Eq (UTxOType tx)
  , Show (UTxOType tx)
  , Monoid (UTxOType tx)
  , FromJSON (UTxOType tx)
  , ToJSON (UTxOType tx)
  , FromCBOR (UTxOType tx)
  , ToCBOR (UTxOType tx)
  ) =>
  IsTx tx
  where
  -- | Type which identifies a transaction
  type TxIdType tx

  -- | Type for individual transaction outputs.
  type TxOutType tx = out | out -> tx

  -- | Type for a set of unspent transaction outputs.
  type UTxOType tx = utxo | utxo -> tx

  -- | Type representing a value on the ledger.
  type ValueType tx

  -- XXX(SN): this name easily conflicts
  txId :: tx -> TxIdType tx

  -- XXX: Is this even used?
  balance :: UTxOType tx -> ValueType tx

  -- | Hash a utxo set to be able to sign (off-chain) and verify it (on-chain).
  hashUTxO :: UTxOType tx -> ByteString

  -- | Raw bytes identifying a transaction, as bound into snapshot signatures.
  -- Must match what the on-chain code reads out of a 'TxOutRef', see
  -- 'Hydra.Tx.Snapshot.getSignableRepresentation'.
  txIdBytes :: TxIdType tx -> ByteString

  txSpendingUTxO :: UTxOType tx -> tx

  -- | Get the UTxO produced by given transaction.
  utxoFromTx :: tx -> UTxOType tx

  -- | Get only the outputs in given UTxO.
  outputsOfUTxO :: UTxOType tx -> [TxOutType tx]

  -- | Return the left-hand side without the right-hand side.
  withoutUTxO :: UTxOType tx -> UTxOType tx -> UTxOType tx

  -- | Whether every input the transaction refers to is available, including
  -- reference and collateral inputs. This does not validate scripts or time.
  txInputsAvailable :: tx -> UTxOType tx -> Bool

  -- | Remove spending inputs that are present, without adding outputs. This
  -- also works when some inputs are absent, as when retaining a reservation.
  withoutTxInputs :: tx -> UTxOType tx -> UTxOType tx

  -- | Apply a transaction to a UTxO via pure UTxO arithmetic, returning the
  -- post-tx UTxO. Removes the tx's inputs from the given UTxO and adds the
  -- tx's outputs. Performs no ledger validation; the caller must ensure the
  -- tx is valid against the given UTxO.
  applyTxTo :: tx -> UTxOType tx -> UTxOType tx

  -- | Filter a UTxO set keeping only entries whose output value is in the given set.
  filterUTxOByOutputs :: UTxOType tx -> Set (TxOutType tx) -> UTxOType tx

  removeOneOutputFromUTxO :: TxOutType tx -> UTxOType tx -> UTxOType tx

  -- | Convert a TxOut to a ByteString element for the accumulator.
  -- This serializes the TxOut in the same way as the on-chain code does.
  utxoToElement :: TxOutType tx -> ByteString

-- | A snapshot's UTxO together with its pending commit and decommit, if any.
--
-- The single definition of that union. Snapshot signing, the accumulator built
-- from a snapshot and the membership proof a fanout carries all have to describe
-- the same element set — when two of them disagree a fanout fails on chain with
-- @FanoutUTxOHashMismatch@ — so they take the union from here rather than each
-- spelling it out.
combinedUTxO :: Monoid a => a -> Maybe a -> Maybe a -> a
combinedUTxO utxo utxoToCommit utxoToDecommit =
  utxo <> fold utxoToCommit <> fold utxoToDecommit

-- | The entries of the first set whose key also appears in the second set.
-- Subtracting what the second set lacks leaves exactly the shared keys; the
-- outputs are taken from the first set.
restrictedTo :: forall tx. IsTx tx => UTxOType tx -> UTxOType tx -> UTxOType tx
restrictedTo a b = a `withoutUTxO` (a `withoutUTxO` b)
