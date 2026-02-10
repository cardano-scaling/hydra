-- | A mock implementation of a ledger using very simple UTxO transactions.
--
-- These transactions have a very simplified representation of unspent
-- transaction outputs being just integers, but already have inputs and outputs.
-- Transactions are validated against the current state of the ledger, so that
-- one transaction could at some point be invalid, then becomes valid because
-- some inputs it consumes are now available.
--
-- NOTE: There is no notion of time in this ledger, so transactions validation
-- will never depend on the L1 slot.
module Hydra.Ledger.Simple where

import "hydra-prelude" Hydra.Prelude
import "aeson" Data.Aeson (
  object,
  withObject,
  (.:),
  (.=),
 )
import "containers" Data.Set qualified as Set
import "hydra-tx" Hydra.Chain.ChainState (ChainSlot, ChainStateType, IsChainState (..))
import "hydra-tx" Hydra.Tx (IsTx (..))
import "serialise" Codec.Serialise (serialise)

import Hydra.Ledger (Ledger (..), ValidationError (ValidationError))

-- * Simple transactions

type SimpleId = Integer

-- | Simple transaction.
-- A transaction is a 'SimpleId', a list of inputs and a list of outputs,
-- and it has no time validity.
data SimpleTx = SimpleTx
  { txSimpleId :: SimpleId
  , txInputs :: UTxOType SimpleTx
  , txOutputs :: UTxOType SimpleTx
  }
  deriving stock (Eq, Ord, Generic, Show)

instance ToJSON SimpleTx where
  toJSON tx =
    object
      [ "id" .= txId tx
      , "inputs" .= txInputs tx
      , "outputs" .= txOutputs tx
      ]

instance FromJSON SimpleTx where
  parseJSON = withObject "SimpleTx" $ \obj ->
    SimpleTx
      <$> (obj .: "id")
      <*> (obj .: "inputs")
      <*> (obj .: "outputs")

instance ToCBOR SimpleTx where
  toCBOR (SimpleTx txid inputs outputs) =
    toCBOR txid <> toCBOR inputs <> toCBOR outputs

instance FromCBOR SimpleTx where
  fromCBOR =
    SimpleTx
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

-- | A single output of a 'SimpleTx' having an integer identity and sole value.
newtype SimpleTxOut = SimpleTxOut {unSimpleTxOut :: Integer}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Num, ToJSON, FromJSON)

instance ToCBOR SimpleTxOut where
  toCBOR (SimpleTxOut inId) = toCBOR inId

instance FromCBOR SimpleTxOut where
  fromCBOR = SimpleTxOut <$> fromCBOR

instance IsTx SimpleTx where
  type TxIdType SimpleTx = SimpleId
  type TxOutType SimpleTx = SimpleTxOut
  type UTxOType SimpleTx = Set SimpleTxOut
  type ValueType SimpleTx = Int

  txId (SimpleTx tid _ _) = tid
  balance = Set.size
  hashUTxO = toStrict . foldMap (serialise . unSimpleTxOut)
  utxoFromTx = txOutputs
  outputsOfUTxO = toList
  withoutUTxO = Set.difference

  txSpendingUTxO utxo =
    SimpleTx
      { txSimpleId = 0
      , txInputs = utxo
      , txOutputs = mempty
      }

-- * Simple chain state

newtype SimpleChainState = SimpleChainState {slot :: ChainSlot}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Num)

instance IsChainState SimpleTx where
  type ChainPointType SimpleTx = ChainSlot
  type ChainStateType SimpleTx = SimpleChainState
  chainStatePoint = slot
  chainPointSlot = id

-- * A simple ledger

simpleLedger :: Ledger SimpleTx
simpleLedger =
  Ledger{applyTransactions}
 where
  -- NOTE: _slot is unused as SimpleTx transactions don't have a notion of time.
  applyTransactions :: Foldable t => p -> Set SimpleTxOut -> t SimpleTx -> Either (SimpleTx, ValidationError) (Set SimpleTxOut)
  applyTransactions _slot =
    foldlM $ \utxo tx@(SimpleTx _ ins outs) ->
      if ins `Set.isSubsetOf` utxo && utxo `Set.disjoint` outs
        then Right $ (utxo Set.\\ ins) `Set.union` outs
        else Left (tx, ValidationError "cannot apply transaction")

-- * Builders

utxoRef :: Integer -> UTxOType SimpleTx
utxoRef = Set.singleton . SimpleTxOut

utxoRefs :: [Integer] -> UTxOType SimpleTx
utxoRefs = Set.fromList . fmap SimpleTxOut

aValidTx :: Integer -> SimpleTx
aValidTx n = SimpleTx n mempty (utxoRef n)
